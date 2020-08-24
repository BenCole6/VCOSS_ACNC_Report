# packages

library(pacman)

p_load(tidyverse,
       rvest,
       lubridate,
       ggplot2,
       plotly,
       readxl,
       janitor)

##################################################
##################################################
## ---  Importing Data  ----------------------- ##
##################################################
##################################################

ACNC_Yearly_Datasets_fp <- file.path(getwd(), "Datasets", "Individual_charities")

ACNC_Yearly_Datasets <- list.files(ACNC_Yearly_Datasets_fp)

ACNC_Datasets_13to18 <- sapply(X = ACNC_Yearly_Datasets,
                               FUN = function(year){
                                 read_excel(path = file.path(ACNC_Yearly_Datasets_fp, year))
                               })

##################################################
##################################################
## ---  Cleaning column names  ---------------- ##
##################################################
##################################################

names(ACNC_Datasets_13to18) <- sapply(names(ACNC_Datasets_13to18),
                                      function(year){str_remove_all(year, ".xlsx")})

## Standardises the colnames of each df
ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               clean_names)

## Creates a new column in each data frame and assigns the year to it based on
## the name of the dataframe within the list
for(year in 1:length(objects(ACNC_Datasets_13to18))) {
        
        object_name <- objects(ACNC_Datasets_13to18[year])
        
        year_val <- paste0("20", str_extract(object_name, "\\d\\d"))
        
        ACNC_Datasets_13to18[[year]]$Year <- year_val
        
}

ACNC_colnames <- lapply(ACNC_Datasets_13to18, colnames)

colnames_df <- NULL

# the below line and for loop are required because different years have different numbers of columns
maxlen_colnames <- max(sapply(ACNC_colnames, length)) 

for(year in 1:length(objects(ACNC_colnames))) {
        
        object_name <- objects(ACNC_colnames[year])
        
        year_colnameslen <- length(ACNC_colnames[[year]])
        
        if(year_colnameslen < maxlen_colnames) {
                
                ACNC_colnames[[year]][(year_colnameslen+1):(maxlen_colnames)] <- NA
                
        }
        
        colnames_df <- cbind(colnames_df,
                             ACNC_colnames[[year]])
        
}

colnames(colnames_df) <- objects(ACNC_colnames)

#finds common column names
for(col_index in 1:ncol(colnames_df)) {
        
        if(col_index == 1) {
                
                common_cols <- colnames_df[,1]
                
        } else{
                
                common_cols <- intersect(common_cols,
                                         colnames_df[,col_index])
                
        }
        
}

# function for below lapply
common_col_selector <- function(data) {
        
        selected_data <- select(data,
                                any_of(common_cols))
        
        return(selected_data)
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               common_col_selector)


##################################################
##################################################
## ---  Recoding Booleans  -------------------- ##
##################################################
##################################################

# ACNC data records booleans as y or no

yn_boolfix <- function(dataframe){
        
        for(col_name in colnames(dataframe)) {
                
                if(sum(c("n", "y") %in% tolower(unique(dataframe[[col_name]]))) == 2) {
                        
                        dataframe[[col_name]] <- recode(dataframe[[col_name]],
                                                        "n" = FALSE, "N" = FALSE,
                                                        "y" = TRUE, "Y" = TRUE)
                        
                        
                        
                } else if(sum(c(NA, "y") %in% tolower(unique(dataframe[[col_name]]))) == 2) {
                        # replace_na needed because recode(dataframe[[col_name]], NA = FALSE) does not work like that
                        
                        dataframe[[col_name]] <- recode(dataframe[[col_name]],
                                                        "y" = TRUE, "Y" = TRUE)
                        
                        dataframe[[col_name]] <- replace_na(dataframe[[col_name]],
                                                            FALSE)
                        
                }
        }
        
        return(dataframe)
        
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               yn_boolfix)


##################################################
##################################################
## ---  Removing Invalid ABNs  ---------------- ##
##################################################
##################################################

# Removing Invalid ABNs

## Reference Keybreaker file

ABN_Keybreaker <- read_excel("ABN Keybreaker.xlsx")

## Function for checking ABNs

ABN_Checker <- function(ABN_No) {
        
        sumproduct <- c()
        
        for(position in 1:nchar(ABN_No)) {
                
                number <- as.numeric(substr(ABN_No, position, position))  
                
                if(position == 1) {
                        
                        number <- as.numeric(number - 1)
                        
                }
                
                product <- (number * ABN_Keybreaker$Weighting[position])
                
                sumproduct <- sum(sumproduct, product)
                
        }
        
        if(sumproduct %% 89 == 0) {
                
                Check <- "Valid ABN"
                
        } else {
                
                Check <- "Invalid ABN"
                
        }
        
        return(Check)
        
}

ABN_Validator <- function(dataframe, abn_colname = "abn") {
        
        Valid_ABN <- sapply(dataframe[[abn_colname]], ABN_Checker)
        
        dataframe <- cbind(dataframe, Valid_ABN = Valid_ABN)
        
        return(dataframe)
        
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               ABN_Validator)

##################################################
##################################################
## ---  Filtering on Main Activity  ----------- ##
##################################################
##################################################


main_act_present <- function(dataframe) {
        
        regex("main_activity", ignore_case = TRUE) %in%
                make_clean_names(colnames(dataframe)) # complement of clean_names but for a vector
        
}

mainactivity_check <- sapply(ACNC_Datasets_13to18,
                             main_act_present)

if(FALSE %in% mainactivity_check) {
        
        stop(paste("Main activity is missing from",
                   paste(names(mainactivity_check[which(mainactivity_check == FALSE)]),
                         collapse = ", ")))
        
}

main_activity_cleaner <- function(dataframe) {
        
        dataframe[["main_activity"]] <- str_remove_all(dataframe[["main_activity"]],
                                                                  "and")
        
        dataframe[["main_activity"]] <- str_replace_all(dataframe[["main_activity"]],
                                                       "\\,|\\.",
                                                       " ")
        
        dataframe[["main_activity"]] <- str_squish(dataframe[["main_activity"]])
        
        return(dataframe)
        
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               main_activity_cleaner)

SocServ_activities <- c("aged care",
                        "civic advocacy",
                        "economic social community",
                        "emergency relief",
                        "employment training", "training",
                        "housing activities",
                        "income support maintenance", "income maintenance",
                        "international activities",
                        "law legal services", "law services", "legal services",
                        "mental health crisis intervention", "mental health", "crisis intervention",
                        "other education",
                        "other health service delivery", "health service delivery",
                        "social service", "social services")

Community_sector_colnames <- make_clean_names(SocServ_activities) 

Community_sector_other_varcreator <- function(dataset) {
        
        temp_df <- select(dataset, any_of(Community_sector_colnames))
        
        matching_cols <- colnames(temp_df)[which(colnames(temp_df) %in% Community_sector_colnames)]
        
        temp_df <- mutate(rowwise(dataset),
                          community_sector_other = if_else(sum(c_across(matching_cols),
                                                               na.rm = TRUE)>0,
                                                           true = TRUE, false = FALSE))
        
        return(temp_df)
        
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               Community_sector_other_varcreator)

Community_sector_varcreator <- function(dataframe) {
        
        dataframe <- mutate(dataframe,
                            community_sector_main = if_else(str_detect(main_activity,
                                                                       regex(paste0(SocServ_activities,
                                                                                    collapse = "|"),
                                                                             ignore_case = TRUE)),
                                                            true = TRUE, false = FALSE),
                            vic_community_sector = if_else((community_sector_main == TRUE |
                                                             community_sector_other == TRUE) &
                                                             (operates_in_vic == TRUE &
                                                             str_detect(state,
                                                                        regex("vic|victoria",
                                                                              ignore_case = TRUE))),
                                                           true = TRUE, false = FALSE))
        
        return(dataframe)
        
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               Community_sector_varcreator)

Vic_ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                                   function(x) {filter(x, vic_community_sector == TRUE)})


