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


## Commented out because it seems like a bad idea in retrospect
# ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
#                                common_col_selector)


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
                          community_sector_other = if_else(sum(c_across(any_of(matching_cols)),
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


##################################################
##################################################
## ---  Organisation Size  -------------------- ##
##################################################
##################################################

## The below commented code was used to uncover that the 2013 dataset varies substantially from datasets in the other years
# lapply(ACNC_Datasets_13to18,
#        function(x) {colnames(x)[which(str_detect(colnames(x),
#                                                  regex("gross_income", ignore_case = TRUE)))]})
# 
# stop("All code above here was run")

ACNC_Datasets_14to18 <- ACNC_Datasets_13to18[which(!str_detect(names(ACNC_Datasets_13to18), "13"))]

rm(ACNC_Datasets_13to18)

## cleaning the encoding for charity size

Charitysize_cleaner <- function(data){
        
        data <- mutate(data,
                       cleaned_charitysize = toupper(substring(charity_size,
                                                               1, 1)))
        
        return(data)
        
}

ACNC_Datasets_14to18 <- lapply(ACNC_Datasets_14to18,
                               Charitysize_cleaner)

Correct_selfreport_size <- function(data) {
        
        grossIncome_col <- colnames(data)[which(str_detect(colnames(data),
                                                           make_clean_names("total gross income")))]
        
        reportedSize_col <- colnames(data)[which(str_detect(colnames(data),
                                                            make_clean_names("cleaned_charitysize")))]
        
        data <- mutate(data,
                       correct_charitysize = case_when(get(grossIncome_col) <= 250000 &
                                                               get(reportedSize_col) == "S" ~ TRUE,
                                                       get(grossIncome_col) > 250000 & get(grossIncome_col) <= 1000000 &
                                                               get(reportedSize_col) == "M" ~ TRUE,
                                                       get(grossIncome_col) > 1000000 &
                                                               get(reportedSize_col) == "L" ~ TRUE),
                       VCOSS_charitysize = case_when(get(grossIncome_col) < 50000 ~ "Extra Small",
                                                     get(grossIncome_col) >= 50000 & get(grossIncome_col) < 250000 ~ "Small",
                                                     get(grossIncome_col) >= 250000 & get(grossIncome_col) < 1000000 ~ "Medium",
                                                     get(grossIncome_col) >= 1000000 & get(grossIncome_col) < 10000000 ~ "Large",
                                                     get(grossIncome_col) >= 10000000 & get(grossIncome_col) < 100000000 ~ "Extra Large",
                                                     get(grossIncome_col) >= 100000000 ~ "Extra Extra Large"))
        
        data[["correct_charitysize"]] <- replace_na(data[["correct_charitysize"]],
                                                    FALSE)
        
        return(data)
        
}

ACNC_Datasets_14to18 <- lapply(ACNC_Datasets_14to18,
                               Correct_selfreport_size)


ACNC_Datasets_14to18 <- lapply(ACNC_Datasets_14to18,
                               function(data) {
                                       filter(data,
                                              correct_charitysize == TRUE)
                               })

##################################################
##################################################
## ---  Income  ------------------------------- ##
##################################################
##################################################

## Removing charities with no income

ACNC_Datasets_14to18 <- lapply(ACNC_Datasets_14to18,
                               function(data) {
                                       filter(data,
                                              total_gross_income != 0)
                               })


## Removing charities with inaccurate income reporting
#  first bringing in previously-prepared VCOSS ACNC data to select columns

VCOSS_ACNC_2016_columns <- colnames(clean_names(read_excel("Datasets/VCOSS - 2016 ACNC Data.xlsx")))

VCOSS_ACNC_2016_columns <- str_remove_all(VCOSS_ACNC_2016_columns, "_2016")

VCOSS_ACNC_2016_columns <- c(VCOSS_ACNC_2016_columns,
                             "Year",
                             "Valid_ABN",
                             "community_sector_other",
                             "community_sector_main",
                             "vic_community_sector",
                             "cleaned_charitysize",
                             "correct_charitysize",
                             "VCOSS_charitysize") ## hard coded from created variables in code **above** only

VCOSS_ACNC_Datasets_14to18 <- lapply(ACNC_Datasets_14to18,
                                     function(data){
                                             select(data,
                                                    any_of(VCOSS_ACNC_2016_columns),
                                                    contains("government_grants"),
                                                    government_grants = contains("revenue_from_government"))
                                     })

VCOSSACNC_missingcols <- lapply(VCOSS_ACNC_Datasets_14to18,
                                function(data) {
                                        missing_cols <- VCOSS_ACNC_2016_columns[which(!(VCOSS_ACNC_2016_columns %in% colnames(data)))]
                                        
                                        return(missing_cols)
                                })

if(sum(sapply(VCOSSACNC_missingcols, length) >0) >0) {
        stop(paste("There are missing columns in the VCOSS-filtered ACNC Data.",
                   "Ctrl+F error code: `VCOSS_MISSINGCOLS_423` in Data_Cleaning.R script.",
                   sep = "\n"))
}

## Removing charities with inaccurate income data

Income_cols <- c("government_grants",
                 "donations_and_bequests",
                 "all_other_revenue",
                 "other_income") ## hard coded from VCOSS_ACNC_2016_columns

Inaccurate_income <- function(data) {
        
        data <- mutate(data,
                       reported_income = sum(c_across(all_of(Income_cols))),
                       income_variance = abs(reported_income - total_gross_income),
                       inaccurate_income_reported = case_when(cleaned_charitysize == "S" &
                                                                      income_variance > 25000 ~ "inaccurate",
                                                              cleaned_charitysize == "M" &
                                                                      income_variance > 100000 ~ "inaccurate",
                                                              cleaned_charitysize == "L" &
                                                                      income_variance > 1000000 ~ "inaccurate"))
        
        data$inaccurate_income_reported <- replace_na(data$inaccurate_income_reported,
                                                      "accurate")
        
        return(data)
}


VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     Inaccurate_income)

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             filter(data,
                                                    inaccurate_income_reported == "accurate")
                                     })



##################################################
##################################################
## ---  Expenditure  -------------------------- ##
##################################################
##################################################

expenses_cols <- c("employee_expenses",
                   "interest_expenses",
                   "all_other_expenses") ## hardcoded for same reasons as above

Inaccurate_expenses <- function(data) {
        
        data <- mutate(data,
                       reported_expenses = sum(c_across(all_of(expenses_cols))),
                       expenses_variance = abs(reported_expenses - total_expenses),
                       inaccurate_expenses_reported = case_when(cleaned_charitysize == "S" &
                                                                        expenses_variance > 25000 ~ "inaccurate",
                                                                cleaned_charitysize == "M" &
                                                                        expenses_variance > 100000 ~ "inaccurate",
                                                                cleaned_charitysize == "L" &
                                                                        expenses_variance > 1000000 ~ "inaccurate"))
        
        data$inaccurate_expenses_reported <- replace_na(data$inaccurate_expenses_reported,
                                                        "accurate")
        
        return(data)
        
}

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     Inaccurate_expenses)

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             filter(data,
                                                    inaccurate_expenses_reported == "accurate")
                                     })

## Coercing employee columns to numeric

employee_cols <- c("staff_full_time",
                   "staff_part_time",
                   "staff_casual",
                   "staff_volunteers")

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
       function(data) {
               
               data <- mutate(data,
                              across(all_of(employee_cols),
                                     as.numeric))
               
       })

Employee_expenses <- function(data) {
        
        data <- mutate(data,
                       total_employees = sum(c_across(employee_cols), na.rm = TRUE),
                       employeeexpenses_per_employee = (employee_expenses / total_employees),
                       excessive_expenses = if_else(total_employees > 0 & 
                                                            employeeexpenses_per_employee > 300000,
                                                    true = "excessive", false = "not excessive"))
        
        return(data)
        
}

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     Employee_expenses)

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             
                                             filter(data,
                                                    excessive_expenses == "not excessive")
                                     })

##################################################
##################################################
## ---  Ratios  ------------------------------- ##
##################################################
##################################################

## Negative values for fields that should have non-negative values

nonnegative_cols <- c("staff_full_time",
                      "staff_part_time",
                      "staff_casual",
                      "staff_volunteers",
                      "government_grants",
                      "donations_and_bequests",
                      "all_other_revenue",
                      "other_income",
                      "employee_expenses",
                      "interest_expenses",
                      "grants_and_donations_made_for_use_in_australia",
                      "grants_and_donations_made_for_use_outside_australia",
                      "all_other_expenses")

## coercing nonnegative_cols into numeric

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             mutate(data,
                                                    across(nonnegative_cols,
                                                           as.numeric))
                                     })

Negative_values_checker <- function(data) {
        
        data <- mutate(data,
                       invalid_negative_values = sum(c_across(all_of(nonnegative_cols)) < 0,
                                                     na.rm = TRUE))
        
}

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     Negative_values_checker)

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             filter(data,
                                                    invalid_negative_values == 0)
                                     })

## Imbalanced reporting, eg govt grants > total income

Imbalanced_income <- function(data) {
        
        data <- mutate(data,
                       total_grants_type = sum(c_across(c(government_grants, donations_and_bequests)),
                                               na.rm = TRUE),
                       total_grants_use = sum(c_across(c(grants_and_donations_made_for_use_outside_australia,
                                                         grants_and_donations_made_for_use_in_australia)),
                                              na.rm = TRUE),
                       grants_income_prop = total_grants_type / total_gross_income,
                       grants_use_prop = total_grants_use / total_gross_income,
                       imbalanced_grants = case_when(grants_income_prop > 1.05 ~ "imbalanced",
                                                     grants_use_prop > 1.05 ~ "imbalanced"))
        
        data$imbalanced_grants <- replace_na(data$imbalanced_grants,
                                             "balanced")
        
        return(data)
        
}

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     Imbalanced_income)

VCOSS_ACNC_Datasets_14to18 <- lapply(VCOSS_ACNC_Datasets_14to18,
                                     function(data) {
                                             filter(data,
                                                    imbalanced_grants == "balanced")
                                     })

