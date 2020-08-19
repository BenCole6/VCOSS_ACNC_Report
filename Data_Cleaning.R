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
                                common_cols)
        
        return(selected_data)
}

ACNC_Datasets_13to18 <- lapply(ACNC_Datasets_13to18,
                               common_col_selector)

