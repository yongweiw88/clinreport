#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_ussias_csv2data.R
# Purpose: Read in US IAS HIV Resistence Score file and create
#          a reference dataset
# Date:    04/26/2025
#========================================================================= 

ru_usias_csv2data <- function(
  iasfile="C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/US-IAS-Resistance_2022.csv",
  version="2022"
  ) {
  
  df_ias <- read.csv(iasfile) 
  str_names <- toupper(names(df_ias))
  str_names <- stringr::str_replace_all(str_names, "\\.", "")
  names(df_ias) <- str_names
  
  df_ias <- df_ias %>% dplyr::mutate(VERSION=!! version, CRDTC=as.character(as.Date(now())))
  return(df_ias)
}


# df_ias <- ru_usias_csv2data (
#     iasfile="C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/US-IAS-Resistance_2022.csv",
#     version="2022"
# )
# 
# rfmtdata$write2xpt(df_ias, name="ias2024")
