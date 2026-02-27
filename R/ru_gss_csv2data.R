#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_gss_csv2data.R
# Purpose: Read in Standford HIV Resistence Score files and create
#          a reference dataset
# Date:    04/26/2025
#========================================================================= 

ru_gss_csv2data <- function(
  gssfiles=c(
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/pi_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/pi_c_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nrti_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nrti_c_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nnrti_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nnrti_c_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/insti_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/insti_c_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/cai_2024.csv",
    "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/cai_c_2024.csv"),
  classes = c("PI", "PI", "NRTI", "NRTI", "NNRTI", "NNRTI", "INSTI", "INSTI", "FI", "FI"),
  rulecolnames = c("RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE")
  ) {
  
  rulecolnames <- toupper(rulecolnames)
  df_gss <- NULL
  for (i in 1:length(gssfiles)) {
    df_gss_csv <- read.csv(gssfiles[i]) 
    names(df_gss_csv) <- toupper(names(df_gss_csv))
    str_drugcodes <- setdiff(names(df_gss_csv), rulecolnames[i]) 
    df_gss_csv <- df_gss_csv %>%
      tidyr::pivot_longer(cols=all_of(c(str_drugcodes)), names_to="DRUGCODE", values_to="GSS", values_drop_na=TRUE) %>%
      dplyr::mutate(CLASS=!! classes[i]) %>% 
      dplyr::rename(GENOTYPE = !! rulecolnames[i] )
    if (is.null(df_gss)) df_gss <- df_gss_csv else 
      df_gss <- rbind(df_gss, df_gss_csv)
  }
  df_gss <- df_gss %>% dplyr::mutate(CRDTC=as.character(as.Date(now())))
  df_gss[["DRUGCODE"]] <- str_replace(df_gss[["DRUGCODE"]], "\\.", "")
  return(df_gss)
}

# df_gss <- ru_gss_csv2data(
#     gssfiles=c(
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/pi_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/pi_c_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nrti_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nrti_c_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nnrti_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/nnrti_c_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/insti_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/insti_c_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/cai_2024.csv",
#       "C:/Users/yw62951/OneDrive - GSK/Documents/R/SAS Data/refdata/cai_c_2024.csv"),
#     classes = c("PI", "PI", "NRTI", "NRTI", "NNRTI", "NNRTI", "INSTI", "INSTI", "FI", "FI"),
#     rulecolnames = c("RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE", "RULE", "COMBINATION.RULE")
# )
# 
# rfmtdata$write2xpt(df_gss, name="gss2024")
