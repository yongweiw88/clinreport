#' Merge two SDTM Domain based on relationship specified in RELREC domain.
#'
#' Pass in a primary SDTM domain and FA domain to merge FA domain into primary domain.
#'
#' @param dsetin1 primary SDTM domain
#' @param dsetin2 second SDTM domain
#' @param dsetinrelrec RELREC domain
#' 
#' @return A data frame with all records and variables from DSETIN1 and extra variables from DSETIN2.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' df_addv <- ru_addrelrec(sdtmdata$dv(), sdtmdata$fa(), sdtmdata$relrec()) 
#'
#' @export
#'

ru_adgetadslvars <- function (
  dsetin,               # Input dataset name
  dsetinadsl =G_POPDATA, # Input subject level dataset name 
  adslvars  =c("STUDYID", "INVID", "SITEID", "USUBJID", "AGE", "AGEU", "SEX", "RACE"), # List of variables to be fetched from subject level dataset 
  dropexist =TRUE      # if ADSLVARS which are in DSETIN should be dropped before the merge. 
  ) {
  
  print(paste0("RU_ADGETADSLVARS: ", "Start of RU_ADGETADSLVARS")) 
  all_adsl_vars <- names(dsetinadsl)
  all_dsetin_vars <- names(dsetin)
  
  # keep only ADSLVARS in ADSL data set
  this.adsl_vars <- base::intersect(adslvars, all_adsl_vars)
  not_in_adsl <- base::setdiff(adslvars, this.adsl_vars)
  if (length(not_in_adsl) > 0) print(paste0("RTW", "ARNING: ", "ADSLVARS Variavbles which are not in input ADSL dataset are ",  not_in_adsl), collapse=" ")  
  
  this.byvars <- base::intersect(all_adsl_vars, c("STUDYID", "APERIOD", "APERIODC", "APHASE", "APHASEC", "USUBJID", "SUBJID"))
  this.byvars <- base::intersect(this.byvars, all_dsetin_vars)
  
  this.adsl_vars.1 <- base::setdiff(this.adsl_vars, this.byvars)
  common_vars <- base::intersect(this.adsl_vars.1, all_dsetin_vars)
  
  if (length(this.adsl_vars.1) > 0) {
    if (length(common_vars) <= 0) {
      this.adsl <- dplyr::select(dsetinadsl, all_of(c(this.byvars, this.adsl_vars.1)))
      this.dsetin <- dsetin
    } else if (dropexist) {
      this.adsl <- dplyr::select(dsetinadsl, all_of(c(this.byvars, this.adsl_vars.1)))
      this.dsetin <- dplyr::select(dsetin, -all_of(common_vars))    
    } else {
      this.adsl_vars <- base::setdiff(this.adslvars, common_vars)
      this.adsl <- dplyr::select(dsetinadsl, -all_of(this.adsl_vars))
      this.dsetin <- dsetin        
    }
    
    d.out <- base::merge(x = this.dsetin, y = this.adsl, by = c(this.byvars), all.x = TRUE, all.y=FALSE)
    d.out <- ru_labels(d.out, labels(dsetin))
    d.out <- ru_labels(d.out, labels(dsetinadsl[, c(c(this.byvars, this.adsl_vars.1))]))
  } else {
    d.out <- dsetin
  }
  print(paste0("RU_ADGETADSLVARS: ", "End of RU_ADGETADSLVARS"))
  return(d.out)
}


