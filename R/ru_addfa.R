#' Merge SDTM FA Domain in if relation is not availabele in RELREC domain.
#'
#' Pass in a primary SDTM domain and FA domain to merge FA domain into primary domain.
#'
#' @param dsetin primary SDTM domain
#' @param dsetinfa SDTM FA domain.
#' @param objvar Column name in primary SDTM domain which will be used to match with FAOBJ in FA domain. If NULL, one of xxDECOD/xxTRT/xxTERM will be used.
#' @param idvars extra varaibles (e.g. time point variables like VISITNUM/VISIT), other than STUDYID/USUBJID, to be used when merging two domains together.  
#' @param fatransposeyn logic values to specify if FA domain FATESTCD/FATEST be normalized  to columns 
#' 
#' @return A data frame with all records and variables from DSETIN and extra variables from FA.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' df_ae <- ru_addfa(sdtmdata$ae(), sdtmdata$faae(), objvars="AEDECOD", fatransposeyn = FALSE, idvars = c("visitnum", "visit")) 
#'
#' @export
#'

ru_addfa <- function(dsetin, dsetinfa, objvar = NULL, fatransposeyn = FALSE, idvars = NULL) {  
  
  print(paste0("RU_ADDFA: ", "Start of RU_ADDFA"))
  
  #
  # Parameter Validation
  #
  bln_CheckParam <- FALSE 

  if (bln_CheckParam) return()

  # Identify matching variable names
  idvar2 <- NULL
  idvar1 <- grep("SPID$", names(dsetin), value = TRUE, ignore.case = TRUE)
  if (length(idvar1) < 1) {
    idvar2 <- grep("REFID$", names(dsetin), value = TRUE, ignore.case = TRUE)
  }
  objvar_default <- names(dsetin)[grepl("DECOD$", toupper(names(dsetin)))]
  if (length(objvar_default) < 1) objvar_default <- names(dsetin)[grepl("TRT$", toupper(names(dsetin)))]
  if (length(objvar_default) < 1) objvar_default <- names(dsetin)[grepl("TERM$", toupper(names(dsetin)))]
  
  if (!is.null(objvar)) {
    objvar_final <- objvar
  } else if (length(objvar_default) > 0) {
    objvar_final <- objvar_default[1]
  } else {
    warning("Input dataset is not an event or intervention domain. Returning input dataset.")
    return(dsetin)
  }
  
  # Add sequence variable
  dsetin <- dsetin %>% mutate(seq1__ = row_number())

  rename_vars <- stats::setNames("FAOBJ", objvar_final)
  if (length(idvar1) > 0 && "FASPID" %in% names(dsetinfa)) {
    rename_vars[idvar1] <- "FASPID"
  } else if (length(idvar2) > 0 && "FAREFID" %in% names(dsetinfa)) {
    rename_vars[idvar2] <- "FAREFID"
  }

  # Rename FA dataset columns
  dsetinfa_1 <- dsetinfa %>% dplyr::rename(!!!rename_vars)
  
  if (length(idvar1) == 0) idvar1 <- NULL
  if (length(idvar2) == 0) idvar2 <- NULL
  
  result_vars <- base::intersect(names(dsetinfa), c("FAORRES", "FAORRESU", "FASTRESN", "FASTRESC", "FASTRESU"))
  favars <- base::setdiff(names(dsetinfa), c(result_vars, "USUBJID", "FASEQ", "FASPID", "FAREFID", "FATESTCD", "FATEST",
                      "SUBJID", "STUDYID", "DOMAIN", "EPOCH", "VISITNUM", "VISIT", "FAOBJ", "FACAT", "FASCAT"))
  
  # Transpose FA dataset if required
  str_this_byvars <- base::intersect(names(dsetinfa), c("STUDYID", "USUBJID", idvars, objvar_final, idvar1, idvar2))
  str_this_byvars <- base::intersect(names(dsetin), str_thisbyvars)  
    
  if (fatransposeyn) {
    str_this_prefixes <- NULL
    str_this_suffixes <- NULL
    for (i in 1:length(result_vars)) {
      if (result_vars[i] == "FAORRES") {str_this_prefix <- "OR"; str_this_suffix <- ""}
      else if (result_vars[i] == "FAORRESU") {str_this_prefix <- "OR"; str_this_suffix <- "U"}
      else if (result_vars[i] == "FASTRESC") {str_this_prefix <- ""; str_this_suffix <- ""}
      else if (result_vars[i] == "FASTRESN") {str_this_prefix <- ""; str_this_suffix <- "N"}
      else if (result_vars[i] == "FASTRESU") {str_this_prefix <- ""; str_this_suffix <- "U"}
      str_this_prefixes <- c(str_this_prefixes, str_this_prefix)
      str_this_suffixes <- c(str_this_suffixes, str_this_suffix)
    }
    dsetinfa_2 <- dplyr::mutate(dsetinfa_1, seq__ = dplyr::row_number())
    dsetinfa_3 <- ru_denorm(dsetinfa_2, varstodenorm=result_vars, groupbyvars=c(str_this_byvars, favars, "seq__"), acrossvar="FATESTCD", 
                           acrossvarlabel="FATEST", acrossvarprefix=str_this_prefixes, acrossvarsuffix = str_this_suffixes) %>%
                  dplyr::select(-seq__)
  } else {
    dsetinfa_3 <- dsetinfa_1 %>% dplyr::select(all_of(c(str_this_byvars, favars, result_vars, "FATESTCD", "FATEST")))
  }
  
  # Merge datasets
  merged_data <- dplyr::left_join(dsetin, dsetinfa_3, by = str_this_byvars)
  
  # Warning for unmatched records
  unmatched <- merged_data %>% dplyr::filter(is.na(seq1__))
  if (nrow(unmatched) > 0) {
    warning("Records in FA data are not in the input dataset for some subjects.")
  }
  
  # Drop sequence variable
  df_dsetout <- merged_data %>% dplyr::select(-seq1__)
    
  print(paste0("RU_ADDFA: ", "End of RU_ADDFA"))
  return(df_dsetout)
}

# Example usage
# aewfa <- ru_addfa(ae, faae, NULL, NULL, TRUE)
