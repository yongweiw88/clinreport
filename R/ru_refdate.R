#' To Derive a reference date
#'
#' Pass in a data with related column names and return a data with derived reference start/end date (C)  
#'
#' @param dsetin Input reference dataset with referece start and end date columns
#' @param stdtc Start date (C) column name in DSETIN
#' @param endtc End date (C) column name in DSETIN, can be NULL
#' @param refsdtcvar Derived reference start date (C) column name. It is first STDTC in a period/phase
#' @param refedtcvar Derived reference end date (C) column name. It is last of stdtc/endtc in a period/phase
#' 
#' @return an ADaM data with US-IAS Resistance Score.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'

ru_refdate <- function (
  dsetin,        
  stdtc,         
  endtc=NULL,      
  refsdtcvar=NULL, 
  refedtcvar=NULL  
  ) {

  print(paste0("RU_REFDATE: ", "Start of RU_RUREFDATE"))

  this_invars <- base::colnames(dsetin)
  this_byvars <- base::intersect(this_invars, c("STUDYID", "APHASE", "APHASEC", "APERIOD", "APERIODC", "USUBJID", "SUBJID"))
  print(this_byvars)
  
  if (is.null(endtc)) {
    if (is.null(refsdtcvar)) {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(!! refedtcvar := suppressWarnings(max(!!! rlang::syms(stdtc), na.rm = TRUE))) %>%
        dplyr::ungroup()
    } else if (is.null(refedtcvar)) {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(!! refsdtcvar := suppressWarnings(min(!!! rlang::syms(stdtc), na.rm=TRUE))) %>%
        dplyr::ungroup()      
    } else {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(!! refsdtcvar := suppressWarnings(min(!!! rlang::syms(stdtc), na.rm=TRUE)), 
                         !! refedtcvar := suppressWarnings(max(!!! rlang::syms(stdtc), na.rm=TRUE))) %>%
        dplyr::ungroup()      
    }
  } else {
    if (is.null(refsdtcvar)) {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(MAXEDTC_ = suppressWarnings(max(!!! rlang::syms(endtc), na.rm = TRUE)), 
                         MAXSDTC_ = suppressWarnings(max(!!! rlang::syms(stdtc), na.rm = TRUE))) %>% 
        dplyr::mutate(!! refedtcvar := ifelse(MAXSDTC_ > MAXEDTC_, MAXSDTC_, MAXEDTC_)) %>% dplyr::ungroup() %>% dplyr::select(-MAXSDTC_, -MAXEDTC_) 
    } else if (is.null(refedtcvar)) {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(!! refsdtcvar := suppressWarnings(min(!!! rlang::syms(stdtc), na.rm=TRUE))) %>%
        dplyr::ungroup()       
    } else {
      df_out <- dsetin %>% dplyr::group_by(!!! rlang::syms(this_byvars)) %>% 
        dplyr::summarize(MAXEDTC_ = suppressWarnings(max(!!! rlang::syms(endtc), na.rm = TRUE)), 
                         MAXSDTC_ = suppressWarnings(max(!!! rlang::syms(stdtc), na.rm = TRUE)), 
                         !! refsdtcvar := suppressWarnings(min(!!! rlang::syms(stdtc), na.rm = TRUE))) %>% 
        dplyr::mutate(!! refedtcvar := ifelse(MAXSDTC_ > MAXEDTC_, MAXSDTC_, MAXEDTC_)) %>% dplyr::ungroup() %>% dplyr::select(-MAXSDTC_, -MAXEDTC_)       
    }
  }
  
  return(df_out)
}

