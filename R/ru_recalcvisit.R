#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_calculatedvisit.R
# Purpose: Recalculate visit based on a specified date.
# Date:    02/26/2025
#========================================================================= 

ru_recalcvisit <- function (
  dsetin, 
  refdat, 
  reftim=NULL, 
  dsetinvisit, 
  visitdat, 
  visittim=NULL, 
  timepointvars=c("APERIOD", "APERIODC", "APHASE", "APHASEC", "VISITNUM", "VISIT", "AVISITN", "AVISIT")
  ) {
  
  # dsetin=df_sv_unsch_1
  # refdat="SVSTDT"
  # reftim=NULL
  # dsetinvisit=df_sv_all_1 
  # visitdat="NEWSVSTDT"
  # visittim=NULL
  # timepointvars=c("APERIOD", "APERIODC", "APHASE", "APHASEC", "VISITNUM", "VISIT", "AVISITN", "AVISIT")
  
  print(paste0("RU_RECALCVISIT: ", "Start of RU_RECALCVISIT"))
  
  str_byvars <- c("STUDYID", "USUBJID", "SUBJID")
  str_byvars <- intersect(str_byvars, names(dsetin))
  str_byvars <- intersect(str_byvars, names(dsetinvisit))
  
  timepointvars <- intersect(timepointvars, names(dsetinvisit))
  bln_CheckParam <- FALSE 
  
  if  (length(timepointvars) < 1) {
    message(paste0("RTE", "RROR: RU_RECALCVISIT: None TIMEPOINTVARS in DSETINVISIT is given"))
    bln_CheckParam <- TRUE    
  }  
  
  if  (length(str_byvars) < 1) {
    message(paste0("RTE", "RROR: RU_RECALCVISIT: Can't find USUBJID and/or SUBJID in both DSETIN and DSETINVISIT"))
    bln_CheckParam <- TRUE    
  }  
  
  if (bln_CheckParam) return()

  df_dsetin <- dsetin %>% 
    dplyr::mutate(
      seqrcv1__=row_number(),
      REFDAT__=as.Date(!! rlang::sym(refdat))
    )
  
  if (! is.null(reftim)) {
    df_dsetin <- df_dsetin %>% 
      dplyr::mutate(
        REFTIM__=!! rlang::sym(reftim)
      )    
  } else {
    df_dsetin <- df_dsetin %>% 
      dplyr::mutate(
        REFTIM__=NA_real_
      )       
  }
  
  df_visitdm <- dsetinvisit %>%
    dplyr::select(all_of(c(str_byvars, timepointvars, visitdat, visittim))) %>%
    dplyr::filter(! is.na(!! rlang::sym(visitdat))) %>% 
    dplyr::group_by(across(all_of(c(str_byvars, timepointvars)))) %>%
    dplyr::arrange(across(all_of(c(visitdat, visittim))), .by_group=TRUE) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      VISITDAT__=as.Date(!! rlang::sym(visitdat))
    )
  
  if (! is.null(visittim)) {
    df_visitdm <- df_visitdm %>%
      dplyr::mutate(
        VISITTIM__=!! rlang::sym(visittim)
      )
  } else {
    df_visitdm <- df_visitdm %>%
      dplyr::mutate(
        VISITTIM__=!! NA_real_
      )
  }
  
  df_visitdm <- df_visitdm %>%
    dplyr::group_by(across(all_of(c(str_byvars)))) %>%
    dplyr::arrange(desc(VISITDAT__), desc(VISITTIM__), .by_group=TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(all_of(c(str_byvars)))) %>%
    dplyr::mutate(
      VENDAT__=dplyr::lag(VISITDAT__),
      VENTIM__=dplyr::lag(VISITTIM__)
    ) %>%
    dplyr::ungroup()
    
  df_visit <- df_visitdm %>%
    dplyr::select(all_of(c(timepointvars))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(seqrcv2__=row_number())

  
  df_subset <- NULL
  
  for (i in 1:nrow(df_visit)) {
    if (length(timepointvars) > 0) {
      df_thisvisit <- df_visit %>% dplyr::filter(seqrcv2__== !! i) 
      df_thisvisitdm <- dplyr::inner_join(df_visitdm, df_thisvisit, by=c(timepointvars)) 
    } else df_thisvisitdm <- df_visitdm

    df_subset_1 <- dplyr::inner_join(x=df_dsetin, y=df_thisvisitdm, by=str_byvars) %>%
      dplyr::filter((REFDAT__ > VISITDAT__ | (REFDAT__ == VISITDAT__ & (REFTIM__ >= VISITTIM__ | is.na(VISITTIM__) | is.na(REFTIM__)))) & 
                    (REFDAT__ < VENDAT__ | (REFDAT__ == VENDAT__ & REFTIM__ < VENTIM__ & ! is.na(VENTIM__) & ! is.na(REFTIM__)) | is.na(VENDAT__))
                   )

    if (nrow(df_subset_1) > 0) {
      if (is.null(df_subset)) df_subset <- df_subset_1
      else df_subset <- dplyr::bind_rows(df_subset, df_subset_1)
    }
  }
  
  if (is.null(df_subset)) {
    df_out <- df_dsetin %>% dplyr::select(-all_of(c("seqrcv1__", "seqrcv2__", "REFDAT__", "REFTIM__", "VENDAT__", "VENTIM__", "VISITDAT__", "VISITTIM__", visitdat, visittim)))
  } else {
    df_subset2 <- dplyr::anti_join(df_dsetin, df_subset %>% dplyr::select(seqrcv1__) %>% dplyr::distinct(), by="seqrcv1__")
    df_out <- repfun::ru_setdata(df_subset2, df_subset) %>% #dplyr::bind_rows(df_subset2, df_subset) %>% 
      dplyr::group_by(across(all_of(c("seqrcv1__")))) %>%
      dplyr::arrange(VISITDAT__, VISITTIM__, .by_group = TRUE ) %>%
      dplyr::slice_head(n=1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-all_of(c("seqrcv1__", "seqrcv2__", "REFDAT__", "REFTIM__", "VENDAT__", "VENTIM__", "VISITDAT__", "VISITTIM__", visitdat, visittim)))
  }

  print(paste0("RU_RECALCVISIT: ", "End of RU_RECALCVISIT"))
  return(as.data.frame(df_out))
}
