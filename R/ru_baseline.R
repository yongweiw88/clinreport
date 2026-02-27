#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_baseline.R
# Purpose: derive baseline for finding domains
# Date:    02/26/2025
#========================================================================= 

ru_baseline <- function(
  dsetin               ,          # Input dataset name 
  rederivebaselineyn =FALSE,      # Flag to indicate if baseline is to be re-derived, otherwise use SDTM baseline flag 
  baselineoption     = c("date", "time", "relday", "visit", "tpt", "visittpt"),      # Calculation of baseline option: date, time, relday, visit, tpt or visittpt 
  reldays            = NULL,      # Number of days prior to start of study medication 
  startvisnum        = NULL,      # VISITNUM and/or ATPTN value for start of baseline range 
  endvisnum          = NULL,      # VISITNUM and/or ATPTN value for end of baseline range 
  baselinetype       = c("last", "first", "mean", "median"),      # How to calculate baseline for multiple baseline records: first, last, mean or median 
  byvars             = c("PARAMCD", "PARAM", "STUDYID", "USUBJID")
) {
  # dsetin <- df_lastdset
  # rederivebaselineyn = TRUE
  # baselineoption     = c("date", "time", "relday", "visit", "tpt", "visittpt")
  # reldays            = NULL
  # startvisnum        = NULL
  # endvisnum          = NULL
  # baselinetype       = c("last", "mean", "last", "first","median")
  # byvars             = c("PARAMCD", "PARAM", "STUDYID", "USUBJID")

  newvarlabels=list("BASE"="", "BASEC"="", "AVAL"="", "AVALC"="", "ABLFL"="", "BASETYP"="", 
                    "CHG"="", "PCHG"="", "A2INDCD"="", "A2IND"="")
    
  #
  # Parameter Validation
  #
  bln_CheckParam <- FALSE 
  if  (is.null(dsetin)) {
    message(paste0("RTE", "RROR: RU_BASELINE: DSETIN is null while it is required"))
    bln_CheckParam <- TRUE    
  }  
  if (! (tolower(baselinetype[1]) %in% c("last", "first", "mean", "median"))) {
    message(paste0("RTE", "RROR: RU_BASELINE: Parameter BASELINETYPE should be one of LAST, FIRST, MEAN, MEDIAN"))
    bln_CheckParam <- TRUE    
  }
  if (tolower(baselineoption[1]) %in% c("visit", "tpt", "visittpt")) {
    if (is.null(startvisnum)) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter STARTVISNUM is null while it is requred wthen BASELINEOPTION is one of VISIT, TPT, VISITTPT"))
      bln_CheckParam <- TRUE
    }
    if (is.null(endvisnum)) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter ENDVISNUM is null while it is requred wthen BASELINEOPTION is one of VISIT, TPT, VISITTPT"))
      bln_CheckParam <- TRUE
    }
  }    
  if  (tolower(baselineoption[1]) == "visittpt") {
    if (length(startvisnum) != 2) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter STARTVISNUM should contains two numeric values wthen BASELINEOPTION is VISITTPT"))
      bln_CheckParam <- TRUE
    }
    if (length(endvisnum) != 2) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter ENDVISNUM should contains two numeric values wthen BASELINEOPTION is VISITTPT"))
      bln_CheckParam <- TRUE
    }
  }
  if  (tolower(baselineoption[1]) == "relday") {
    if (is.null(reldays)) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter RELDAYS is null. It is required wthen BASELINEOPTION is RELDAY"))
      bln_CheckParam <- TRUE
    }
    if (length(endvisnum) != 2) {
      message(paste0("RTE", "RROR: RU_BASELINE: Parameter ENDVISNUM should contains two numeric values wthen BASELINEOPTION is VISITTPT"))
      bln_CheckParam <- TRUE
    }
  }
  if  (typeof(rederivebaselineyn) != "logical") {
    message(paste0("RTE", "RROR: RU_BASELINE: Value of REDERIVEBASELINEYS should be either TRUE or FALSE"))
    bln_CheckParam <- TRUE    
  }    
  
  if (bln_CheckParam) {
    stop("ru_baseline: stop with error(s)")
    return(invisible())
  }
  
  # if (tolower(baselineoption[1]) %in% c("date", "time", "relday") & is.null(dsetinadsl)) {
  #   message(paste0("RTE", "RROR: RU_BASELINE: Parameter DSETINADSL is null while it is requred wthen BASELINEOPTION is one of DATE, TIME, RELDAY"))
  #   bln_CheckParam <- TRUE
  # }   
  
  str_all_vars <- names(dsetin)
  if ("DOMAIN" %in% toupper(str_all_vars)) str_domain <- as.character(base::unique(dsetin[["DOMAIN"]])) else str_domain <- NULL
  
  str_datevar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ASDT( |$)"))]
  if (length(str_datevar) == 0) str_datevar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ADT( |$)"))]
  if (!is.null(str_domain)) {
    if (length(str_datevar) == 0) str_datevar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "SDT( |$)")))]
    if (length(str_datevar) == 0) str_datevar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "DT( |$)")))]
  }
  if (length(str_datevar) == 0) str_datevar <- NULL 
   
  str_timevar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ASTM( |$)"))]
  if (length(str_timevar) == 0) str_timevar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ATM( |$)"))]
  if (!is.null(str_domain)) {
    if (length(str_timevar) == 0) str_timevar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "STM( |$)")))]
    if (length(str_timevar) == 0) str_timevar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "TM( |$)")))]
  }
  if (length(str_timevar) == 0) str_timevar <- NULL 
                      
  if (tolower(baselineoption[1]) %in% c("date", "time", "relday")) {
    if (! "TRTSDT" %in% toupper(str_all_vars)) {
      message(paste0("RTE", "RROR: RU_BASELINE: TRTSDT is not in DSETIN while it is requred wthen BASELINEOPTION is one of DATE, TIME, RELDAY"))
      bln_CheckParam <- TRUE    
    }
    if (is.null(str_datevar)) {
      message(paste0("RTE", "RROR: RU_BASELINE: None of ADT/ASDT/xxSDT/xxDT is not in DSETIN while it is requred wthen BASELINEOPTION is one of DATE, TIME, RELDAY"))
      bln_CheckParam <- TRUE    
    }    
    if (is.null(str_timevar) & (tolower(baselineoption[1]) == "time")) {
      message(paste0("RTE", "RROR: RU_BASELINE: None of ATM/ASTM/xxSTM/xxTM is not in DSETIN while it is requred wthen BASELINEOPTION is one of DATE, TIME, RELDAY"))
      bln_CheckParam <- TRUE    
    }   
    if ((! "TRTSTM" %in% toupper(str_all_vars)) & (tolower(baselineoption[1]) == "time")) {
      message(paste0("RTE", "RROR: RU_BASELINE: TRTSTM is not in DSETIN while it is requred wthen BASELINEOPTION is one of DATE, TIME, RELDAY"))
      bln_CheckParam <- TRUE    
    }    
  }
  
  str_strescvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)AVALC( |$)"))]
  str_stresnvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)AVAL( |$)"))]
  
  if (length(str_strescvar) == 0 & length(str_stresnvar) == 0) {
    if (length(str_domain) == 0) {
      message(paste0("RTE", "RROR: RU_BASELINE: Can't find result variable AVAL, or AVALC and can't find DOMAIN in DSETIN"))
      bln_CheckParam <- TRUE      
    } else {
      str_strescvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "STRESC( |$)")))]
      if (length(str_strescvar) == 0) {
        str_strescvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "STRE( |$)")))]
      }
      str_stresnvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "STRESN( |$)")))]   
      if (length(str_strescvar) == 0 & length(str_stresnvar) == 0) {
        message(paste0("RTE", "RROR: RU_BASELINE: Can't find result variable ", str_domain, "STRESN", ", ",  str_domain, "STRES", " or ",  str_domain, "STRESC", " in DSETIN"))
        bln_CheckParam <- TRUE      
      }
    }
  }
  
  if (length(str_domain) == 0) str_domain <- NULL
  if (length(str_stresnvar) == 0) str_stresnvar <- NULL
  if (length(str_strescvar) == 0) str_strescvar <- NULL
  
  str_avisitnvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)AVISITN( |$)"))]
  if (length(str_avisitnvar) == 0) str_avisitnvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)VISITNUM( |$)"))]
  if (length(str_avisitnvar) == 0) str_avisitnvar <- NULL  
  
  if (tolower(baselineoption[1]) %in% c("visit", "tpt", "visittpt")) {
    if (is.null(str_avisitnvar)) {
      message("RTE", "RROR: RU_BASELINE: Can't find either AVISITN or VISITNUM variable in DSETIN. One is required while BASELINEOPTION is one of VISIT, TPT, VISITTPT")
      bln_CheckParam <- TRUE      
    }    
  }
  str_tptnvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ATPTN( |$)"))]
  if (length(str_tptnvar) == 0) str_tptnvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "TPTNUM", "( |$)")))]
  if (length(str_tptnvar) == 0) str_tptnvar <- NULL
  if (tolower(baselineoption[1]) %in% c("tpt", "visittpt")) {
    if (is.null(str_tptnvar)) {
      message("RTE", "RROR: RU_BASELINE: Can't find either ATPTN or ", str_domain, "TPTNUM", " variable in DSETIN. One is required while BASELINEOPTION is one of TPT, VISITTPT")
      bln_CheckParam <- TRUE      
    }    
  } 
  
  str_blflvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "BLFL", "( |$)")))]
  if (length(str_blflvar) == 0) str_blflvar <- NULL
  if (rederivebaselineyn & is.null(str_blflvar)) {
    message("RTE", "RROR: RU_BASELINE: Can't ", str_domain, "BLFL", " variable in DSETIN. It required while REDEIRVEBASELINEYN is TRUE")
    bln_CheckParam <- TRUE          
  }
  
  str_atoxgrvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)ATOXGR( |$)"))]
  if (length(str_atoxgrvar) == 0) str_atoxgrvar <- str_all_vars[!is.na(str_extract(str_all_vars, paste0("(^| )(?i)", str_domain, "TOXGR", "( |$)")))]
  if (length(str_atoxgrvar) == 0) str_atoxgrvar <- NULL
  
  if (bln_CheckParam) {
    stop("ru_baseline: stop with error(s)")
    return(invisible())
  }
  
  #
  # Starting of normal processes
  #
  
  str_aperiodvar <- str_all_vars[!is.na(str_extract(str_all_vars, "(^| )(?i)APERIOD( |$)"))]
  if (length(str_aperiodvar) == 0) str_aperiodvar <- NULL
  
  df_data_1 <- dsetin %>% dplyr::mutate(seqbln__=dplyr::row_number())

  if (is.null(str_strescvar)) {
    df_data_1 <- df_data_1 %>% dplyr::mutate(AVALC=NA_character_)
  } else if(str_strescvar != "AVALC") {
    df_data_1 <- df_data_1 %>% dplyr::mutate(AVALC=!! rlang::sym(str_strescvar))    
  }
  if (is.null(str_stresnvar)) {
    df_data_1 <- df_data_1 %>% dplyr::mutate(AVAL=NA_real_)
  } else if(str_stresnvar != "AVAL") {
    df_data_1 <- df_data_1 %>% dplyr::mutate(AVAL=!! rlang::sym(str_stresnvar))    
  }
  
  #
  # Mark all possible baseline records based on given parameters
  # A2INDCD
  #
  if (rederivebaselineyn) {
    if (tolower(baselineoption[1]) == "visittpt") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          AVISITN__=!! rlang::sym(str_avisitnvar),
          ATPTN__=rlang::sym(str_avisitnvar),
          A2INDCD=case_when(
            is.na(AVISITN__) ~ "P",
            AVISITN__ < !! startvisnum[1] | (AVISITN__ == !! startvisnum[1] & (ATPTN__  < startvisnum[1] | ! is.na(ATPTN__))) ~ "P",
            AVISITN__ < !! endvisnum[1] |  (AVISITN__ == !! endvisnum[1] & (ATPTN__  <= endvisnum[1] | is.na(ATPTN__))) ~ "R",
            TRUE ~ NA_character_
          )
        ) %>% dplyr::select(-AVISITN__, -ATPTN__)
    } else if (tolower(baselineoption[1]) == "visit") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          AVISITN__=!! rlang::sym(str_avisitnvar),
          A2INDCD=case_when(
            is.na(AVISITN__) ~ "P",
            AVISITN__ < !! startvisnum[1] ~ "P",
            AVISITN__ <= !! endvisnum[1] ~ "R",
            TRUE ~ NA_character_
          )
        ) %>% dplyr::select(-AVISITN__)  
    } else if (tolower(baselineoption[1]) == "tpt") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          ATPTN__=!! rlang::sym(str_avisitnvar),
          A2INDCD=case_when(
            is.na(ATPTN__) ~ "P",
            ATPTN__ < !! startvisnum[1] ~ "P",
            ATPTN__ <= !! endvisnum[1] ~ "R",
            TRUE ~ NA_character_
            )
          ) %>% dplyr::select(-ATPTN__)    
    } else if (tolower(baselineoption[1]) == "date") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          ADT__=!! rlang::sym(str_datevar),
          A2INDCD=case_when(
            is.na(ADT__) | is.na(TRTSDT) ~ "P",
            ADT__ <= TRTSDT ~ "R",
            TRUE ~ NA_character_
            )
          ) %>% dplyr::select(-ADT__)     
    } else if (tolower(baselineoption[1]) == "time") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          ADT__=!! rlang::sym(str_datevar),
          ATM__=!! rlang::sym(str_timevar),
          A2INDCD=case_when(
            is.na(ADT__) | is.na(TRTSDT) ~ "P",
            ADT__ < TRTSDT | (ADT__ == TRTSDT & (ATM <= TRTSTM | is.na(ATM) | is.na(TRTSTM))) ~ "R",
            TRUE ~ NA_character_
          )
        ) %>% dplyr::select(-ADT__, -ATM__)      
    } else if (tolower(baselineoption[1]) == "relday") {
      df_data_1 <- df_data_1 %>% 
        dplyr::mutate(
          ADT__=!! rlang::sym(str_datevar),
          A2INDCD=case_when(
            is.na(ADT__) | is.na(TRTSDT) ~ "P",
            as.numeric(as.Date(ADT__) - as.Date(TRTSDT - !! rlang::sym(reldays))) < 0 ~ "P",
            ADT__ <= TRTSDT ~ "R",
            TRUE ~ NA_character_
          )
        ) %>% dplyr::select(-ADT__)       
    }
  } else {
    df_data_1 <- df_data_1 %>% 
      dplyr::mutate(
        A2INDCD=case_when(
          !! rlang::sym(str_blflvar) == "Y" ~ "R",
          is.na(!! rlang::sym(str_datevar)) | is.na(TRTSDT) ~ "P",
          !! rlang::sym(str_datevar) < TRTSDT ~ "P",
          TRUE ~ NA_character_
        )
      )       
  }
  df_data_1 <- df_data_1 %>% dplyr::mutate(
    A2IND = case_when(
    A2INDCD == "P" ~ "Pre-treatment",
    A2INDCD == "R" ~ "Baseline",
    TRUE ~ A2INDCD
    )
  )
  
  #
  # Extract possible baseline records based on given parameters. Remove missing
  # values
  #  
  df_base_1 <- df_data_1 %>% dplyr::filter(A2INDCD == "R") %>%
    dplyr::select(all_of(c(byvars, str_avisitnvar, str_tptnvar, str_atoxgrvar, str_datevar, str_timevar, "AVAL", "AVALC", "seqbln__")))

  df_base_1 <- df_base_1 %>% dplyr::filter(! is.na(AVAL) & !is.na(AVALC) & ! (AVALC %in% c("NA", "ND", "NR", "")))
  
  #
  # Derive baseline based on BASELINEOPTION and BASELINETYPE
  # 
  if (tolower(baselineoption[1]) %in% c("visittpt", "visit", "tpt")) str_thisbyvars <- NULL else 
    if (tolower(baselineoption[1]) %in% c("data", "time")) str_thisbyvars <- str_datevar

  if (tolower(baselineoption[1]) == "visittpt") str_thisordervars=c(str_avisitnvar, str_tptnvar, str_datevar, str_timevar, str_atoxgrvar, "AVAL", "AVALC")
  if (tolower(baselineoption[1]) == "visit")    str_thisordervars=c(str_avisitnvar, str_datevar, str_atoxgrvar,  "AVAL", "AVALC")
  if (tolower(baselineoption[1]) == "visit")    str_thisordervars=c(str_tptnvar, str_datevar, str_timevar, str_atoxgrvar, "AVAL", "AVALC")
  if (tolower(baselineoption[1]) == "date")     str_thisordervars=c(str_datevar, str_atoxgrvar, "AVAL", "AVALC")
  if (tolower(baselineoption[1]) == "time")     str_thisordervars=c(str_datevar, str_timevar, str_atoxgrvar, "AVAL", "AVALC")
  
  if (tolower(baselinetype[1]) %in% c("last", "first")) {
    if (tolower(baselineoption[1]) %in% c("date", "time")) {
      if (tolower(baselinetype[1]) == "first") {
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::filter(!! rlang::sym(str_datevar) == max(!! rlang::sym(str_datevar))) %>% 
          dplyr::arrange(across(all_of(c(str_thisordervars))), .by_group = TRUE) %>%
          dplyr::slice_head(n=1) %>%
          dplyr::ungroup()
      } else { 
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::filter(!! rlang::sym(str_datevar) == max(!! rlang::sym(str_datevar))) %>% 
          dplyr::arrange(across(all_of(c(str_thisordervars))), .by_group = TRUE) %>%
          dplyr::slice_tail(n=1) %>%
          dplyr::ungroup()
      }
    } else {
      if (tolower(baselinetype[1]) == "first") {
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::arrange(across(all_of(c(str_thisordervars))), .by_group=TRUE) %>%
          dplyr::slice_head(n=1) %>%
          dplyr::ungroup() 
      } else { 
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::arrange(across(all_of(c(str_thisordervars))), by_group=TRUE) %>%
          dplyr::slice_tail(n=1) %>%
          dplyr::ungroup() 
      }
    }
  } else if (tolower(baselinetype[1]) %in% c("mean", "medina")) {
    if (tolower(baselineoption[1]) %in% c("date", "time")) {
      if (tolower(baselinetype[1]) == "mean") {
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::filter(!! rlang::sym(str_datevar) == max(!! rlang::sym(str_datevar))) %>%
          dplyr::summarize(AVAL=mean(AVAL)) %>%
          dplyr::ungroup() %>% 
          dplyr::mutate(AVALC=NA_character_)
      } else {
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>% 
          dplyr::filter(!! rlang::sym(str_datevar) == max(!! rlang::sym(str_datevar))) %>%
          dplyr::summarize(AVAL=median(AVAL)) %>%
          dplyr::ungroup() %>%  
          dplyr::mutate(AVALC=NA_character_)
      }
      df_base_seq <- df_base_1 %>% 
        dplyr::group_by(across(all_of(c(byvars)))) %>% 
        dplyr::filter(!! rlang::sym(str_datevar) == max(!! rlang::sym(str_datevar))) %>%
        dplyr::ungroup() %>%
        dplyr::select(all_of(c(byvars, "seqbln__")))
    } else {
      if (tolower(baselinetype[1]) == "mean") {
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>%
          dplyr::summarize(AVAL=mean(AVAL)) %>%
          dplyr::ungroup() %>%
          dplyr::select(all_of(c(byvars, "seqbln__")))
      } else { 
        df_base_2 <- df_base_1 %>% 
          dplyr::group_by(across(all_of(c(byvars)))) %>%
          dplyr::summarize(AVAL=median(AVAL)) %>%
          dplyr::ungroup() %>%
          dplyr::select(all_of(c(byvars, "seqbln__")))        
      }
      df_base_seq <- df_base_1 %>% 
        dplyr::select(all_of(c(byvars, "seqbln__")))     
    }
    df_base_2 <- dplyr::left_join(df_base_2, df_base_seq, by=c(byvars)) 
  } 
  
  #
  # Merge baseline record into source data. Derive CHG/PCHG/BASE/BASEC/A2IND
  #
  df_base_3 <- df_base_2 %>% dplyr::select(all_of(c(byvars, "seqbln__", "AVAL", "AVALC"))) %>% dplyr::mutate(BASETYPE=toupper(!! baselinetype[1]), ABLFL="Y") %>% 
    dplyr::rename(BASE=AVAL, BASEC=AVALC)
  str_basevars <- c("BASE", "BASEC")
  
  str_base_extra_vars <- base::intersect(c("ANRHI", "ANRLO", "ANRIND", "ATOXGR", "ATOXGRN"), names(df_data_1))
  if (length(str_base_extra_vars) > 0) {
    df_base_3 <- dplyr::left_join(df_base_3, df_data_1 %>% dplyr::select(all_of(c(byvars, str_base_extra_vars, "seqbln__"))), by=c(byvars, "seqbln__"))
  }
  
  if ("ANRHI" %in% names(df_base_3)) {
    df_base_3 <- df_base_3 %>% dplyr::rename(BNRHI=ANRHI)
    str_basevars <- c(str_basevars, "BNRHI")
  }
  if ("ANRLO" %in% names(df_base_3)) {
    df_base_3 <- df_base_3 %>% dplyr::rename(BNRLO=ANRLO)
    str_basevars <- c(str_basevars, "BNRLO")
  }
  if ("ANRIND" %in% names(df_base_3)) {
    df_base_3 <- df_base_3 %>% dplyr::rename(BNRIND=ANRIND)
    str_basevars <- c(str_basevars, "BNRIND")
  } 
  if ("ATOXGRN" %in% names(df_base_3)) {
    df_base_3 <- df_base_3 %>% dplyr::rename(BTOXGRN=ATOXGRN)
    str_basevars <- c(str_basevars, "BTOXGRN")
  } 
  if ("ATOXGR" %in% names(df_base_3)) {
    df_base_3 <- df_base_3 %>% dplyr::rename(BTOXGR=ATOXGR)
    str_basevars <- c(str_basevars, "BTOXGR")
  } 
  
  df_data_2 <- df_base_3 %>% dplyr::select(seqbln__, ABLFL, BASETYPE) %>% 
    dplyr::right_join(df_data_1, by="seqbln__")
  
  df_data_3 <- df_base_3 %>% dplyr::select(all_of(c(byvars, str_basevars))) %>% 
    dplyr::right_join(df_data_2, by=c(byvars)) %>%
    dplyr::mutate(
      CHG=if_else(A2INDCD %in% c("P", "R"), NA_real_, AVAL - BASE),
      PCHG=if_else((A2INDCD %in% c("P", "R")) | BASE==0, NA_real_, 100 * (AVAL - BASE)/BASE),
    ) 
  
  str_dropvars <- "seqbln__"
  if (is.null(str_strescvar)) str_dropvars <- c(str_dropvars, str_strescvar, "AVALC", "BASEC")
  if (is.null(str_stresnvar)) str_dropvars <- c(str_dropvars, str_stresnvar, "AVAL", "BASE", "CHG", "PCHG")
  
  if (! is.null(str_dropvars)) {
    df_data_3 <- df_data_3 %>% dplyr::select(-all_of(c(str_dropvars)))
  }

  df_out <- ru_labels(df_data_3, newvarlabels)
  
  return(df_out)
}



