#========================================================================= 
# Author:  Yongwei Wang
# Program: rc_adae.R
# Purpose: Create ADAE dataset: based on admiral ad_adae template
# Date:    03/14/2025
#========================================================================= 
rc_adae <- function(
    dsetin = sdtmdata$ae(),
    adsuppjoinyn = FALSE,
    dsetinsupp = sdtmdata$suppae(),
    addatetimeyn = TRUE,
    datevars = c("AESTDTC", "AEENDTC"),
    getadslvarsyn = TRUE,
    dsetinadsl = adamdata$adsl(),
    adslvars = c("SITEID", "AGE", "SEX" ,"RACE", "ACOUNTRY", "TRTSEQ", "TRTSDT"  , "TRTEDT", "FASFL", "ITTFL", "SAFFL", "PPROTFL", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN"),
    decodeyn = TRUE,
    decodepairs = NULL,
    codepairs = c("ASEVN", "ASEV", "AEOUTN", "AEOUT", "ARELN", "AREL"),
    codeformatpairs = c("ASEVN", "ADAE_ASEVN.", "AEOUTN", "ADAE_AEOUTN."),
    decodemethod = "SPEC",
    adreldaysyn = TRUE,
    dyrefdatevar = NULL,
    adperiodyn = FALSE,
    TRTEMFLdays = 0,
    fupfldays = 1,
    durationunits = "Days",
    aoccperiodvar = NULL,
    misschkyn = TRUE,
    attributesyn = TRUE,
    dsplanfile = NULL
){
  # Initialize variables
  G_ABORT <- 0
  str_domain <- "AE"
  df_lastdset <- dsetin
  durationunits <- toupper(durationunits)

  # Main processing
  if (adsuppjoinyn) {
    df_lastdset <- ru_addsupp(df_lastdset, dsetinsupp)
  }
  
  str_dsetin_vars <- names(df_lastdset)

  if (addatetimeyn) {
    df_lastdset <- ru_datetime(df_lastdset)   #, datevars
    str_sdtm_dtvars <- c("DT", "TM", "DTM", "STDT", "STTM", "STDTM", "ENDT", "ENTM", "ENDTM")
    str_adsl_dtvars <- c("ADT", "ATM", "ADTM", "ASTDT", "ASTTM", "ASTDTM", "AENDT", "AENTM", "AENDTM")
    
    for (i in 1:length(str_sdtm_dtvars)) {
      if (paste0(str_domain, str_sdtm_dtvars[i]) %in% str_dsetin_vars) df_lastdset[[str_adsl_dtvars[i]]] <- df_lastdset[[paste0(str_domain, str_sdtm_dtvars[i])]]
    }
    
    if (! "ASTTM" %in% names(df_lastdset)) df_lastdset[["ASTTM"]] <- NA_Date_
    if (! "AENTM" %in% names(df_lastdset)) df_lastdset[["AENTM"]] <- NA_Date_
  }

  if (getadslvarsyn) {
    df_lastdset <- ru_adgetadslvars(df_lastdset, dsetinadsl, adslvars)
  }

  #=============================================================
  # Impute AE start date (day) and end date (day) when missing.
  #=============================================================
  df_lastdset <- df_lastdset %>% 
    dplyr::mutate(
      TRTSDTC=substr(format(as.Date(TRTSDT),format="%Y-%m-%d"),1,7),
      TRTEDTC=substr(format(as.Date(TRTEDT),format="%Y-%m-%d"),1,7),
      ASTDT=ifelse(AESTDTC==TRTSDTC,as.Date(TRTSDT),ifelse(nchar(AESTDTC)==7,as.Date(paste0(AESTDTC,'-01')),as.Date(AESTDT))),
      AENDT=ifelse(AEENDTC==TRTEDTC,as.Date(TRTEDT),ifelse(nchar(AEENDTC)==7,as.Date(paste0(AEENDTC,'-01'))+months(1)-1,as.Date(AEENDT))),
      ASTDT = as.IDate(ASTDT),
      AENDT = as.IDate(AENDT))
  
  #CHK <<- df_lastdset 
  #CHK$ASTDTC <<- format(as.Date(CHK$ASTDT),format="%Y-%m-%d")
  #CHK$AENDTC <<- format(as.Date(CHK$AENDT),format="%Y-%m-%d")
  #CHK <<- CHK %>% select(AESTDTC, AESTDT, ASTDT, ASTDTC, AEENDTC, AEENDT, AENDT, AENDTC, TRTSDTC)

  #print('MADE IT HERE ...')
  #return(0)
  
  if (adperiodyn) {
    df_lastdset <- ru_aperiod(df_lastdset, dsetinadsl, groupbyvars=c("STUDYID", "USUBJID"),
                           stdtvar="AESTDT", endtvar=NULL, reldays=0, sliceaperiod="first") 
  }

  # CHKBEF <<- df_lastdset %>% select(AESTDTC, AESTDT, ASTDT, AEENDTC, AEENDT, AENDT)
  
  if (adreldaysyn) {
    df_lastdset <- ru_adreldays(df_lastdset, dyrefdatevar = dyrefdatevar, domaincode = str_domain)
  }

  # CHKAFT <<- df_lastdset %>% select(AESTDTC, AESTDT, ASTDT, AEENDTC, AEENDT, AENDT, ASTDY, AENDY)

  #CHK <<- df_lastdset %>% select(AESTDTC, AESTDT, ASTDT, AEENDTC, AEENDT, AENDT, ASTDY, AENDY)
  #print('MADE IT HERE ...')
  #return(0)
  
  # Domain-specific derivations
  str_dsetin_vars <- names(df_lastdset)
  
  temp_data <- df_lastdset
  if (paste0(str_domain, "TPT")  %in% str_dsetin_vars) temp_data[["ATPT"]] <- temp_data[[paste0(str_domain, "TPT")]]
  if (paste0(str_domain, "TPTREF") %in% str_dsetin_vars) temp_data[["ATPTREF"]] <- temp_data[[paste0(str_domain, "TPTREF")]]
  if (paste0(str_domain, "TPTNUM") %in% str_dsetin_vars) temp_data[["ATPTN"]] <- temp_data[[paste0(str_domain, "TPTNUM")]]
  if (! paste0(str_domain, "ASEV") %in% str_dsetin_vars) {
    if (paste0(str_domain, "AESEV") %in% str_dsetin_vars) temp_data[["ASEV"]] <- temp_data[[paste0(str_domain, "AESEV")]]
  }
  if (! paste0(str_domain, "AREL") %in% str_dsetin_vars) {
    if (paste0(str_domain, "REL") %in% str_dsetin_vars) temp_data[["AREL"]] <- if_else(is.na(temp_data[[paste0(str_domain, "REL")]]), 
                                                                                                           "Y", temp_data[[paste0(str_domain, "REL")]])
  }  
  if (! paste0(str_domain, "ATOXGR") %in% str_dsetin_vars) {
    if (paste0(str_domain, "AETOXGR") %in% str_dsetin_vars) temp_data[["ATOXGR"]] <- temp_data[[paste0(str_domain, "AETOXGR")]]
  } 
  
  if (base::all(c("TRTSDT", "TRTEDT", "ASTDT", "AENDT") %in% str_dsetin_vars)) {
    if (! "TRTEMFL" %in% str_dsetin_vars) temp_data <- temp_data %>%
        dplyr::mutate(TRTEMFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & !is.na(TRTEDT) & TRTSDT <= ASTDT & ASTDT <= (TRTEDT + !! TRTEMFLdays), "Y", NA_character_))
    if (! "PREFL" %in% str_dsetin_vars) temp_data <- temp_data %>%
        dplyr::mutate(PREFL = if_else(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT, "Y", NA_character_))
    if ("FUPFL" %in% str_dsetin_vars) temp_data <- temp_data %>%
        dplyr::mutate(FUPFL = if_else(!is.na(ASTDT) & !is.na(TRTEDT) & ASTDT > (TRTEDT + fupfldays), "Y", NA_character_))    
    
    # Duration calculations
    if (durationunits == "HOURS" && (base::all(c("ASTTM", "AENTM")  %in% str_dsetin_vars))) {
      temp_data <- temp_data %>% dplyr::mutate(
        ADURN = case_when(
          !! durationunits == "HOURS" ~ {
            if (!is.na(AENTM) & !is.na(ASTTM)) {
              duration=(as.numeric(base::difftime(AENDT, ASTDT, units = "secs")) + as.numeric(AENTM - ASTTM))/3600
            } else {
              duration=(as.numeric(base::difftime(AENDT, ASTDT, units = "secs")))/3600 
            } 
            if_else(duration == ceiling(duration), duration + 1, duration)
          }, 
          TRUE ~ NA_real_
        )
      )
    } else {
      temp_data <- temp_data %>% dplyr::mutate(
        ADURN = case_when(
          (is.na(ASTDT) | is.na(AENDT)) ~ NA_real_,
          (durationunits=="YEARS")      ~ {lubridate::year(AENDT + 1) -lubridate::year(ASTDT) -
                                           if_else(lubridate::month(AENDT + 1) < lubridate::month(ASTDT) |
                                                  (lubridate::month(AENDT + 1) == lubridate::month(ASTDT) & lubridate::day(AENDT + 1) < lubridate::day(ASTDT)), 0, 1)},
          (durationunits == "MONTHS")   ~ {(lubridate::year(AENDT + 1) - lubridate::year(ASTDT)) * 12 +
                                           (lubridate::month(AENDT + 1) - lubridate::month(ASTDT) - 1) -
                                           if_else(lubridate::day(AENDT + 1) < lubridate::day(ASTDT), 0, 1)},
          (durationunits == "WEEKS")    ~ {as.numeric(base::difftime(AENDT + 1, ASTDT, units = "days")) / 7},
          (durationunits == "DAYS")     ~ {as.numeric(base::difftime(AENDT + 1, ASTDT, units = "days"))},
          # (durationunits == "HOURS")  ~ {message(paste0('*** MADE IT HERE 3 ***',durationunits))
          #                                if (!is.na(AENTM) & !is.na(ASTTM)) {duration=(as.numeric(base::difftime(AENDT, ASTDT, units = "secs")) + as.numeric(AENTM - ASTTM))/3600
          #                                   } else {duration=(as.numeric(base::difftime(AENDT, ASTDT, units = "secs")))/3600} 
          #                                if_else(duration == ceiling(duration), duration + 1, duration)}, 
          TRUE ~ NA_real_
        )
      )
      TEMP_DATA <<- temp_data
    }
    temp_data <- temp_data %>% dplyr::mutate(
      seqn__ = row_number(),
      ADURU = if_else(!is.na(ADURN), !! durationunits, NA_character_),
      ADURC = if_else(!is.na(ADURN), paste(ADURN, ADURU), NA_character_),
      AONGO = if_else(toupper(AEOUT) %in% c("RECOVERING/RESOLVING", "NOT RECOVERED/NOT RESOLVED"), "Y", "N")
    )
  }

  df_lastdset <- temp_data
  
  # Manual derivation of ASEVN
  str_dsetin_vars <- names(df_lastdset)
  
  if (! "ASEVN" %in% str_dsetin_vars && "ASEV" %in% str_dsetin_vars) {
    df_lastdset <- df_lastdset %>%
      dplyr::mutate(
        ASEVN = case_when(
          ASEV == "MILD" ~ 1,
          ASEV == "MODERATE" ~ 2,
          ASEV == "SEVERE" ~ 3,
          TRUE ~ NA_real_
        )
      )
  }

  # Manual derivation of AETOXGRN
  if (! "AETOXGRN" %in% str_dsetin_vars && "AETOXGR" %in% str_dsetin_vars) {
    df_lastdset <- df_lastdset %>%
      dplyr::mutate(
        AETOXGRN = case_when(
          is.na(AETOXGR) ~ NA_real_,
          AETOXGR == "" ~ NA_real_,
          toupper(gsub("[^A-Z0-9]", "", AETOXGR)) %in% c("GRADE1", "1") ~ 1,
          toupper(gsub("[^A-Z0-9]", "", AETOXGR)) %in% c("GRADE2", "2") ~ 2,
          toupper(gsub("[^A-Z0-9]", "", AETOXGR)) %in% c("GRADE3", "3") ~ 3,
          toupper(gsub("[^A-Z0-9]", "", AETOXGR)) %in% c("GRADE4", "4") ~ 4,
          toupper(gsub("[^A-Z0-9]", "", AETOXGR)) %in% c("GRADE5", "5") ~ 5,
          TRUE ~ NA_real_
        )
      )
    if (any( ! is.na(df_lastdset[["AETOXGR"]]) & df_lastdset[["AETOXGR"]] != "" & is.na(df_lastdset[["AETOXGRN"]]))) {
      message("WARNING: rc_adae: AETOXGR variable contains non-standard value")
      remove(l_invalidtoxgr)
    }
    message("WARNING: rc_adae: AETOXGRN values assigned manually. Check decode format is defined in the spec file or CODEFORMATPAIRS parameter, and that format exists in the Controlled Terminology.")
  }
  
  #
  # Define a function to derive AE occurrence flags. 
  # The function will return a dataset with AE occurence flag.
  #
  # Parameters of the function are as follows:
  # 
  # dsetin   - Input data
  # varlist  - Variable list for sort order
  # serious  - Serious adverse event occurence flags to be derived
  # related  - Drug related occurence flags to be derived
  # related1 - Drug related occurence flags to be derived for ADaM IG 1.1
  # withd    - AE leading to Discontinuation NP001 
  # period   - for flags within treatment/treatment period NP001 
  #
  # Occurrence flag derivations
  if (!is.null(aoccperiodvar) && ! (aoccperiodvar %in% str_dsetin_vars)) {
    message("WARNING: rc_adae: Variable ", aoccperiodvar, " specified in parameter AOCCPERIODVAR does not exist in ", toString(deparse(substitute(df_lastdset))), " or has not been derived. Derivation of AOCPxxFL variables will not be performed.")
  }

  aeoccfl <- function(dsetin, varlist, ordervarlist, serious = "N", related = "N", related1 = "N", withd = "N", period = "N", occflvar = "aoccfl") {
    # Initialize dataset and core variables
    df_accdata <- dsetin

    # Apply filters based on conditions
    if ("TRTEMFL" %in% str_dsetin_vars) {
      df_accdata <- df_accdata %>% dplyr::filter(TRTEMFL == "Y")
    }
    if (period == "Y" && aoccperiodvar %in% str_dsetin_vars) {
      df_accdata <- df_accdata %>% dplyr::filter(!is.na(.data[[aoccperiodvar]]))
    }
    if (serious == "Y") df_accdata <- df_accdata %>% dplyr::filter(AESER == "Y")
    if (related == "Y") df_accdata <- df_accdata %>% dplyr::filter(AEREL == "Y")
    if (related1 == "Y") df_accdata <- df_accdata %>% dplyr::filter(AREL == "Y")
    if (withd == "Y") df_accdata <- df_accdata %>% dplyr::filter(AEACNOTH == "WITHDRAWN FROM STUDY")
    
    occflvar <- toupper(occflvar)

    # Process data: arrange, group, select first occurrence, and add flag
    df_accdata <- df_accdata %>%
      dplyr::group_by(!!!syms(unlist(varlist))) %>%
      dplyr::arrange(!!! ordervarlist, .by_group = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::mutate(!! rlang::sym(occflvar) := "Y") %>%
      dplyr::ungroup() %>%
      dplyr::select(USUBJID, seqn__, !! rlang::sym(occflvar)) %>%
      dplyr::arrange(USUBJID, seqn__) %>%
      dplyr::right_join(dsetin, by=c("USUBJID", "seqn__"))

    return(df_accdata)
  }
  
  # Define configurations for aeoccfl calls
  ae_configs <- list(
    list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM), occflvar = "aoccfl"),
    list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM), occflvar = "aoccsfl"),
    list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM), occflvar = "aoccpfl")
  )

  # Additional configurations with period
  if (!is.null(aoccperiodvar) && aoccperiodvar %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID", aoccperiodvar), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), period = "Y", occflvar = "aocpfl"),
      list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC", "ASTDT"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), period = "Y", occflvar = "aocpsfl"),
      list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), period = "Y", occflvar = "aocppfl")
    ))
    
    if ("AESER" %in% str_dsetin_vars) {
      ae_configs <- c(ae_configs, list(
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", period = "Y", occflvar = "aocp1fl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", period = "Y", occflvar = "aocp1sfl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", period = "Y", occflvar = "aocp1pfl")
      ))
    }
    
    if ("AREL" %in% str_dsetin_vars) {
      ae_configs <- c(ae_configs, list(
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp2fl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp2sfl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC ","AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp2pfl")
      ))
    }
    
    if ("ASEVN" %in% str_dsetin_vars && "AREL" %in% str_dsetin_vars) {
      ae_configs <- c(ae_configs, list(
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp3fl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp3sfl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC", "AEDECOD"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", period = "Y", occflvar = "aocp3pfl")
      ))
    }
    
    if ("AEACNOTH" %in% str_dsetin_vars) {
      ae_configs <- c(ae_configs, list(
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", period = "Y", occflvar = "aocp4fl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", period = "Y", occflvar = "aocp4sfl"),
        list(varlist = c("STUDYID", "USUBJID", aoccperiodvar, "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", period = "Y", occflvar = "aocp4pfl")
      ))
    }
  }
  
  # Other variable existence checks
  if ("ASEV" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM), occflvar = "aoccifl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM), occflvar = "aoccsifl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM), occflvar = "aoccpifl")
    ))
    
    if ("AREL" %in% str_dsetin_vars) {
      ae_configs <- c(ae_configs, list(
        list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc3fl"),
        list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc3sfl"),
        list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(desc(ASEV), ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc3pfl")
      ))
    }
  }

  if ("AESER" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", occflvar = "aocc11fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", occflvar = "aocc12fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", occflvar = "aocc13fl"),
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", occflvar = "aocc1fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", occflvar = "aocc1sfl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), serious = "Y", occflvar = "aocc1pfl")
    ))
  }

  if ("AEREL" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM), related = "Y", occflvar = "aocc21fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM), related = "Y", occflvar = "aocc22fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM), related = "Y", occflvar = "aocc23fl")
    ))
  }
  
  if ("AREL" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc2fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc2sfl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), related1 = "Y", occflvar = "aocc2pfl")
    ))
  }
  
  if (base::all(c("AEREL", "AESER") %in% str_dsetin_vars)) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", related = "Y", occflvar = "aocc31fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", related = "Y", occflvar = "aocc32fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM), serious = "Y", related = "Y", occflvar = "aocc33fl")
    ))
  }
  
  if ("AETOXGR" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(desc(AETOXGRN), ASTDT, ASTTM), occflvar = "aocc41fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(desc(AETOXGRN), ASTDT, ASTTM), occflvar = "aocc42fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(desc(AETOXGRN), ASTDT, ASTTM), occflvar = "aocc43fl")
    ))
  }

  if ("AEACNOTH" %in% str_dsetin_vars) {
    ae_configs <- c(ae_configs, list(
      list(varlist = c("STUDYID", "USUBJID"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", occflvar = "aocc4fl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", occflvar = "aocc4sfl"),
      list(varlist = c("STUDYID", "USUBJID", "AESOC", "AEDECOD"), ordervarlist=exprs(ASTDT, ASTTM, AESEQ), withd = "Y", occflvar = "aocc4pfl")
    ))
  }

  temp_data <- df_lastdset
  for (i in 1:length(ae_configs)) {
    df_lastdset <- do.call(aeoccfl, c(base::append(ae_configs[[i]], list(dsetin=expr(df_lastdset)))))
  } 

  df_lastdset <- df_lastdset %>% dplyr::select(-seqn__)
  
  # Final processing
  if (attributesyn) {
    if (file.exists(toString(dsplanfile))) {
      df_lastdset <- ru_attrib(df_lastdset, dsplanfile)
    } else {
      message("rc_adae: ATTRIBUTESYN is Y, but data plan file given by DSPLANFILE is either NULL or does not exist")
    }
  }

  #======================================================
  # Set all missings to NA (both character and numeric)
  #======================================================
  df_lastdset <- df_lastdset %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ na_if(base::trimws(.), "")))

  # Check for all missing columns
  if (nrow(df_lastdset) > 0 && misschkyn) {
    ru_misschk(df_lastdset)
  }

  return(df_lastdset)
}
