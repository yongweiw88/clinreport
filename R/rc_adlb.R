#' To create ADAE ADaM dataset
#'
#' Pass in SDTM AE and other parameters, and return ADAE data
#'
#' @param dsetin Input dataset
#' @param adsuppjoinyn If supplemental dataset is required to be joined with parent domain 
#' @param dsetinsupp Input supplemental dataset
#' @param addatetimeyn Flag to indicate if ru_datetime utility is to be executed 
#' @param getadslvarsyn Flag to indicate if ru_adgetadslvars utility need to be called
#' @param dsetinadsl Input ADSL or ADTRT dataset
#' @param adslvars List of variables from DSETINADSL
#' @param adgettrtyn Flag to indicate if ru_adgettrt utility is to be executed 
#' @param decodeyn Flag to indicate if ru_decode utility is to be executed 
#' @param codedecodevarpairs List of paired code/decode for decode creation
#' @param varcodelistpairs Paired code/codelist for decode creation
#' @param codelistnames List of paired code/format for code creation
#' @param adreldaysyn Flag to indicate if ru_adreldays utility is to be executed
#' @param dyrefdatevar Reference date variable for relative days derivation
#' @param advisityn Flag to indicate if ru_advisit utility is to be executed 
#' @param avisitnfmt Format used to derive AVISITN from VISITNUM
#' @param avisitfmt Format used to derive AVISIT from VISIT
#' @param misschkyn Flag to indicate if ru_misschk utility is to be executed 
#' @param attributesyn Flag to indicate if ru_attrib utility is to be executed 
#' @param paramcdmappingdset Dataset containing PARAMCD mapping for LBTESTCD and qualifiers
#' @param fastedtestcds Parameter codes that must consider the LBFAST variable
#' @param fastedmissing Fasting status when LBFAST is missing (F/NF/missing)
#' @param flaggingsubset Condition to identify records to be flagged
#' @param adbaselnyn Flag to indicate if ru_adbaseln utility is to be executed 
#' @param rederivebaselineyn Flag to indicate if baseline is to be re-derived
#' @param baselineoption Calculation of baseline option
#' @param reldays Number of days prior to start of study medication
#' @param startvisnum VISITNUM and/or ATPTN value for start of baseline range
#' @param endvisnum VISITNUM and/or ATPTN value for end of baseline range
#' @param baselinetype How to calculate baseline for multiple baseline records
#' @param derivedbaselinerowinfo Statements to define visit/timepoint variables on derived baseline
#' @param adchgfgyn Flag to indicate if ru_adchgccfg utility is to be executed for CHG
#' @param adccfgyn Flag to indicate if ru_adchgccfg utility is to be executed for CC
#' @param cpdsrng Clinical Pharmacology Range identifier
#' @param critdset Flagging criteria dataset name
#' @param dgcd Compound identifier
#' @param studyid Study identifier
#' 
#' @return Column names which exist or do not exist.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'
rc_adlb <- function(
    dsetin = rfenv$sdtmdata$lb(),                    
    adsuppjoinyn  = FALSE,                        
    dsetinsupp = NULL,                           
    addatetimeyn  = TRUE,                        
    getadslvarsyn  = TRUE,                       
    dsetinadsl = adamdata$adsl(),              
    adslvars = c("SITEID", "AGE", "SEX", "RACE", "ACOUNTRY", "TRTSDT", "TRTEDT", "TRTSEQ", "FASFL", "ITTFL", "SAFFL", "PPROTFL"),
    adgettrtyn  = TRUE,                          
    decodeyn  = FALSE,                            
    codedecodevarpairs = NULL,                          
    codelistnames = NULL,    
    varcodelistpairs = NULL,
    adreldaysyn  = TRUE,                         
    dyrefdatevar = NULL,                         
    advisityn  = TRUE,                           
    avisitnfmt = NULL,                           
    avisitfmt = NULL,                            
    misschkyn  = TRUE,                           
    attributesyn  = TRUE,                        
    paramcdmappingdset = diction$adlbparm(),   
    fastedtestcds = "NONE",                    
    fastedmissing = NULL,                        
    flaggingsubset = expr(PARCAT1 %in% c('CHEMISTRY', 'URINALYSIS', 'HEMATOLOGY')),
    adbaselnyn  = TRUE,                          
    rederivebaselineyn  = FALSE,                  
    baselineoption = "DATE",                   
    reldays = NULL,                              
    startvisnum = NULL,                          
    endvisnum = NULL,                            
    baselinetype = "LAST",                     
    derivedbaselinerowinfo = NULL,               
    adchgfgyn  = TRUE,                           
    adccfgyn  = TRUE,                            
    cpdsrng = NULL,                              
    critdset = NULL,                             
    dgcd = NULL,                                 
    studyid = NULL,
    dsplanfile = NULL 
) {
  # dsetin = rfenv$sdtmdata$lb() |> dplyr::filter(USUBJID=="208090.401001"& LBTESTCD=="ALB" & LBCAT =="CHEMISTRY")
  # dsetin = lb
  # adsuppjoinyn  = FALSE
  # dsetinsupp = NULL
  # addatetimeyn  = TRUE
  # getadslvarsyn  = TRUE
  # dsetinadsl = adamdata$adsl()
  # adslvars = c("SITEID", "AGE", "SEX", "RACE", "ACOUNTRY", "TRTSDT", "TRTEDT", "TRTSEQ", "FASFL", "ITTFL", "SAFFL", "PPROTFL")
  # adgettrtyn  = TRUE
  # adgettrtvars = c("TRT01P", "TRT01PN", "TRT01A", "TRT01AN")
  # decodeyn  = FALSE
  # codedecodevarpairs = NULL
  # codelistnames = NULL
  # varcodelistpairs = NULL
  # adreldaysyn  = TRUE
  # dyrefdatevar = NULL
  # advisityn  = TRUE
  # avisitnfmt = NULL
  # avisitfmt = NULL
  # misschkyn  = TRUE
  # attributesyn  = TRUE
  # paramcdmappingdset = diction$adlbparm()
  # fastedtestcds = "NONE"
  # fastedmissing = NULL
  # flaggingsubset =  expr(PARCAT1 %in% c('CHEMISTRY', 'URINALYSIS', 'HEMATOLOGY'))
  # adbaselnyn  = TRUE
  # rederivebaselineyn  = FALSE
  # baselineoption = "DATE"
  # reldays = NULL
  # startvisnum = NULL
  # endvisnum = NULL
  # baselinetype = "LAST"
  # derivedbaselinerowinfo = NULL
  # adchgfgyn  = TRUE
  # adccfgyn  = TRUE
  # cpdsrng = NULL
  # critdset = NULL
  # dgcd = NULL
  # studyid = NULL
  # dsplanfile = NULL
  
  # Initialize global variables (equivalent to SAS global macros)
  G_SUBJID <- "USUBJID"  # Assuming this is a global variable for subject ID
  
  # Local variables
  str_domain <- "LB"
  df_lastdset <- dsetin
  
  # Parameter validation

  # Validate required parameters
  if (is.null(dsetin)) {
    message("ERROR: rc_adlb: Parameter dsetin is a required parameter, provide a dataset name.")
    rfenv$G_ABORT <<- 1
  }

  if (is.null(paramcdmappingdset)) {
    message("ERROR: rc_adlb: Parameter paramcdmappingdset is a required parameter, provide a dataset name.")
    rfenv$G_ABORT <<- 1
  }
  if (is.null(fastedtestcds)) {
    message("ERROR: rc_adlb: Parameter fastedtestcds cannot be missing.")
    message("ERROR: rc_adlb: Please use NONE, ALL or a comma separated list of quoted parameter codes.")
    rfenv$G_ABORT <<- 1
  }
  if (is.null(fastedmissing)) fastedmissing=""
  if (! is.null(fastedmissing) & ! (fastedmissing %in% c("F", "NF", ""))) {
    message("ERROR: rc_adlb: Parameter fastedmissing should either be F, NF or missing (blank).")
    rfenv$G_ABORT <<- 1
  }

  # Abort if errors
  if (rfenv$G_ABORT == 1) {
    return(invisible(NULL))
  }
  
  # Create working dataset if dsetin has options (simplified, assuming no dataset options in R)
  df_lastdset <- dsetin
  
  # Validate fastedtestcds
  if (fastedtestcds != "NONE" && fastedtestcds != "ALL") {
    fasted_codes <- strsplit(gsub("['\"]", "", fastedtestcds), ",")[[1]]
    if (!all(fasted_codes %in% df_lastdset[[paste0(str_domain, "TESTCD")]])) {
      message("ERROR: rc_adlb: Parameter fastedtestcds must be NONE, ALL or a comma separated list of quoted parameter codes.")
      rfenv$G_ABORT <<- 1
      return(invisible(NULL))
    }
  }
  
  # Main processing
  
  # Merge supplemental dataset
  if (adsuppjoinyn) {
    df_lastdset <- ru_adsupp(df_lastdset, dsetinsupp)
  }
  
  # Check LBFAST variable existence
  if (fastedtestcds != "NONE" && fastedtestcds != "") {
    if (! paste0(str_domain("FAST")) %in% names(df_lastdset)) {
      message("ERROR: rc_adlb: Cannot process fastedtestcds as the variable LBFAST is not on the input dataset.")
      rfenv$G_ABORT <<- 1
      return(invisible(NULL))
    }
  }
  
  # Convert date/time variables
  
  if (addatetimeyn) {
    df_lastdset <- ru_datetime(df_lastdset)
    str_sdtm_dtvars <- c("DT", "TM", "DTM", "STDT", "STTM", "STDTM", "ENDT", "ENTM", "ENDTM")
    str_adsl_dtvars <- c("ADT", "ATM", "ADTM", "ASTDT", "ASTTM", "ASTDTM", "AENDT", "AENTM", "AENDTM")
    
    for (i in 1:length(str_sdtm_dtvars)) {
      if (paste0(str_domain, str_sdtm_dtvars[i]) %in% names(df_lastdset)) df_lastdset[[str_adsl_dtvars[i]]] <- df_lastdset[[paste0(str_domain, str_sdtm_dtvars[i])]]
    }
  }
  
  str_allvars <- names(df_lastdset)
  str_eventdtvar <- str_allvars[grepl(paste0("^ASTDTC$"), str_allvars, ignore.case = TRUE)]
  if (length(str_eventdtvar) < 1) str_eventdtvar <- str_allvars[grepl(paste0("^ASTDT$"), str_allvars, ignore.case = TRUE)] 
  if (length(str_eventdtvar) < 1) str_eventdtvar <- str_allvars[grepl(paste0("^ADTC$"), str_allvars, ignore.case = TRUE)] 
  if (length(str_eventdtvar) < 1) str_eventdtvar <- str_allvars[grepl(paste0("^ADT$"), str_allvars, ignore.case = TRUE)] 
  if (length(str_eventdtvar) < 1) str_eventdtvar <- str_allvars[grepl(paste0("^", str_domain, "STDTC$"), str_allvars, ignore.case = TRUE)] 
  if (length(str_eventdtvar) < 1) str_eventdtvar <- str_allvars[grepl(paste0("^", str_domain, "DTC$"), str_allvars, ignore.case = TRUE)] 

  str_eventtmvar <- NULL
  if (str_eventdtvar == "ASTDT") str_eventtmvar <- str_allvars[grepl(paste0("^ASTTM$"), str_allvars, ignore.case = TRUE)] 
  if (str_eventdtvar == "ADT") str_eventtmvar <- str_allvars[grepl(paste0("^ATM$"), str_allvars, ignore.case = TRUE)] 
  if (length(str_eventdtvar) < 1) str_eventdtvar <- NULL
  if (length(str_eventtmvar) < 1) str_eventtmvar <- NULL
  
  if ( ! is.null(str_eventtmvar) & ! is.null(str_eventdtvar)) {
    df_lastdset <- df_lastdset %>% dplyr::mutate(
      EVENT_TMC = format(!! rlang::sym(str_eventtmvar), "%H:%M"),
      EVENT_DTC = format(as.Date(!! rlang::sym(str_eventdtvar)), "%Y-%m-%d"),
      EVENT_DTC = ifelse(is.na(EVENT_TMC), EVENT_DTC, paste0(EVENT_DTC, "T", EVENT_TMC))
    ) %>% dplyr::select(-EVENT_TMC)
    str_eventdtvar <- "EVENT_DTC"
  }
  # Fetch ADSL variables
  if (getadslvarsyn) {
    df_lastdset <- ru_adgetadslvars(df_lastdset, dsetinadsl, adslvars)
  }

  # Derive period variables
  if (adgettrtyn) {
    df_lastdset <- ru_aperiod(df_lastdset, dsetinadsl, eventtype = "PL", groupbyvars=c("STUDYID", "USUBJID"),
                              stdtvar=str_eventdtvar, endtvar=NULL, reldays=0, sliceaperiod="first") 
  }
  
  # Derive AVISIT and AVISITN
  if (advisityn) {
    df_lastdset <- ru_advisit(
      dsetin = df_lastdset,
      avisitfmt = avisitfmt,
      avisitnfmt = avisitnfmt,
      codelistnames= codelistnames
    )
  }
  
  # Derive relative days
  if (adreldaysyn) {
    df_lastdset <- ru_adreldays(
      dsetin = df_lastdset,
      dyrefdatevar = dyrefdatevar,
      domaincode = str_domain
    )
  }
  
  df_tmp_data <- df_lastdset
  # Domain-specific derivations
  str_sdtm_vars <- paste0(str_domain, c("TPT", "TPTREF", "TPTNUM", "CAT", "STRESC", "STRESN", "STNRLO", "STNRHI", "NRIND", "SEQ", "TOX", "TOXGR", "LOC", "STRESU", "METHOD"))
  str_sdtm_vars_exist <- base::intersect(str_sdtm_vars, names(df_lastdset))
  str_sdtm_vars_nonexist <- base::setdiff(str_sdtm_vars, names(df_lastdset))

  str_dsetin_vars <- names(df_lastdset)
  
  if (paste0(str_domain, "TPT") %in% str_dsetin_vars) df_tmp_data[["ATPT"]] <- df_tmp_data[[paste0(str_domain, "TPT")]]
  if (paste0(str_domain, "TPTREF") %in% str_dsetin_vars) df_tmp_data[["ATPTREF"]] <- df_tmp_data[[paste0(str_domain, "TPTREF")]]
  if (paste0(str_domain, "STNRLO") %in% str_dsetin_vars) df_tmp_data[["ANRLO"]] <- df_tmp_data[[paste0(str_domain, "STNRLO")]]
  if (paste0(str_domain, "STNRHI") %in% str_dsetin_vars) df_tmp_data[["ANRHI"]] <- df_tmp_data[[paste0(str_domain, "STNRHI")]]
  if (paste0(str_domain, "NRIND") %in% str_dsetin_vars) df_tmp_data[["ANRIND"]] <- df_tmp_data[[paste0(str_domain, "NRIND")]]
  if (paste0(str_domain, "TPTNUM") %in% str_dsetin_vars) df_tmp_data[["ATPTN"]] <- df_tmp_data[[paste0(str_domain, "TPTNUM")]]
                                                                                               
  if (paste0(str_domain, "CAT") %in% str_dsetin_vars) df_tmp_data[["PARCAT1"]] <- df_tmp_data[[paste0(str_domain, "CAT")]]
  
  if (paste0(str_domain, "STRESC") %in% str_dsetin_vars) df_tmp_data[["AVALC"]] <- df_tmp_data[[paste0(str_domain, "STRESC")]] else 
    df_tmp_data[["AVALC"]] <- NA_character_
  if (paste0(str_domain, "STRESN") %in% str_dsetin_vars) df_tmp_data[["AVAL"]] <- df_tmp_data[[paste0(str_domain, "STRESN")]] else
    df_tmp_data[["AVAL"]] <- NA_real_
  
  if (paste0(str_domain, "SEQ") %in% str_dsetin_vars) df_tmp_data[["SRCSEQ"]] <- df_tmp_data[[paste0(str_domain, "SEQ")]]  
  
  if (paste0(str_domain, "TOX") %in% str_dsetin_vars) df_tmp_data[["ATOXDSC"]] <- df_tmp_data[[paste0(str_domain, "TOX")]] 
  if (paste0(str_domain, "TOXGR") %in% str_dsetin_vars) {
    df_tmp_data <- df_tmp_data %>% dplyr::mutate(
      ATOXGRN = case_when(
        is.na(!! rlang::sym(paste0(str_domain, "TOXGR"))) ~ NA_real_,
        !! rlang::sym(paste0(str_domain, "TOXGR")) == "" ~ NA_real_,
        toupper(gsub("[^A-Z0-9]", "", !! rlang::sym(paste0(str_domain, "TOXGR")))) %in% c("GRADE1", "1") ~ 1,
        toupper(gsub("[^A-Z0-9]", "", !! rlang::sym(paste0(str_domain, "TOXGR")))) %in% c("GRADE2", "2") ~ 2,
        toupper(gsub("[^A-Z0-9]", "", !! rlang::sym(paste0(str_domain, "TOXGR")))) %in% c("GRADE3", "3") ~ 3,
        toupper(gsub("[^A-Z0-9]", "", !! rlang::sym(paste0(str_domain, "TOXGR")))) %in% c("GRADE4", "4") ~ 4,
        toupper(gsub("[^A-Z0-9]", "", !! rlang::sym(paste0(str_domain, "TOXGR")))) %in% c("GRADE5", "5") ~ 5,
        TRUE ~ NA_real_
      ),
      ATOXGR = ifelse(is.na(ATOXGRN), NA_character_, paste0("Grade ", as.character(ATOXGRN)))
    )
    if ("ATOXDSC" %in% names(df_tmp_data)) {
      df_tmp_data <- df_tmp_data %>% dplyr::mutate(
        ATOXGRHN = ifelse(grepl("high", tolower(ATOXDSC)), ATOXGRN, NA_real_),
        ATOXGRLN = ifelse(grepl("low", tolower(ATOXDSC)), ATOXGRN, NA_real_),
        ATOXGRH = ifelse(is.na(ATOXGRHN), NA_character_, paste0("Grade ", as.character(ATOXGRHN))),
        ATOXGRL = ifelse(is.na(ATOXGRLN), NA_character_, paste0("Grade ", as.character(ATOXGRLN)))
      )
    }
  }
  
  str_forparam_vars <- intersect(paste0(str_domain, c("POS", "LOC", "SPEC", "TEST", "METHOD")), names(df_lastdset))                                                                        
  df_tmp_data <- df_tmp_data %>% 
    tidyr::unite("PARAM", all_of(str_forparam_vars), remove=FALSE, na.rm=TRUE, sep=" ") %>%
    dplyr::mutate(PARAM = if_else(is.na(!! rlang::sym(paste0(str_domain, "STRESU"))) | !! rlang::sym(paste0(str_domain, "STRESU")) == "", PARAM,
                                  paste0(stringr::str_trim(PARAM), " (", !! rlang::sym(paste0(str_domain, "STRESU")), ")"))
    )
  
  df_tmp_data <- df_tmp_data %>%
    dplyr::mutate(
      PARAMLBL = if_else(is.na(!! rlang::sym(paste0(str_domain, "STRESU"))) | !! rlang::sym(paste0(str_domain, "STRESU")) == "", 
                         !! rlang::sym(paste0(str_domain, "TEST")),
                         paste0(stringr::str_trim(!! rlang::sym(paste0(str_domain, "TEST"))), " (", !! rlang::sym(paste0(str_domain, "STRESU")), ")")),
      AVALC = ifelse(is.na(AVAL), AVALC, as.character(AVAL)),
      SRCVAR = ifelse(! is.na(AVAL), paste0(!!str_domain, "STRESN"), paste0(!!str_domain, "STTRESC")),
      AVAL = suppressWarnings(as.numeric(if_else(grepl("<", AVALC) & is.na(AVAL), as.numeric(sub("<", "", AVALC)) - 0.0001, AVAL))),
      AVAL = suppressWarnings(as.numeric(if_else(grepl(">", AVALC) & is.na(AVAL), as.numeric(sub(">", "", AVALC)) + 0.0001, AVAL))),
      SRCDOM = !! str_domain
    )
  
  df_lastdset <- df_tmp_data 
  
  # Add PARAMCD based on the DICTION.ADLBPARM dataset and check for missing PARAMCD
  
  
  # Add PARAMCD from mapping dataset
  df_parm_data <- paramcdmappingdset 
  str_merge_vars <- intersect(paste0(str_domain, c("TESTCD", "STRESU", "POS", "LOC", "SPEC", "METHOD")), names(df_lastdset))                                                                        
  str_merge_vars <- base::intersect(str_merge_vars, names(df_parm_data))
  
  df_tmp_data <- df_lastdset %>%
    dplyr::left_join(df_parm_data %>% dplyr::select(all_of(c(str_merge_vars, "PARAMCD"))), by = c(str_merge_vars))
  
  # Check for missing PARAMCD
  param_check <- df_tmp_data %>%
    dplyr::select(all_of(str_merge_vars), PARAMCD, PARAM, PARAMLBL) %>%
    dplyr::distinct() %>%
    dplyr::filter(! is.na(!! rlang::sym(paste0(str_domain, "TESTCD"))) & is.na(PARAMCD))
  
  if (nrow(param_check) > 0) {
    message(sprintf("WARNING: No PARAMCD for the following rawdata:\n %s", paste(capture.output(print(param_check)), collapse = "\n")))
  }
  
  df_lastdset <- df_tmp_data 
  
  # Split data based on flaggingsubset
  if (typeof(flaggingsubset) == "character") flaggingsubset <- rlang::parse_expr(flaggingsubset)
  if (! is.null(flaggingsubset)) {
    df_lab1 <- df_lastdset %>% dplyr::filter(!! flaggingsubset)
    df_labu <- df_lastdset %>% dplyr::filter(!eval(flaggingsubset))
  } else {
    df_lab1 <- df_lastdset
    df_labu <- df_lastdset[0, ]
  }
  
  # Handle fasting status
  if (fastedtestcds != "NONE") {
    fasted_codes <- if (fastedtestcds == "ALL") unique(lab1[[paste0(str_domain, "TESTCD")]]) else strsplit(gsub("['\"]", "", fastedtestcds), ",")[[1]]
    df_lab1 <- df_lab1 %>%
      dplyr::mutate(
        PARAMCD = case_when(
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "Y" ~ paste0(trimws(PARAMCD), "F"),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "N" ~ paste0(trimws(PARAMCD), "NF"),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "F" ~ paste0(trimws(PARAMCD), "F"),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "NF" ~ paste0(trimws(PARAMCD), "NF"),
          TRUE ~ PARAMCD
        ),
        PARAM = case_when(
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "Y" ~ paste("Fasting", trimws(PARAM)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "N" ~ paste("Non-Fasting", trimws(PARAM)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "F" ~ paste("Fasting", trimws(PARAM)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "NF" ~ paste("Non-Fasting", trimws(PARAM)),
          TRUE ~ PARAM
        ),
        PARAMLBL = case_when(
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "Y" ~ paste("Fasting", trimws(PARAMLBL)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & toupper(substr(!! rlang::sym(paste0(str_domain, "FAST")), 1, 1)) == "N" ~ paste("Non-Fasting", trimws(PARAMLBL)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "F" ~ paste("Fasting", trimws(PARAMLBL)),
          !! rlang::sym(paste0(str_domain, "TESTCD")) %in% fasted_codes & fastedmissing == "NF" ~ paste("Non-Fasting", trimws(PARAMLBL)),
          TRUE ~ PARAMLBL
        )
      )
  }
  df_lastdset <- df_lab1
  
  # Derive baseline
  if (adbaselnyn) {
    df_tmp_data <- ru_baseline(
      dsetin = df_lastdset,
      rederivebaselineyn = rederivebaselineyn,
      baselineoption = baselineoption,
      baselinetype = baselinetype,
      reldays = reldays,
      startvisnum = startvisnum,
      endvisnum = endvisnum,
      byvars = c("PARAMCD", "PARAM", "STUDYID", "USUBJID")
    )
    df_lastdset <- df_tmp_data
  }
  
  # Combine processed and unprocessed data
  df_lastdset <- dplyr::bind_rows(df_lastdset, df_labu)

  # Derive SHIFT1N/SHIFT1 variables
  if (all(c("ANRIND", "A2INDCD") %in% names(df_lastdset))) {
    df_lastdset <- df_lastdset %>%
      dplyr::mutate(
        SHIFT1N = case_when(
          !A2INDCD %in% c("P", "R") & BNRIND != "" & (toupper(ANRIND) == "NORMAL" | ANRIND == BNRIND) ~ 2,
          !A2INDCD %in% c("P", "R") & BNRIND != "" & toupper(ANRIND) == "LOW" ~ 1,
          !A2INDCD %in% c("P", "R") & BNRIND != "" & toupper(ANRIND) == "HIGH" ~ 3,
          TRUE ~ NA_real_
        ),
        SHIFT1 = case_when(
          SHIFT1N == 2 ~ "To Normal or No Change",
          SHIFT1N == 1 ~ "To Low",
          SHIFT1N == 3 ~ "To High",
          TRUE ~ ""
        )
      )
  }
  
  # Sort data
  # Derive ANL70FL/ANL71FL/ANL72FL
  anl_flags <- df_lastdset %>%
    dplyr::group_by(!!sym(G_SUBJID)) %>%
    dplyr::summarise(
      ANL70FL = if ("ANRIND" %in% names(.)) ifelse(any(toupper(ANRIND) %in% c("HIGH", "LOW")), "Y", " ") else " ",
      ANL71FL = if ("A2INDCD" %in% names(.)) ifelse(any(A2INDCD %in% c("H", "L")), "Y", " ") else " ",
      ANL72FL = if ("A1INDCD" %in% names(.)) ifelse(any(A1INDCD %in% c("H", "L")), "Y", " ") else " "
    ) %>%
    dplyr::select(!!sym(G_SUBJID), ANL70FL, ANL71FL, ANL72FL)
  data <- df_lastdset %>%
    dplyr::left_join(anl_flags, by = G_SUBJID)

  # Derive ANL73FL for ALT and Bilirubin
  if ("A1INDCD" %in% names(df_lastdset)) {
    alt <- df_lastdset %>%
      dplyr::filter(!! rlang::sym(paste0(str_domain, "TESTCD")) == "ALT" & A1INDCD == "H") %>%
      dplyr::select(!!sym(G_SUBJID), ADTM)
    bili <-df_lastdset %>%
      dplyr::filter(!! rlang::sym(paste0(str_domain, "TESTCD")) == "BILI" & PARCAT1 == "CHEM" & A1INDCD == "H") %>%
      dplyr::select(!!sym(G_SUBJID), ADTM)
    anl73fl <- inner_join(alt, bili, by = c(G_SUBJID, "ADTM")) %>%
      dplyr::mutate(ANL73FL = "Y") %>%
      dplyr::select(!!sym(G_SUBJID), ADTM, ANL73FL)
    df_lastdset <- df_lastdset %>%
      dplyr::left_join(anl73fl, by = c(G_SUBJID, "ADTM"))
  }
  
  # Decode variables
  if (decodeyn) {
    df_lastdset <- ru_decode(
      dsetin = df_lastdset,
      codedecodevarpairs = codedecodevarpairs,
      varcodelistpairs=varcodelistpairs,
      codelistnames = codelistnames,
      dsplan = dsplanfile
    )
  }
  
  # Apply attributes
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
