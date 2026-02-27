#========================================================================= 
# Author:  Yongwei Wang
# Program: rc_advf.R
# Purpose: Create ADVF dataset
# Date:    03/18/2025
#========================================================================= 
# Label: Medical History Analysis Dataset
#
# Input: adlb, adsl
#
rc_advf <- function (
  dsetinadlb=adamdata$adlb(), # Input ADLB dataset 
  dsetinadsl=G_POPDATA,  # ADSL dataset 
  threshd=200,           # Threshold level of viral load for viral load failure 
  suppthreshd=200,       # Suppression threshold for rebound. Will be the same as THRESHD if not given 
  adslvars=rlang::exprs(ACOUNTRY, AGE, FASFL, ITTFL, PPROTFL, RACE, SAFFL, SEX, SITEID, TRTAN, TRTPN, TRTSEQA, TRTSEQAN, TRTSEQP, TRTSEQPN),  # List of variables from the dataset specified in &DSETINADSL to be added to dataset by %tu_adgetadslvars 
  cd4paramcd="LB0153",     # Parameter code value for CD4 in &DSETINADLB 
  hiv1rnaparamcd="LB1549", # Parameter code value for HIV1 RNA viral load in &DSETINADLB 
  hiv1vldiparamcd="LB1526", # Parameter code value for HIV1 RNA Indication in &DSETINADLB 
  l10hv1vlparamcd="LB1527", # Parameter code value for Log 10 HIV1 RNA in &DSETINADLB 
  mergebyvars=c("USUBJID", "STUDYID", "AVISITN", "AVISIT", "ADT"), # By-variables to merge CD4, HIV1 RNA, HIV1IND Indication and log 10 HIV1 RNA together 
  ontrtsubset="ONTRTFL=='Y' & ADY > 1", # Condition to decide on-treatment records in &DSETNINADLB. Will be used in WHERE statement 
  wk12wk16avisits=c("WEEK 12", "WEEK 16"), # First (and last) visit(s) of early viralogical failure, which has log 10 decrease from baseline < 1 
  logchgbase=-1,         # The minimum log change from baseline threshold for treatment naive patient.If greater than the threshold, it is SVF
  logchgnadir=1,         # The maximum change from nadir threshold while nadir is above threshd. if greater than the threshold it is CVF.
  respavisits=NULL,      # First (and Last) visit(s) viral response, which has HIV1RNA < 50, should be reached 
  postsubset=NULL,       # Optional SAS code to process any code before final ADaM dataset is written out 
  attributesyn=TRUE,      # Flag to indicate if ru_attrib utility is to be executed  
  misschkyn=TRUE          # Call ru_misschk to print RTWARNING messages for each variable in &DSETOUT which has missing values on all records 
  ) {
  

  subjidvars <- c("STUDYID", "TRT01A", "TRT01AN", "TRT01P", "TRT01PN", "SUBJID", "USUBJID") 
  # derivevars <- c("VFTYPE", "CR4HFL", "CR50FL", "CVFFL", "MCRIT1", "MCRIT1ML", "MCRIT1MN", "PVFFL", "RB4HFL", "RBNADFL", "RB50FL", "NADIRTRT", "CVFAFL", "PPVWFL",  "PVWFL")
  derivevars <- c("VFTYPE", "CR4HFL", "CR50FL", "CVFFL", "MCRIT1", "MCRIT1ML", "MCRIT1MN", "PVFFL", "RB4HFL", "RBNADFL", "RB50FL")
  visrelvars <- c("APERIOD", "APERIODC", "APHASEN", "APHASE")
  trtvars <- c("TRTPN", "TRTP", "TRTAN", "TRTA")
  labresultvars <- c("AVAL", "AVAL_", "AVALC", "BASE", "BASEC", "BASE_", "CHG", "CHG_", "L10AVAL", "L10BASE", "L10CHG")
  
  newvarlabels <- list("MCRIT1"="Analysis Multi-Response Criterion 1", "MCRIT1ML"="Multi-Response Criterion 1 Evaluation", "MCRIT1MN"="Multi-Response Criterion 1 Eval (N)",
  "MCRIT2"="Analysis Multi-Response Criterion 2", "MCRIT2ML"="Multi-Response Criterion 2 Evaluation", "MCRIT2MN"="Multi-Response Criterion 2 Eval (N)",
  "RB4HFL"= "First Rebound after CR4HFL=Y", "RB50FL"="First Rebound after CR50FL=Y", "RBNADFL"="First Rebound from NADIR", "CR50FL"="Confirmed Response <50 c/mL Flag",
  "CR4HFL"="Confirmed Response < 400 c/mL Flag", "VFTYPE"="Virologic Failure Type", "CVFAFL"="Confirmed Virologic Failure Pop Flag", "THRESHD"="Threshold Level",
  "PVFFL"="Possible Virologic Failure Flag", "CVFFL"="Confirmed Virologic Failure Flag", "CD4"="CD4 Value", "CD4CHG"="CD4 Change from Baseline", "L10BASE"="Log 10 of BASE",
  "L10CHG"="Change from L10 Base to L10 Aval", "L10AVAL"="Log 10 of Analysis Value", "HIV1VLDI"="HIV-1 Viral Load Interpretation", "DTYPE"="Derivation Type")

  sysmacroname <- sys.call(0)
  if  (is.null(sysmacroname)) sysmacroname <- ""
  visitdays <- function(visit) {
    visit1 <- toupper(unlist(strsplit(visit, " ")))[1]
    timedays <- case_when(
      visit1 == "WEEK" ~ 7,
      visit1 == "MONTH" ~ 30,
      visit1 == "DAY" ~ 1, 
      TRUE ~ -1
    )
    timen <- as.double(base::gsub("\\D", "", visit)[1])
    days <- timedays * timen
    return(days)
  }
  
  #
  # Get HIV1RNA data and derive L10AVAL, L10BASE, L10CHG if &l10hv1vlparamcd
  # is not given. Assign THRESHD=&THRESHD
  #
  
  df_adlb_1 <- dsetinadlb %>%
    dplyr::mutate(seq__=dplyr::row_number(),
      AVAL_ = suppressWarnings(as.numeric(ifelse(grepl("<", AVALC) & is.na(AVAL), as.numeric(sub("<", "", AVALC)) - 0.0000001, AVAL))),
      BASE_ = suppressWarnings(as.numeric(ifelse(grepl("<", BASEC) & is.na(AVAL), as.numeric(sub("<", "", BASEC)) - 0.0000001, BASE))),
      CHG_ = suppressWarnings(as.numeric(ifelse(is.na(CHG), AVAL_ - BASE_, CHG)))
      )
  
  adlbvars <- colnames(df_adlb_1)
  dropvars <- dplyr::intersect(derivevars, adlbvars)
  subjidvars <- dplyr::intersect(subjidvars, adlbvars)
  trtvars <- dplyr::intersect(trtvars, adlbvars)
  visrelvars <- dplyr::intersect(visrelvars, adlbvars)
  
  debugvars <- c(subjidvars, visrelvars, trtvars, "AVISIT", "AVISITN", "ADT", "ADY", labresultvars)
  
  # Compute log10 values if l10hv1vlparamcd is missing

  # Step 2: Filter the dsetinadlb based on PARAMCD and merge it with the data
  if (!is.null(l10hv1vlparamcd) && l10hv1vlparamcd != "") {
    df_adlb_log <- df_adlb_1 %>%
      dplyr::filter(toupper(PARAMCD) == toupper(!! l10hv1vlparamcd) & !is.na(AVAL_)) %>%
      dplyr::select(all_of(mergebyvars), AVAL_, BASE_, CHG_) %>%
      dplyr::group_by(!!! rlang::syms(mergebyvars)) %>% 
      dplyr::filter(AVAL_==max(AVAL_)) %>% dplyr::distinct() %>%
      dplyr::rename(L10AVAL = AVAL_, L10BASE = BASE_, L10CHG = CHG_) %>%
      dplyr::ungroup()
    
    # Merge the datasets
    df_adlb_1 <- df_adlb_1 %>%
      dplyr::left_join(df_adlb_log, by = mergebyvars)
    
  } else {
    df_adlb_1 <- df_adlb_1 %>%
      dplyr::mutate(
        L10AVAL = suppressWarnings(as.numeric(ifelse(!is.na(AVAL_), log10(AVAL_), NA))),
        L10BASE = suppressWarnings(as.numeric(ifelse(!is.na(BASE_), log10(BASE_), NA))),
        L10CHG = suppressWarnings(as.numeric(ifelse(!is.na(L10AVAL) & !is.na(L10BASE), L10AVAL - L10BASE, NA)))
      )
  }
  
  # Add threshold variable
  df_adlb_1 <- df_adlb_1 %>%
    dplyr::mutate(THRESHD = !! threshd, logchgbase_ =NA_real_, logchgnadir_=NA_real_, suppthreshd_= !!threshd)
  
  if (! is.null(logchgbase)) {
    df_adlb_1 <- df_adlb_1 %>%
      dplyr::mutate(logchgbase_ = !! logchgbase)    
  }
  
  if (! is.null(logchgnadir)) {
    df_adlb_1 <- df_adlb_1 %>%
      dplyr::mutate(logchgnadir_ = !! logchgnadir)    
  }
  
  if (! is.null(suppthreshd)) {
    df_adlb_1 <- df_adlb_1 %>%
      dplyr::mutate(suppthreshd_ = !! suppthreshd)    
  }  
  
  #
  # Drop CR4HFL CR50FL CVFFL MCRIT1 MCRIT1ML MCRIT1MN PVFFL RB4HFL RBNADFL
  # RB50FL NADIRTRT CVFAFL PPVWFL PVWFL if they are in HIV1RNA dataset
  #
  if (!is.null(dropvars) && length(dropvars) > 0) {
    df_adlb_1 <- df_adlb_1 %>% dplyr::select(-all_of(dropvars))
  }
  
  #
  # If &l10hv1vlparamcd is given, merge AVAL_, BASE_, CHG_ with
  # PARAMCD=&l10hv1vlparamcd into HIV1RNA data and rename them to L10AVAL,
  # L10BASE, L10CHG by &MERGEBYVARS.
  # Step 1: Sort the data by the specified merge variables
  
  df_adlb_1_1 <- df_adlb_1 %>% dplyr::filter(!is.na(AVAL_)) %>%
    dplyr::filter(toupper(PARAMCD) == toupper( !! hiv1rnaparamcd))
  
  # Get Minimum/Maximum of AVISITN for AVISIT in &RESPAVISITS if given.
  # Get Minimum/Maximum of AVISITN for AVISIT in &wk12wk16avisits if given.
  if (!is.null(ontrtsubset) && length(ontrtsubset) > 0) {
    df_adlb_2 <- df_adlb_1_1 %>%
      dplyr::filter(eval(parse(text = ontrtsubset)))
  } else {
    df_adlb_2 <- df_adlb_1_1
  }
  
  #
  #
  if (! is.null(wk12wk16avisits) && length(wk12wk16avisits)) {
    wk12wk16days <- sapply(wk12wk16avisits, visitdays)
    minwk12wk16days <- suppressWarnings(min(wk12wk16days))
    maxwk12wk16days <- suppressWarnings(max(wk12wk16days))
    temp_data <- df_adlb_2 %>%
      dplyr::filter(toupper(AVISIT) %in% !! toupper(wk12wk16avisits)) %>%
      dplyr::group_by(across(all_of(c(visrelvars, subjidvars)))) %>%
      dplyr::summarise(
        min_wk12day = suppressWarnings(min(ADY, na.rm = TRUE)),
        max_wk16day = suppressWarnings(max(ADY, na.rm = TRUE))
      )
    
    df_adlb_2 <- df_adlb_2 %>%
      dplyr::left_join(temp_data, by = c(visrelvars, subjidvars)) %>% 
      dplyr::mutate(
        max_wk16day = suppressWarnings(as.numeric(ifelse(min_wk12day == max_wk16day | is.na(max_wk16day) | max_wk16day < !! maxwk12wk16days, !! maxwk12wk16days, max_wk16day))),
        min_wk12day = suppressWarnings(as.numeric(ifelse(min_wk12day == max_wk16day | is.na(min_wk12day) | min_wk12day > !! minwk12wk16days, !! minwk12wk16days, min_wk12day))),
        WK12WK16FL_ = suppressWarnings(as.character(ifelse(ADY >= min_wk12day & ADY <= max_wk16day, "Y", "N"))))
  } else {
    df_adlb_2 <- df_adlb_2 %>%  dplyr::mutate(WK12WK16FL_="N", min_wk12day=NA, max_wk16day=NA)
  }
  
  if (! is.null(respavisits) && length(respavisits)) {
    respdays <- sapply(respavisits, visitdays)
    minrespdays <- suppressWarnings(min(respdays))
    minrespdays <- suppressWarnings(max(respdays))
    temp_data <- df_adlb_2 %>%
      dplyr::filter(toupper(AVISIT) %in% !! toupper(respavisits)) %>%
      dplyr::group_by(across(all_of(c(visrelvars, subjidvars)))) %>%
      dplyr:summarise(
        min_respday = suppressWarnings(min(ADY, na.rm = TRUE)),
        max_respday = suppressWarnings(max(ADY, na.rm = TRUE))
      ) 
    
    df_adlb_2 <- df_adlb_2 %>%
      dplyr::left_join(temp_data, by = c(visrelvars, subjidvars)) %>% 
      dplyr::mutate(
        max_respday = suppressWarnings(as.numeric(ifelse(min_respday == max_respday | is.na(max_respday) | max_respday < !! maxrespdays, !! maxrespdays, max_respday))),
        min_respday = suppressWarnings(as.numeric(ifelse(min_respday == max_respday | is.na(min_respday) | min_respday < !! minrespdays, !! minrespdays, min_respday))),
        RESPWFL_=as.character(ifelse(ADY >= min_respday & ADY <= max_respday, "Y", "N")))
  } else {
    df_adlb_2 <- df_adlb_2 %>%  dplyr::mutate(RESPWFL_="N", max_respday=NA, min_respday=NA)
  }
  
  
  # Step 3: Apply the ontrtsubset condition if provided
  if (!is.null(ontrtsubset) && nchar(ontrtsubset) > 0) {
    df_adlb_2 <- df_adlb_2 %>%
      dplyr::filter(eval(parse(text = ontrtsubset)))
  }
  
  # Start to create VF dataset:
  # Derive PVFFL:
  # 1. If &ONTRTSUBSET, subste HIV1RNA data with &ONTRTSUBSET
  # 2. If more than one HIV1RNA values, use the maximum one
  # 3. If AVISITN between Minimum/Maximum of AVISITN of &wk12wk16avisits
  #    and L10CHG > -1, assign PVFFL=Y
  # 4. If AVISITN > Maximum of AVISITN of &wk12wk16avisits, and HIV1RNA value
  #    > THRESHD, assign PVFFL=Y
  
  # Step 1: Sort data by specified variables in descending order of AVAL_ and AVISITN
  # Step 2: Handle the case of multiple results on the same date (keep highest value)
  df_adlb_2 <- df_adlb_2 %>%
    dplyr::group_by(across(all_of(c(visrelvars, subjidvars)))) %>%
    dplyr::arrange(ADT, desc(AVAL_), .by_group = TRUE) %>%
    dplyr::mutate(
      NADIR_ = base::cummin(replace(AVAL_, is.na(AVAL_), Inf)),
      L10CHGNADIR_=L10AVAL - log(NADIR_),
      PADT_ = dplyr::lag(ADT)
    ) %>% dplyr::ungroup()
  
  df_adlb_2 <- df_adlb_2 %>% 
    dplyr::mutate(
      PVFFL = case_when(
        is.na(AVAL_) ~ NA_character_,
        WK12WK16FL_ =="Y" &  min_wk12day < ADY ~ "N",
        WK12WK16FL_ =="Y" &  AVAL_ < THRESHD ~ "N",
        WK12WK16FL_ =="Y" &  max_wk16day <= ADY & min_wk12day >= ADY & L10CHG >= logchgbase_ ~ "Y",
        WK12WK16FL_ =="Y" &  max_wk16day <= ADY & min_wk12day >= ADY & (L10CHG < logchgbase_ | is.na(L10CHG)) ~ "N",
        RESPWFL_ =="Y" &  max_respday <= ADY & min_respday >= ADY & AVAL_ >= suppthreshd_ ~ "Y",
        RESPWFL_ =="Y" &  max_respday <= ADY & min_respday >= ADY & AVAL_ < suppthreshd_ ~ "N",
        AVAL_ >= THRESHD ~ "Y",
        TRUE ~ "N"
        ) 
      ) 

  # Step 1: Get next AVAL_ and L10CHG
  df_adlb_2 <- df_adlb_2 %>%
    dplyr::group_by(across(all_of(c(visrelvars, subjidvars)))) %>%
    dplyr::arrange(desc(ADT), desc(AVAL_), .by_group = TRUE) %>%
    dplyr::mutate(
      NAVAL_ = dplyr::lag(AVAL_),
      NL10CHG_ = dplyr::lag(L10CHG),
      NPVFFL_ = dplyr::lag(PVFFL),
      NL10CHGNADIR_ = dplyr::lag(L10CHGNADIR_),
      VFTYPE = NA_character_,
      CVFFL = NA_character_,
      CRFL_ = NA_character_,
      RBFL_ = NA_character_,
      CR50FL = NA_character_,
      CR4HFL = NA_character_,
      RB50FL = NA_character_,
      RBNADFL = NA_character_,
      RB4HFL = NA_character_,
      MCRIT1ML = NA_character_,
      MCRIT1MN = NA_real_,
      MCRIT1 =NA_character_
    ) %>% dplyr::mutate(
      CVFFL = as.character(ifelse(PVFFL == "Y" & NPVFFL_ == "Y", "Y", NA_character_)),
      VFTYPE = as.character(ifelse(PVFFL == "Y" & NPVFFL_ == "Y", "Suspected", NA_character_)),
      CRFL_ = as.character(ifelse(!is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < 50 & NAVAL_ < 50, "Y", NA_character_)),
      CR50FL = as.character(ifelse(!is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < 50 & NAVAL_ < 50, "Y", NA_character_)),
      CR4HFL = as.character(ifelse(!is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < 400 & NAVAL_ < 400, "Y", NA_character_))
    ) %>% dplyr::ungroup()

  # Step 2: Derive flags (e.g., CVFFL, CR4HFL, RB50FL, etc.)
  # 
  df_adlb_3 <- df_adlb_2 %>%
    dplyr::group_by(across(all_of(c(visrelvars, subjidvars)))) %>%
    dplyr::arrange(ADT, desc(AVAL_), .by_group = TRUE) %>%
    dplyr::mutate(
      # CVFFL Logic
      CVFFL = case_when(
        is.na(dplyr::lag(CVFFL)) & PVFFL == "Y" & NPVFFL_ == "Y" ~ "Y",
        dplyr::lag(CVFFL) %in% c("Y", "M") & !is.na(AVAL_) & PVFFL == "N" ~ NA_character_,
        CVFFL == "Y" ~ "M",
        TRUE ~ dplyr::lag(CVFFL)
      ),
      
      # VFTYPE Logic
      VFTYPE = case_when(
        is.na(dplyr::lag(VFTYPE)) & PVFFL == "Y" & NPVFFL_ == "Y" ~ "Suspected",
        dplyr::lag(VFTYPE) == "Suspected" & !is.na(AVAL_) ~ "Confirmed",
        dplyr::lag(VFTYPE) == "Confirmed" ~ "M",
        TRUE ~ dplyr::lag(VFTYPE)
      ),

      # _CRFL Logic
      CRFL_ = case_when(
        is.na(dplyr::lag(CRFL_)) & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < suppthreshd & NAVAL_ < suppthreshd ~ "Y",
        dplyr::lag(CRFL_) == "Y" ~ "M",
        TRUE ~ dplyr::lag(CRFL_)
      ),

      # CR50FL Logic
      CR50FL = case_when(
        is.na(dplyr::lag(CR50FL)) & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < 50 & NAVAL_ < 50 ~ "Y",
        dplyr::lag(CR50FL) == "Y" ~ "M",
        TRUE ~ dplyr::lag(CR50FL)
      ),
      
      # CR4HFL Logic
      CR4HFL = case_when(
        is.na(CR4HFL) & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ < 400 & NAVAL_ < 400 ~ "Y",
        dplyr::lag(CR4HFL) == "Y" ~ "M",
        TRUE ~ dplyr::lag(CR4HFL)
      ),
      RBFL_ = as.character(ifelse((CRFL_ %in% c("M", "Y") | (BASE_ < threshd & !is.na(BASE_))) & !is.na(AVAL_) &
        !is.na(NAVAL_) & AVAL_ >= threshd & NAVAL_ >= threshd, "Y", NA_character_)),
      RB4HFL = as.character(ifelse(CR4HFL %in% c("M", "Y") & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ >= 400 & NAVAL_ >= 400, "Y", NA_character_)),
      RB50FL = as.character(ifelse(CR50FL %in% c("M", "Y") & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ >= 50 & NAVAL_ >= 50, "Y", NA_character_))
    ) %>% dplyr::mutate(      
      # _RBFL Logic
      RBFL_ = case_when(
        (CRFL_ %in% c("M", "Y") | (BASE_ < threshd & !is.na(BASE_))) & RBFL_=="Y" & !is.na(AVAL_) & 
        !is.na(NAVAL_) & AVAL_ >= threshd & NAVAL_ >= threshd ~ "Y",
        dplyr::lag(RBFL_) == "Y" ~ "M",
        dplyr::lag(RBFL_) == "M" ~ "N",
        TRUE ~ dplyr::lag(RBFL_)
      ),     
      
      # RB50FL Logic
      RB50FL = case_when(
        CR50FL %in% c("M", "Y") & is.na(dplyr::lag(RB50FL)) & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ >= 50 & NAVAL_ >= 50 ~ "Y",
        dplyr::lag(RB50FL) == "Y" ~ "M",
        dplyr::lag(RB50FL) == "M" ~ "N",
        TRUE ~ dplyr::lag(RB50FL)
      ),
      
      # RB4HFL Logic
      RB4HFL = case_when(
        CR4HFL %in% c("M", "Y") & is.na(dplyr::lag(RB4HFL)) & !is.na(AVAL_) & !is.na(NAVAL_) & AVAL_ >= 400 & NAVAL_ >= 400 ~ "Y",
        RB4HFL == "Y" ~ "M",
        RB4HFL == "M" ~ "N",
        TRUE ~ dplyr::lag(RB4HFL)
      ),  
      
      # Setting CR4HFL to 'N' when RB4HFL is 'Y'
      CR4HFL = case_when(
        RB4HFL == "Y" ~ "N",
        TRUE ~ CR4HFL
      ),

      # RBNADFL Logic
      RBNADFL = case_when(
        RB50FL %in% c("M", "Y") ~ RB50FL,
        is.na(RBNADFL) & !is.na(L10CHGNADIR_) & !is.na(NL10CHGNADIR_) & L10CHGNADIR_ >= 1 & NL10CHGNADIR_ >= 1 ~ "Y",
        dplyr::lag(RBNADFL) == "Y" ~ "M",
        dplyr::lag(RBNADFL) == "M" ~ "N",
        TRUE ~ dplyr::lag(RBNADFL)
      ),

      # Final flag logic for MCRIT1, MCRIT1MN, MCRIT1ML
      MCRIT1 = case_when(
        VFTYPE == "Suspected" ~ "CVF Category",
        TRUE ~ NA_character_
      ),
      
      MCRIT1MN = case_when(
        VFTYPE == "Suspected" & RBFL_ == "Y" ~ 1,
        VFTYPE == "Suspected" & RBFL_ != "Y" ~ 2,
        TRUE ~ NA_real_
      ),
      
      MCRIT1ML = case_when(
        VFTYPE == "Suspected" & RBFL_ == "Y" ~ "Rebound",
        VFTYPE == "Suspected" & RBFL_ != "Y" ~ "Virologic Non-response",
        TRUE ~ NA_character_
      )
    ) %>% 
    dplyr::ungroup()
  

  
  # Find if APERIOD, APERIODC, APHASE, and APHASEN depend on AVISIT (their
  # values do not change in the same AVISIT).
  # Initialize a list to store the dependent variables
  l_vars <- c()
  
  # Loop through each variable in visrelvars
  for (l_var in visrelvars) {
    
    # Sort the data by AVISITN, AVISIT, and the variable l_var
    sorted_data <- df_adlb_3 %>%
      dplyr::filter(!is.na(AVISITN) & AVISITN == floor(AVISITN)) %>%
      arrange(AVISITN, AVISIT, !!sym(l_var)) %>%
      dplyr::select(AVISITN, AVISIT, !!sym(l_var)) %>%
      dplyr::distinct()
    
    # Check if any variable values change within the same AVISIT
    grouped_data <- sorted_data %>%
      dplyr::group_by(AVISITN, AVISIT) %>%
      dplyr::summarise(
        change_check = n_distinct(!!sym(l_var)) > 1
      ) %>%
      dplyr::ungroup()
    
    # If there's any change within the same AVISIT, set flag to N
    if (! any(grouped_data$change_check)) {
      l_vars <- c(l_vars, l_var)
    } 
  }
  
  visrelvars <- l_vars
  
  # Create a data with all visits per PARAMC/PARAM/AVISITN/AVISIT and visit
  # related variables.  
  # Assuming `l_prefix.pvf` is your dataset, visrelvars and subjidvars are character vectors
  # of variable names.
  
  # Step 1: Sort the data by AVISITN, AVISIT, and other relevant variables, then remove duplicates
  # for the first table
  vis_data <- df_adlb_3 %>%
    dplyr::filter(!is.na(AVISITN) & AVISITN == floor(AVISITN)) %>%
    dplyr::arrange(AVISITN, AVISIT, across(all_of(visrelvars))) %>%
    dplyr::distinct(AVISITN, AVISIT, across(all_of(visrelvars)), .keep_all = TRUE) %>%
    dplyr::select(PARAMCD, PARAM, AVISITN, AVISIT, all_of(visrelvars))
  
  # Step 2: Create a unique list of subjects
  subj_data <- df_adlb_3 %>%
    dplyr::distinct(across(all_of(subjidvars))) %>%
    dplyr::select(all_of(subjidvars))
  
  # Step 3: Perform a full join to create the "fillvis" table
  fillvis_data <- base::merge(x=subj_data, y=vis_data, all.x=TRUE, all.y=TRUE)
  
  # Find if TRTPN, TRTP, TRTAN, and TRTA is depending on AVISIT. If Yes,
  # Merge them into all visits dataset by subjet and AVISIT
  
  # Initialize variables
  l_vars <- c()

  # Iterate over treatment variables
  for (l_i in seq_along(trtvars)) {
    l_var <- trtvars[l_i]
    
    # Step 1: Sort the data by subject ID, visit-related variables, and treatment variable
    trt_data <- df_adlb_3 %>%
      dplyr::select(all_of(c(subjidvars, visrelvars, l_var))) %>%
      dplyr::distinct(across(all_of(c(subjidvars, visrelvars, l_var))))

    # Step 2: Check if there are duplicates (first and last check per subject and visit)
    trtchk_data <- trt_data %>%
      dplyr::group_by(across(all_of(c(subjidvars, visrelvars)))) %>%
      dplyr::summarise(
        change_check = n_distinct(!!sym(l_var)) > 1
      ) %>%
      dplyr::ungroup()
    
    if (! any(trtchk_data$change_check)) {
      l_vars <- c(l_vars, l_var)
    } 
  }
  trtvars <- l_vars
    
  if (length(trtvars) > 0) {
    alltrts_data <- df_adlb_3 %>%
      dplyr::select(all_of(c(subjidvars, visrelvars, trtvars))) %>%
      dplyr::distinct(across(all_of(c(subjidvars, visrelvars, trtvars))))
      
    # Step 4: Merge fillvis and alltrts data
    fillvis_data <- base::merge(x=fillvis_data, y=alltrts_data, by = c(subjidvars, visrelvars), all.x=TRUE, all.y=FALSE)
  }
  
  # Merge missing visits to VF data. For filled AVISIT, assign DTYPE=PHANTOM  
  # Sort the data by subject ID, visit number, visit, and treatment variables
  
  # Step 1: Merge missing visits with the 'fillvis' data
  df_adlb_4 <- base::merge(x=df_adlb_3, y=fillvis_data, 
                         by = c(subjidvars, "PARAMCD", "PARAM", "AVISITN", "AVISIT", visrelvars, trtvars),
                         all.x=TRUE, all.y=TRUE) %>%
    dplyr::mutate(DTYPE_ = if_else(is.na(seq__), "PHANTOM", NA_character_))

  # Step 2: Derive MCRIT2, MCRIT2MN, MCRIT2ML by carrying forward values from MCRIT1 if not missing
  df_adlb_4 <- df_adlb_4 %>%
    dplyr::group_by(across(all_of(c(subjidvars, "PARAMCD", "PARAM", "AVISITN", "AVISIT", visrelvars, trtvars)))) %>%
    dplyr::mutate(MCRIT2 = dplyr::coalesce(MCRIT1),     # Carry forward MCRIT1 if MCRIT2 is NA
           MCRIT2MN = dplyr::coalesce(MCRIT1MN),  # Same for MCRIT2MN
           MCRIT2ML = dplyr::coalesce(MCRIT1ML)) %>%
    dplyr::ungroup()
  
  #
  # Merge derived variables to HIV1RNA data.
  
  # Sort data by seq__
  # Merge data
  df_adlb_5.1 <- df_adlb_4 %>% dplyr::filter(is.na(DTYPE_) | DTYPE_=="") %>%
    dplyr::select(all_of(c("seq__", derivevars)))
  df_adlb_5.2 <- df_adlb_4 %>% dplyr::filter(! (is.na(DTYPE_) | DTYPE_=="")) %>% 
    dplyr::mutate(DTYPE = DTYPE_)
  df_adlb_5.3 <- base::merge(df_adlb_1_1, df_adlb_5.1, by = "seq__", all.x = TRUE) 
  # 
  # Keep derived variables: "L10AVAL",  "L10BASE", "L10CHG","THRESHD", "PVFFL",
  #   "CVFFL", "VFTYPE", "CR50FL", "RB50FL", "RBNADFL", "CR4HFL", "RB4HFL",       
  #   "MCRIT1ML", "MCRIT1MN", "MCRIT1", "MCRIT1MN", "MCRIT2", "MCRIT2MN",
  #   "MCRIT2ML"
  # Drop others.
  df_adlb_5 <- ru_setdata(df_adlb_5.3, df_adlb_5.2, keeprownames = FALSE) %>%
    dplyr::select(-all_of(c("seq__", "logchgbase_", "logchgnadir_", "suppthreshd_", "min_wk12day", 
      "max_wk16day", "WK12WK16FL_", "RESPWFL_", "max_respday", "min_respday", "NADIR_",  
      "L10CHGNADIR_", "PADT_", "NAVAL_", "NL10CHG_", "NPVFFL_", "NL10CHGNADIR_",  "CRFL_",       
      "RBFL_",  "DTYPE_")))

  
  # Derive CVFAFL
  df.cvfafl <- df_adlb_5 %>%
    dplyr::filter(CVFFL == 'Y') %>%
    dplyr::select(all_of(subjidvars)) %>%
    dplyr::distinct() %>% dplyr::mutate(CVFAFL = "Y")
  
  # Merge and derive CVFAFL variable
  df.wcvfafl <- base::merge(df_adlb_5, df.cvfafl, by = subjidvars, all.x = TRUE)
  
  # Merge in CD4/HIV1VLDI by &MERGEBYVARS if &cd4paramcd/&hiv1vldiparamcd
  # is given.  
 
  
  # If cd4paramcd is provided, process it
  if (nzchar(cd4paramcd)) {
    # Sort the dataset by mergebyvars and filter for cd4paramcd
    cd4_data <- df_adlb_1 %>%
      dplyr::filter(toupper(PARAMCD) == toupper(cd4paramcd)) %>%
      dplyr::select(all_of(c(mergebyvars, "AVAL_", "CHG_"))) %>%
      dplyr::rename(CD4 = AVAL_, CD4CHG = CHG_) 
    
    # If an error occurs (just as in SAS where SYSERR > 0), return an error message
    df.wcvfafl <- base::merge(df.wcvfafl, cd4_data, by = mergebyvars, all.x = TRUE)
  }
  
  # If hiv1vldiparamcd is provided, process it
  if (nzchar(hiv1vldiparamcd)) {
    # Sort the dataset by mergebyvars and filter for hiv1vldiparamcd
    hiv1vldi_data <- df_adlb_1 %>%
      dplyr::filter(toupper(PARAMCD) == toupper(hiv1vldiparamcd)) %>%
      dplyr::select(all_of(c(mergebyvars, "AVALC"))) %>%
      dplyr::rename(HIV1VLDI = AVALC)
    
      df.wcvfafl <- base::merge(df.wcvfafl, hiv1vldi_data, by = mergebyvars, all.x = TRUE)
  }
  
  # Call ru_adgetadslvars to add in ADSL variables listed in ADSLVARS  
  
  # Assign PARAMCD/PARAM
  
  # Assuming l_workdata is already loaded as a data frame
  
  # Create a new data frame with calculated categories and conditions
  df_param <- df.wcvfafl %>%
    dplyr::mutate(
      AVALCAT1 = case_when(
        is.na(AVAL_) ~ NA_character_,
        AVAL_ < 1000 ~ '<1000',
        AVAL_ < 100000 ~ '1000-<10,000',
        TRUE ~ '>=10,000'
      ),
      AVALCA1N = case_when(
        is.na(AVAL_) ~ NA_integer_,
        AVAL_ < 1000 ~ 1,
        AVAL_ < 100000 ~ 2,
        TRUE ~ 3
      )
    ) %>%
    dplyr::filter(!is.na(AVAL_))  # Remove rows with missing AVAL_
  
  # Create new rows for 'Confirmed' and 'Suspected' VFTYPE
  confirmed_vf <- df.wcvfafl %>%
    dplyr::filter(VFTYPE == 'Confirmed') %>%
    dplyr::mutate(
      PARAMCD = 'SVF',
      PARAM = 'Suspected Virological Failure HIV-1 RNA (copies/mL)'
    )
  
  suspected_vf <- df.wcvfafl %>%
    dplyr::filter(VFTYPE == 'Suspected') %>%
    dplyr::mutate(
      PARAMCD = 'CVF',
      PARAM = 'Confirmed Virological Failure HIV-1 RNA (copies/mL)'
    )
  
  # Combine the original data with the new rows
  df_param <- dplyr::bind_rows(df.wcvfafl, confirmed_vf, suspected_vf)
  
  df_param <- ru_labels(df_param, newvarlabels)
  df_lastdset <- ru_labels(df_param, base::labels(dsetinadlb))
  

  # Apply attributes
  if (attributesyn) {
    if (file.exists(toString(dsplanfile))) {
      df_lastdset <- ru_attrib(df_lastdset, dsplanfile)
    } else {
      message("rc_adds: ATTRIBUTESYN is Y, but data plan file given by DSPLANFILE is either NULL or does not exist")
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
  
  # CAll %tu_tidyup to delete temporary datasets used in this macro.
  return(df_lastdset)
}

