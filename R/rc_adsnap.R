#========================================================================= 
# Author:  Yongwei Wang
# Program: rc_adsnap.R
# Purpose: Create ADSNAP dataset
# Date:    04/18/2025
#========================================================================= 
# Label: Snapshot Analysis Dataset
#
# Input: adsl, DS, CM, MB, SV
#

rc_adsnap <- function(
  dsetinlb=sdtmdata$lb(),
  dsetincm=sdtmdata$cm(),
  dsetinds=sdtmdata$ds(),
  dsetinadsl=G_POPDATA, 
  dsetinvisit=sdtmdata$sv(), 
  hiv1rnacutoff=50, 
  deldays=0, 
  stdwindowweeks=6,
  adslvars=NULL, 
  getadslvarsyn  = TRUE,
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
  keeplabvars=c("LBDTC", "VISIT", "VISITNUM")) {
  
  newvarlabels <- list("ADT"="Analysis Date", "ADY"="Analysis Relative Day", "SVISIT"="Snapshot Analysis Visit",
   "SVISITN"="Snapshot Analysis Visit (N)", "DCSDT"="Date of Study Discontinuation", "DCSREAS"="Reason for Discontinuation from Study",
   "PARAMCD"="Parameter Code", "PARAM"="Parameter", "PERMITFL"="Permitted Background Therapy Change Flag",
   "BKTCHGFL"="Change in Background Therapy Flag", "BKTCHGDT"="Date of Background Therapy Change", "AVALCAT1"="Analysis Value Category 1",
   "AVALC"="Analysis Value (C)", "MCRIT1ML"="Multi-Response Criterion 1 Evaluation", "VLLAST"="Viral Load HIV-1 RNA at End of Window",
   "DCAEDTFL"="Discontinued due to AE or Death Flag", "DCSFL"="Disc due to Lack of Efficacy Flag", "DCVFFL"="Disc due to Lack of Efficacy Flag",
   "VFFL"="Viral Failure Flag", "VLIW"="Categorization of Viral Load in Window", "VLHIVRNA"="Viral Load HIV-1 RNA",
   "DTYPE"="Derivation Type", "APERADY"="Analysis Relative Day in Period", "CRIT1"="Analysis Criterion 1", 
   "CRIT1FL"="Criterion 1 Evaluation Result Flag", "CRIT1FN"="Criterion 1 Evaluation Result Flag (N)", "CRIT2"="Analysis Criterion 1",
   "CRIT2FL"="Criterion 1 Evaluation Result Flag", "CRIT2FN"="Criterion 1 Evaluation Result Flag (N)",
   "MCRIT1MN"="Multi-Response Criterion 1 Evaluation (N)", "MCRIT1"="Analysis Multi-Response Criterion 1", "THRESHD"="Threshold Level")
  
  # 
  # Read in HIV1-RNA, DS and CM data:
  # ADaM PARAMCD: "LB2967", "LB2968", "LB2969", "LB0153"
  # SDTM LBTESTCD: "HIV1RNA", "HIV1RNAI", "HIV1RNAL"
  #
  df_mb <- dsetinlb %>% dplyr::filter(LBTESTCD %in% c("HIV1RNA")) %>% 
    dplyr::mutate(VLHIVRNA = suppressWarnings(as.numeric(if_else(grepl("<", LBSTRESC) & is.na(LBSTRESN), as.numeric(sub("<", "", LBSTRESC)) - 0.0000001, LBSTRESN)))) %>%
    dplyr::filter(!is.na(VLHIVRNA))
  
  df_cm <- dsetincm %>% dplyr::filter(CMCAT=="ANTIRETROVIRAL MEDICATION" & CMSTDTC != "") %>% 
    dplyr::select("STUDYID", "USUBJID", "CMDECOD", "CMSTDTC", "CMSTDY") %>% dplyr::group_by(STUDYID, USUBJID) %>% 
    dplyr::filter(CMSTDY == min(CMSTDY)) %>% dplyr::mutate(seq__=dplyr::row_number()) %>% dplyr::rename(BKTCHGDTC=CMSTDTC)
  
  this_count <- max(df_cm$seq__)
  this_cols <- NULL
  for (i in 1:this_count) {
    this_cols <- c(this_cols, paste0("CMDECOD", i))
  }
  df_cm_1 <- df_cm %>%  tidyr::pivot_wider(names_from = seq__, values_from = CMDECOD,  names_prefix = "CMDECOD") %>% 
    tidyr::unite("CMDECOD", !! this_cols, sep = ", ", remove = TRUE, na.rm = TRUE) %>%
    dplyr::ungroup() 
  
  df_ds <- dsetinds %>% dplyr::filter(DSSCAT %in% c("STUDY TREATMENT DISCONTINUATION", "STUDY CONCLUSION"), ) %>% 
    dplyr::select("STUDYID", "USUBJID", "DSDECOD", "DSSTDTC", "DSSTDY") %>% dplyr::group_by(STUDYID, USUBJID) %>% 
    dplyr::filter(DSSTDY == min(DSSTDY)) %>% dplyr::mutate(seq__=dplyr::row_number()) %>% 
    dplyr::rename(DCSDTC=DSSTDTC, DCSREAS=DSDECOD)
  
  this_count <- max(df_ds$seq__)
  this_cols <- NULL
  for (i in 1:this_count) {
    this_cols <- c(this_cols, paste0("DCSREAS", i))
  }
  df_ds_1 <- df_ds %>%  tidyr::pivot_wider(names_from = seq__, values_from = DCSREAS,  names_prefix = "DCSREAS") %>% 
    tidyr::unite("DCSREAS", !! this_cols, sep = ", ", remove = TRUE, na.rm = TRUE) %>% 
    dplyr::ungroup() 
  

  df_mb_1 <- ru_snapshotvisit(dsetin=df_mb, dsetinadsl=dsetinadsl, dsetinvisit=dsetinvisit, groupbyvars=c("STUDYID", "USUBJID"), stdtvar="LBDTC", endtvar=NULL, stdwindowweeks=stdwindowweeks) %>%
    dplyr::filter(! is.na(SVISIT))
  
  visitwindowvars <- base::setdiff(names(df_mb_1), names(df_mb))
  visitwindowvarlabels <- base::lapply(df_mb_1, base::attr, "label")[c(visitwindowvars)]
  
  #
  # Insert missing SVISIT for each subject
  #
  df_phantom <- df_mb_1 %>% dplyr::select(APERIOD, SVISITN, SVISIT, AWTARGET, AWRANGE) %>% dplyr::filter(! is.na(SVISIT)) %>% dplyr::distinct()
  df_period <- df_mb_1 %>% dplyr::select(USUBJID, STUDYID, APERIOD, TRTAN, TRTPN, TRTA, TRTP, TRTSDT, TRTEDT) %>% dplyr::filter(! is.na(APERIOD)) %>% dplyr::distinct()
  df_subjid <- dsetinadsl %>% dplyr::filter(ITTEFL=="Y") %>% dplyr::select(STUDYID, USUBJID) %>% dplyr::distinct()
  df_subperiod <- base::merge(x=df_subjid, y=df_period, by=c("STUDYID", "USUBJID"), all.x=TRUE, all.y=TRUE)
  df_mb_p <- base::merge(x=df_subperiod, y=df_phantom, by="APERIOD", all.x=TRUE, all.y=TRUE)
  
  df_mb_2 <- base::merge(df_mb_1, df_mb_p, by=c("APERIOD", "STUDYID", "USUBJID", "TRTAN", "TRTPN", "TRTA", "TRTP", "TRTSDT", "TRTEDT", "SVISITN", "SVISIT", "AWTARGET", "AWRANGE"),
                         all.x=TRUE, all.y=TRUE)
  
  #
  # Merge dispostion and first ART date into dataset
  # APERIOD may need to be added for cross-over studies
  #
  df_mb_3 <- base::merge(x=df_mb_2, y=df_ds_1, by=c("STUDYID", "USUBJID"), all.x=TRUE, all.y=TRUE)
  df_mb_4 <- base::merge(x=df_mb_3, y=df_cm_1, by=c("STUDYID", "USUBJID"), all.x=TRUE, all.y=FALSE)
  
  df_mb_4 <- ru_datetime(df_mb_4, includevar=c("LBDTC", "DCSDTC", "BKTCHGDTC")) 

  
  #
  # Derive Snapshot outcome
  #
  # df_mb_5 <- df_mb_4 %>% dplyr::filter(USUBJID=="201585.005482")
  
  # Derive:
  #  ADT="Analysis Date"
  #  ADY="Analysis Relative Day"
  #  SVISIT="Snapshot Analysis Visit"
  #  SVISITN="Snapshot Analysis Visit (N)"
  #  DCSDT="Date of Study Discontinuation"
  #  DCSREAS="Reason for Discontinuation from Study"
  #  PARAMCD="Parameter Code"
  #  PARAM="Parameter"
  #  PERMITFL="Permitted Background Therapy Change Flag"
  #  BKTCHGFL="Change in Background Therapy Flag" 
  #  BKTCHGDT="Date of Background Therapy Change"
  #  AVALCAT1="Analysis Value Category 1"
  #  AVALC="Analysis Value (C)"
  #  MCRIT1ML="Multi-Response Criterion 1 Evaluation"
  #  VLLAST: Last HIV1RNA result in snapshot window (per SVISIT/SVISITN)
  #  DCAEDTFL="Discontinued due to AE or Death Flag"
  #  DCSFL="Disc due to Lack of Efficacy Flag"
  #  DCVFFL="Disc due to Lack of Efficacy Flag"
  #  VFFL="Viral Failure Flag"
  #  VLIW="Categorization of Viral Load in Window"
  #  VLHIVRNA="Viral Load HIV-1 RNA"
  #  DTYPE="Derivation Type"
  #  APERADY=Analysis Relative Day in Period"
  #  
  # Assign: 
  #  CRIT1="Analysis Criterion 1"
  #  CRIT1FL="Criterion 1 Evaluation Result Flag"
  #  CRIT1FN="Criterion 1 Evaluation Result Flag (N)"
  #  CRIT2="Analysis Criterion 1"
  #  CRIT2FL="Criterion 1 Evaluation Result Flag"
  #  CRIT2FN="Criterion 1 Evaluation Result Flag (N)"
  #  MCRIT1MN="Multi-Response Criterion 1 Evaluation (N)"
  #  MCRIT1="Analysis Multi-Response Criterion 1" 
  #  THRESHD="Threshold Level"
  #
  
  df_snap <- df_mb_4 %>%
    dplyr::group_by(STUDYID, USUBJID, APERIOD, SVISITN, SVISIT) %>%
    dplyr::arrange(LBDTC, VLHIVRNA, .by_group=TRUE) %>% dplyr::mutate(lagaval_ = dplyr::lag(VLHIVRNA)) %>%
    dplyr::ungroup()
 
  df_snap <- df_snap %>%   
    dplyr::mutate(
      ADT = LBDT,
      APERADY = as.numeric(as.Date(ADT) - as.Date(TRTSDT) + 1),
      ADY = as.numeric(as.Date(ADT) - as.Date(TRTSDT) + 1),
      DTYPE = if_else(is.na(VLHIVRNA), "PHANTOM", ""),
      ADT_=if_else(is.na(VLHIVRNA) | is.na(LBDT), if_else(is.na(AWRANGE), NA_Date_, as.Date(TRTSDT + days(as.numeric(unlist(strsplit(AWRANGE, split=" "))[5])))), as.Date(LBDT)),
      lagaval_ = dplyr::lag(VLHIVRNA),
      THRESHD=!! hiv1rnacutoff,
      AVALCAT1 = case_when(
        ! is.na(DCSDT) & ADT_ > DCSDT + deldays & (BKTCHGDT >= DCSDT | is.na(BKTCHGDT)) & str_detect("LACK OF EFFICACY", toupper(DCSREAS))  ~ 'Discontinued for lack of efficacy',
        ! is.na(DCSDT) & ADT_ > DCSDT + deldays & (BKTCHGDT >= DCSDT | is.na(BKTCHGDT)) & lagaval_ >= THRESHD ~ 'Discontinued for other reason while not below threshold',
        ! is.na(DCSDT) & ADT_ > DCSDT + deldays & (BKTCHGDT >= DCSDT | is.na(BKTCHGDT)) & str_detect("ADVERSE EVENT", toupper(DCSREAS)) ~ 'Discontinued study due to AE or death',
        ! is.na(DCSDT) & ADT_ > DCSDT + deldays & (BKTCHGDT >= DCSDT | is.na(BKTCHGDT)) ~ 'Discontinued study for other reasons',
        ! is.na(BKTCHGDT) & ADT_ > BKTCHGDT ~ "Change in background therapy",
        is.na(VLHIVRNA) ~ 'On study but missing data in window',
        VLHIVRNA < THRESHD ~ paste0("HIV-1 RNA < ", THRESHD, " copies/mL"),
        VLHIVRNA >= THRESHD ~ paste0("Data in window and HIV-1 RNA >= ",  THRESHD, " copies/mL"),
        TRUE ~ NA
      ),
      AVALC=case_when(
        str_detect(AVALCAT1, paste0("HIV-1 RNA < ", THRESHD, " copies/mL")) ~ "1",
        str_detect(AVALCAT1, paste0("Data in window and HIV-1 RNA >= ", THRESHD, " copies/mL")) ~ "2a",
        str_detect(AVALCAT1, "Discontinued for lack of efficacy") ~ "2b",
        str_detect(AVALCAT1, "Discontinued for other reason while not below threshold") ~ "2c",
        str_detect(AVALCAT1, "Change in background therapy") ~"2d",
        str_detect(AVALCAT1, "Discontinued study due to AE or death") ~ "3a",
        str_detect(AVALCAT1, "Discontinued study for other reasons") ~ "3b",
        str_detect(AVALCAT1, "On study but missing data in window") ~ "3c",
        TRUE ~ ""
      ),
      DCSFL = if_else(AVALC %in% c("2b", "2c", "3a", "3b"), "Y", "N"),
      DCVFFL = if_else(AVALC == "2b", "Y", "N"),
      PERMITFL = if_else(AVALC == "2d", "Y", "N"),
      BKTCHGFL = if_else(AVALC == "2d", "Y", "N"),
      DCAEDTFL = if_else(AVALC == "3a", "Y", "N"),
      MCRIT1="Snapshot Table Value",
      MCRIT1MN=case_when(
        str_detect(AVALC, "1") ~ 1,
        str_detect(AVALC, "2") ~ 2,
        str_detect(AVALC, "3") ~ 3,
        TRUE ~ NA
      ),
      MCRIT1ML=case_when(
        MCRIT1MN == 1 ~ paste0("HIV-1 RNA < ", THRESHD, " copies/mL"),
        MCRIT1MN == 2 ~ paste0("HIV-1 RNA >= ", THRESHD, " copies/mL"),
        MCRIT1MN == 3 ~ "No Virologic Data",
        TRUE ~ NA
      ),
      PARAMCD=paste0("SS", THRESHD),
      PARAM=paste0("Snapshot Status for cut-point of ", THRESHD, " copies/mL"),
      CRIT1=paste0("Response for HIV-1 RNA < ", THRESHD, " copies/mL or no virologic data"),
      CRIT1FN=case_when(
        MCRIT1MN == 1 | MCRIT1MN == 3 ~ 1,
        MCRIT1MN == 2 ~ 2,
        TRUE ~ NA
      ),
      CRIT1FL=case_when(
        CRIT1FN == 1 ~ "Y",
        CRIT1FN == 2 ~ "N",
        TRUE ~ NA
      ),
      CRIT2=paste0("Response for HIV-1 RNA < ", THRESHD, " copies/mL"),
      CRIT2FN=case_when(
        MCRIT1MN == 1 ~ 1,
        MCRIT1MN == 2 | MCRIT1MN == 3 ~ 2,
        TRUE ~ NA
      ),
      CRIT2FL=case_when(
        CRIT2FN == 1 ~ "Y",
        CRIT2FN == 2 ~ "N",
        TRUE ~ NA
      ),    
      VFFL=if_else(VLHIVRNA >= THRESHD, "Y", "N"),
      VLIW=case_when(
        is.na(VLHIVRNA) ~ "Missing",
        VLHIVRNA >= THRESHD ~ paste0("HIV-1 RNA >= ", THRESHD, " copies/mL"),
        TRUE ~ paste0("HIV-1 RNA < ", THRESHD, " copies/mL")
      )
    ) %>% dplyr::group_by(STUDYID, USUBJID, APERIOD, SVISITN, SVISIT) %>%
    dplyr::arrange(desc(MCRIT1MN), LBDTC, .by_group = TRUE) %>%
    dplyr::slice_head(n=1) %>% ungroup() %>% 
    dplyr::mutate(VLLAST=VLHIVRNA) 
  
  df_out <- df_snap %>% dplyr::select(all_of(unique(c("STUDYID", "USUBJID", keeplabvars, visitwindowvars, names(newvarlabels)))))
 
  if (length(adslvars) > 0) {
    df_out <- ru_adgetadslvars(df_out, dsetinadsl, adslvars)
  } else {
    df_out <- ru_labels(df_out, base::lapply(dsetinadsl, base::attr, "label")[c("STUDYID", "USUBJID")])
  }

  if (length(keeplabvars) > 0) {
    df_out <- ru_labels(df_out, base::lapply(df_mb, base::attr, "label")[c(keeplabvars)])
  }   
  
  df_out <- ru_labels(df_out, newvarlabels)
  df_lastdset <- ru_labels(df_out, visitwindowvarlabels )
  
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

