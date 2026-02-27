#=============================================================================== 
# Author:  Yongwei Wang
# Program: ru_aperiod.R
# Purpose: Derive APERIOD based on TRxxSDT/TRxxEDT/TRTxxA/TRTxxP
#          For a partial date, one record may fall into more than one
#         period.
# Parameters:
#   SLICE: If a period is only based on start date (STDTVAR) and a record 
#          falling into more than one period, which period to choose?
#          valid value NULL/first/last
# Date:    08/23/2024
#=============================================================================== 

ru_aperiod <- function(dsetin, dsetinadsl=G_POPDATA, groupbyvars=c("STUDYID", "USUBJID"),
  eventtype=c("planned", "spontaneous"), stdtvar=NULL, endtvar=NULL, reldays=0, sliceaperiod=NULL ) {
  print(paste0("RU_APERIOD: ", "Start or RU_APERIOD"))
  
  eventtype <- tolower(substring(eventtype[1], 1, 2))
  df_dsetin <- dsetin
  
  newvarlabels <- list("TRTA"="Actual Treatment", "TRTAN"="Actual Treatment (N)", "TRTP"="Planned Treatment",
                       "TRTPN"="Planned Treatment (N)", TRTSDT="Date of First Exposure to Treatment",
                       "TRTEDT"="Date of Last Exposure to Treatment", "APERIOD"="Actual Period")
  
  #
  # Add row number and keep only variables which will be used to derive visit 
  #
  df_dsetin_1 <- dplyr::mutate(df_dsetin, seqap__=dplyr::row_number())
  df_dsetin_2 <- df_dsetin_1 %>% dplyr::select(all_of(c(groupbyvars, "seqap__", stdtvar, endtvar)))

  #
  # Get ADSL treatment variables (TRTxxA, TRTxxP, TRxxSDT, TRxxEDT)
  # and merge them into DSETIN create above.
  #
  str_adslvars <- names(dsetinadsl)
  str_trtavars <- str_adslvars[grepl("^TRT[0-9]+A$", str_adslvars, ignore.case = TRUE)]
  str_trtpvars <- str_adslvars[grepl("^TRT[0-9]+P$", str_adslvars, ignore.case = TRUE)]
  str_trtanvars <- str_adslvars[grepl("^TRT[0-9]+AN$", str_adslvars, ignore.case = TRUE)]
  str_trtpnvars <- str_adslvars[grepl("^TRT[0-9]+PN$", str_adslvars, ignore.case = TRUE)]
  if (is.null(str_trtavars) || str_trtavars[1] == "") str_trtavars <- str_trtpvars  
  
  str_trsdtvars <- NULL
  if (eventtype == "pl") {
    str_trsdtvars <- str_adslvars[grepl("^AP[0-9]+SDT$", str_adslvars, ignore.case = TRUE)]
    str_tredtvars <- str_adslvars[grepl("^AP[0-9]+EDT$", str_adslvars, ignore.case = TRUE)]
    str_trstmvars <- str_adslvars[grepl("^AP[0-9]+STM$", str_adslvars, ignore.case = TRUE)]
    str_tretmvars <- str_adslvars[grepl("^AP[0-9]+ETM$", str_adslvars, ignore.case = TRUE)]    
  } 
  if (eventtype == "sp" | is.null(str_trsdtvars)) {  
    str_trsdtvars <- str_adslvars[grepl("^TR[0-9]+SDT$", str_adslvars, ignore.case = TRUE)]
    str_tredtvars <- str_adslvars[grepl("^TR[0-9]+EDT$", str_adslvars, ignore.case = TRUE)]
    str_trstmvars <- str_adslvars[grepl("^TR[0-9]+STM$", str_adslvars, ignore.case = TRUE)]
    str_tretmvars <- str_adslvars[grepl("^TR[0-9]+ETM$", str_adslvars, ignore.case = TRUE)]
  }
  
  str_otheradslvars <- NULL
  if (length(str_trsdtvars) < 1 & "TRTSDT" %in% str_adslvars) str_otheradslvars <- "TRTSDT"
  if (length(str_trsdtvars) < 1 & "TRTEDT" %in% str_adslvars) str_otheradslvars <- c(str_otheradslvars, "TRTEDT")

  df_thisadsl <- dsetinadsl %>% dplyr::select(all_of(unique(c(groupbyvars, str_trtavars, str_trtpvars, str_trsdtvars, str_tredtvars, str_trtanvars, str_trtpnvars, str_otheradslvars))))
  df_dsetin_3 <- dplyr::left_join(df_dsetin_2, df_thisadsl, by=groupbyvars, relationship = "many-to-many") %>% 
    dplyr::mutate(seqap__=row_number())
  
  #
  # Loop over each period to add Visits in. If both start date and end date are given,
  # a record can be in multiple visits and the record will be duplicated multiple times
  # in that case.
  #
  df_drvadtrt <- NULL
  if (is.null(endtvar)) endtvar <- "NA"
  if (is.null(stdtvar)) stdtvar <- "NA"
  
  for (i in 1:length(str_trtavars)) {
    str_aperiod <- stringr::str_extract(str_trtavars[i], "[0-9]+")
    str_trtavar <- str_trtavars[i]
    str_trtpvar <- str_trtpvars[grepl(str_aperiod, str_trtpvars, ignore.case = TRUE)]
    if (length(str_trtpvar) < 1) str_trtpvar <- "NA"

    str_trtanvar <- str_trtanvars[grepl(str_aperiod, str_trtanvars, ignore.case = TRUE)]
    if (length(str_trtanvar) < 1) str_trtanvar <- "NA"
    
    str_trtpnvar <- str_trtpnvars[grepl(str_aperiod, str_trtpnvars, ignore.case = TRUE)]
    if (length(str_trtpnvar) < 1) str_trtpnvar <- "NA"
    
    str_trsdtvar <- str_trsdtvars[grepl(str_aperiod, str_trsdtvars, ignore.case = TRUE)]
    if (length(str_trsdtvar) < 1) str_trsdtvar <- "NA"
    
    str_trstmvar <- str_trstmvars[grepl(str_aperiod, str_trstmvars, ignore.case = TRUE)]
    if (length(str_trstmvar) < 1) str_trstmvar <- "NA"
    
    str_tredtvar <- str_tredtvars[grepl(str_aperiod, str_tredtvars, ignore.case = TRUE)]
    if (length(str_tredtvar) < 1) str_tredtvar <- "NA"
    
    str_tretmvar <- str_tretmvars[grepl(str_aperiod, str_tretmvars, ignore.case = TRUE)]
    if (length(str_tretmvar) < 1) str_tretmvar <- "NA"
    
    
    n_aperiod <- as.numeric(str_aperiod)
    if (i < length(str_trtavars)) n_reldays = 0 else n_reldays = reldays
    
    if (length(str_trtavars) == 1 & "TRTSDT" %in% str_otheradslvars) { str_trsdtvar <- "TRTSDT"}
    if (length(str_trtavars) == 1 & "TRTEDT" %in% str_otheradslvars) { str_tredtvar <- "TRTEDT"}
    
    df_drvadtrt_1 <- df_dsetin_3 %>% dplyr::mutate(
      STDTC_ = ifelse(!! stdtvar != "NA" , ifelse(is.character(!! rlang::sym(stdtvar)), !! rlang::sym(stdtvar), 
                                                     format(as.Date(!! rlang::sym(stdtvar)), "%Y-%m-%d")), NA_character_),
      ENDTC_ = ifelse(!! endtvar != "NA", ifelse(is.character(!! rlang::sym(endtvar)), !! rlang::sym(endtvar), 
                                                     format(as.Date(!! rlang::sym(endtvar)), "%Y-%m-%d")), NA_character_),  
      TRTSTMC_ = ifelse(!! str_trstmvar != "NA", format(!! rlang::sym(str_trstmvar), "%H:%M"), NA_character_),
      TRTSDTC_ = ifelse(!! str_trsdtvar != "NA", format(as.Date(!! rlang::sym(str_trsdtvar)), "%Y-%m-%d"), NA_character_),
      TRTSDTC_ = ifelse(is.na(TRTSTMC_), TRTSDTC_, paste0(TRTSDTC_, "T", TRTSTMC_)),
      TRTETMC_ = ifelse(!! str_tretmvar != "NA", format(!! rlang::sym(str_tretmvar), "%H:%M"), NA_character_),
      TRTEDTC_ = ifelse(!! str_tredtvar != "NA", format(as.Date(!! rlang::sym(str_tredtvar)) + !! n_reldays, "%Y-%m-%d"), NA_character_),
      TRTEDTC_ = ifelse(is.na(TRTETMC_), TRTEDTC_, paste0(TRTEDTC_, "T", TRTETMC_)),
      n_stdtlen_1 = min(nchar(STDTC_), nchar(TRTSDTC_), 0, na.rm=TRUE),
      n_stdtlen_2 = min(nchar(STDTC_), nchar(TRTEDTC_), 0, na.rm=TRUE),
      n_endtlen_1 = min(nchar(STDTC_), nchar(TRTEDTC_), 0, na.rm=TRUE),
      n_endtlen_2 = min(nchar(ENDTC_), nchar(TRTSDTC_), 0, na.rm=TRUE),
      INWINDOW_ = case_when(
        length(!! str_trtavars) == 1 ~ TRUE,
        is.na(STDTC_) ~ FALSE,
        is.na(TRTSDTC_) ~ FALSE,
        nchar(!! endtvar) > 0 & (n_stdtlen_1 > 3) & (substring(STDTC_, 1, n_stdtlen_1) >= substring(TRTSDTC_, 1, n_stdtlen_1)) &
          (n_stdtlen_2 == 0 | (substring(STDTC_, 1, n_stdtlen_2) <= substring(TRTEDTC_, 1, n_stdtlen_2))) ~ TRUE,
        ( (n_endtlen_1 > 3 & substring(STDTC_, 1, n_stdtlen_1) <= substring(TRTEDTC_, 1, n_stdtlen_1)) &
          (n_endtlen_2 == 0 | (substring(ENDTC_, 1, n_stdtlen_2) >= substring(TRTSDTC_, 1, n_stdtlen_2))) ) ~ TRUE,
        TRUE ~ FALSE
      ),
      APERIOD = !! n_aperiod,
      APERIODC = paste0("Period ", APERIOD),
      TRTAN = ifelse(INWINDOW_ & !! str_trtanvar != "NA", !! rlang::sym(str_trtanvar), NA),
      TRTPN = ifelse(INWINDOW_ & !! str_trtpnvar != "NA", !! rlang::sym(str_trtpnvar), NA),
      TRTA = ifelse(INWINDOW_ & !! str_trtavar != "NA", !! rlang::sym(str_trtavar), NA),
      TRTP = ifelse(INWINDOW_ & !! str_trtpvar != "NA", !! rlang::sym(str_trtpvar), NA),
      TRTSDT = ifelse(INWINDOW_ & !! str_trsdtvar != "NA", !! rlang::sym(str_trsdtvar), NA),
      TRTEDT = ifelse(INWINDOW_ & !! str_tredtvar != "NA", !! rlang::sym(str_tredtvar), NA)
    ) %>% dplyr::filter(INWINDOW_) 
    
    if (is.null(df_drvadtrt)) df_drvadtrt <- df_drvadtrt_1 else 
      df_drvadtrt <- rbind(df_drvadtrt, df_drvadtrt_1)
  }

  if (endtvar == "NA") endtvar <- NULL
  if (stdtvar == "NA") stdtvar <- NULL
  
  if (is.null(df_drvadtrt)) {
    df_out <- df_dsetin_1 %>% dplyr::mutate(TRTA=NA, TRTAN=NA, TRTP=NA, TRTPN=NA, APERIOD=1, APERIODC="Period 1", TRTSDT=NA, TRTEDT=NA)
  } else if (! is.null(sliceaperiod) && toupper(sliceaperiod) == "FIRST") {
    df_out_1 <- df_drvadtrt %>% dplyr::arrange(!!!syms(groupbyvars), seqap__, APERIOD) %>% dplyr::group_by(!!!syms(groupbyvars), seqap__) %>% 
      dplyr::slice_head(n=1) %>% dplyr::select(-all_of(unique(c(str_trtavars, str_trtpvars, str_trsdtvars, str_tredtvars, str_trtanvars, 
             str_trtpnvars, "seqap__", "ENDTC_", "STDTC_", "TRTSDTC_", "TRTEDTC_", "INWINDOW_",      
             "TRTSTMC_", "TRTETMC_", "n_stdtlen_1", "n_stdtlen_2", "n_endtlen_1", "n_endtlen_2", stdtvar, endtvar))))  
    str_dropvars <- intersect(names(df_dsetin_1), names(df_out_1))
    str_dropvars <- setdiff(str_dropvars, c(groupbyvars, "seqap__"))
    if (length(str_dropvars) > 0) {
      df_dsetin_1 <- df_dsetin_1 %>% dplyr::select(-all_of(str_dropvars))
    }
    df_out <- dplyr::left_join(x=df_dsetin_1, y=df_out_1, by=c(groupbyvars, "seqap__"), relationship = "many-to-many") %>% dplyr::select(-seqap__)
  } else {
    df_out_1 <- df_drvadtrt %>% dplyr::arrange(!!!syms(groupbyvars), seqap__, APERIOD) %>% dplyr::group_by(!!!syms(groupbyvars), seqap__) %>%
      dplyr::slice_tail(n=1) %>% dplyr::select(-all_of(unique(c(str_trtavars, str_trtpvars, str_trsdtvars, str_tredtvars, str_trtanvars, 
            str_trtpnvars, "seqap__", "ENDTC_", "STDTC_", "TRTSDTC_", "TRTEDTC_", "INWINDOW_",      
            "TRTSTMC_", "TRTETMC_", "n_stdtlen_1", "n_stdtlen_2", "n_endtlen_1", "n_endtlen_2", stdtvar, endtvar))))  
    str_dropvars <- intersect(names(df_dsetin_1), names(df_out_1))
    str_dropvars <- setdiff(str_dropvars, c(groupbyvars, "seqap__"))
    if (length(str_dropvars) > 0) {
      df_dsetin_1 <- df_dsetin_1 %>% dplyr::select(-all_of(str_dropvars))
    }
    df_out <- dplyr::left_join(x=df_dsetin_1, y=df_out_1, by=c(groupbyvars, "seqap__"), relationship = "many-to-many") %>% dplyr::select(-seqap__)
  }
  if (length(str_trtavars) == 1 & "TRTSDT" %in% str_otheradslvars) { str_trsdtvars <- "TRTSDT"}
  if (length(str_trtavars) == 1 & "TRTEDT" %in% str_otheradslvars) { str_tredtvars <- "TRTEDT"}
  if (length(str_trtanvars) >= 1) base::attributes(df_out[["TRTAN"]]) <- base::attributes(dsetinadsl[[str_trtanvars[1]]]) 
  if (length(str_trtavars) >= 1) base::attributes(df_out[["TRTA"]]) <- base::attributes(dsetinadsl[[str_trtavars[1]]]) 
  if (length(str_trtpnvars) >= 1) base::attributes(df_out[["TRTPN"]]) <- base::attributes(dsetinadsl[[str_trtpnvars[1]]]) 
  if (length(str_trtpvars) >= 1) base::attributes(df_out[["TRTP"]]) <- base::attributes(dsetinadsl[[str_trtpvars[1]]])
  if (length(str_trsdtvars) >= 1) base::attributes(df_out[["TRTSDT"]]) <- base::attributes(dsetinadsl[[str_trsdtvars[1]]]) 
  if (length(str_tredtvars) >= 1) base::attributes(df_out[["TRTEDT"]]) <- base::attributes(dsetinadsl[[str_tredtvars[1]]])                                                
                                                
  df_out <- ru_labels(df_out, base::labels(dsetin))
  df_out <- ru_labels(df_out, newvarlabels)
  if (eventtype == "sp") {
    df_out <- df_out %>% dplyr::rename(TPERIOD=APERIOD, TPERIODC=APERIODC) 
  }  
  return(df_out)
}

# dsetin=df_lastdset
# dsetinadsl=adsl
# groupbyvars=c("STUDYID", "USUBJID")
# stdtvar="EVENT_DTC"
# endtvar=NULL
# reldays=0
# sliceaperiod=NULL
# eventtype="pl"

# df <-  ru_aperiod (dsetin=d.mb, dsetinadsl=G_POPDATA, groupbyvars=c("STUDYID", "USUBJID"),
#                        stdtvar="LBDTC", endtvar=NULL, reldays=0, sliceaperiod=NULL ) 
