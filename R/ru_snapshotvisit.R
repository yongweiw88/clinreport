#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_snapshotvisit.R
# Purpose: Derive Snapshot visit based on snapshot algorithm defined in 
#          SAP
# Date:    08/23/2024
#========================================================================= 
ru_snapshotvisit <- function(dsetin, dsetinadsl=G_POPDATA, dsetinvisit, groupbyvars=c("STUDYID", "USUBJID"),
  stdtvar=NULL, endtvar=NULL, stdwindowweeks=6) {
  print(paste0("RU_SNAPSHOT: ", "Start or RU_SNAPSHOT VISITS"))
  newvarlabels <- list("SVISITN"="Snapshot Analysis Visit (N)", "SVISIT"="Snapshot Analysis Visit", 
                       "AWTARGET"="Analysis Window Target", "AWRANGE"="Analysis Window Valid Relative Range") 
  
  #
  # Create a Visit data set with visit target day (AWTARGET), start day (STARTDY) and end day (ENDDY)
  # if input Visit dataset does not include those variables
  #
  str_visitnames <- names(dsetinvisit)

  if (length(base::intersect(c("STARTDY", "ENDDY", "AWTARGET"), toupper(str_visitnames))) == 3) 
    bl_visitready <- TRUE else bl_visitready <- FALSE
  
  df_dsetinvisit <- dsetinvisit
  names(df_dsetinvisit) <- toupper(names(dsetinvisit))
  if (! bl_visitready) {
    str_visits_1 <- unlist(levels(factor(df_dsetinvisit[["VISIT"]])))
    str_visits_2 <- str_visits_1[! grepl('UNSCH', str_visits_1, ignore.case = TRUE)]
    
    str_visits <- list()
    k <- 0
    for (i in 1:length(str_visits_2)) {
      str_visit_p <- unlist(base::strsplit(str_visits_2[i], " "))
      if (length(str_visit_p) == 2) {
        if (toupper(str_visit_p[1]) == "WEEK" && ! is.na(suppressWarnings(as.numeric(str_visit_p[2])))) {
          n_week <- as.numeric(str_visit_p[2])
          n_awtarget <- n_week * 7 + 1
          n_startdy <- max((n_week - stdwindowweeks) * 7 + 1, 1)
          n_aendy <- (n_week + stdwindowweeks) * 7 
          str_visit_1 <- data.frame("SVISITN"=n_week, "SVISIT"=str_visits_2[i], "AWTARGET"=n_awtarget, "STARTDY"=n_startdy, 
                                  "ENDDY"=n_aendy)
          if (k == 0) str_visits <- str_visit_1
          else str_visits <- rbind(str_visits, str_visit_1)
          k <- k + 1
        } 
      }
    }
    df_visits <- as.data.frame(str_visits)
  } else {
    df_vists <- df_dsetinvisit
  }
  df_visits <- df_visits %>% dplyr::mutate(AWRANGE=paste0('Day ', STARTDY, ' to Day ', ENDDY))
  
  # df_visits <<- df_visits
  
  str_visdnames <- toupper(names(df_visits))


  if ("APERIOD" %in% toupper(names(df_visits))) 
    str_periodvar <- "APERIOD" else str_periodvar <- NULL

  #
  # Add character version of start date and end date in DSETIN 
  #
  stdtcvar <- stdtvar
  endtcvar <- endtvar
  if (! is.null(stdtvar) && is.numeric(typeof(dsetin[stdtvar]))) stdtcvar <- NULL
  if (! is.null(endtcvar) && is.numeric(typeof(endtvar))) endtcvar <- NULL

  #
  # Add row number and keep only variables which will be used to derive visit 
  #
  df_dsetin_1 <- dplyr::mutate(dsetin, seqsnap__=dplyr::row_number())
  df_dsetin_2 <- ru_aperiod(df_dsetin_1, dsetinadsl=dsetinadsl, groupbyvars=groupbyvars,
                           stdtvar=stdtvar, endtvar=endtvar, reldays=0, sliceaperiod=NULL)
  

  #
  # Derive start day (n_astdy) and end day (n_aendy) from TRTSDT
  #
  if (! is.null(stdtcvar)) {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(
      thisstdt = case_when(
        (nchar(!! rlang::sym(stdtcvar)) < 4) ~ NA_character_,
        (nchar(!! rlang::sym(stdtcvar)) == 4) ~ paste0(!! rlang::sym(stdtcvar), "-01-01"),
        (nchar(!! rlang::sym(stdtcvar)) == 7) ~ paste0(!! rlang::sym(stdtcvar), "-01"),
        TRUE ~ substring(!! rlang::sym(stdtcvar), 1, 10)
      ),
      n_astdy = case_when(
        (is.na(!! rlang::sym(stdtcvar))) ~ 1,
        (is.na(TRTSDT)) ~ NA,
        TRUE ~ as.numeric(as.Date(thisstdt, "%Y-%m-%d") - TRTSDT + 1)
      )
    ) %>% dplyr::select(-thisstdt)
  } else if (! is.null(stdtvar)) {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(
        n_astdy = as.numeric(!! rlang::sym(stdtvar) - TRTSDT + 1)
    )
  } else {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(n_astdy=NA)
  }
  
  if (! is.null(endtcvar)) {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(
      thisendt = case_when(
        (nchar(!! rlang::sym(endtcvar)) < 4) ~ NA_character_,
        (nchar(!! rlang::sym(endtcvar)) == 4) ~ paste0(!! rlang::sym(endtcvar), "-01-01"),
        (nchar(!! rlang::sym(endtcvar)) == 7) ~ paste0(!! rlang::sym(endtcvar), "-01"),
        TRUE ~ substring(!! rlang::sym(endtcvar), 1, 10)
      ),
      n_aendy = case_when(
         (is.na(!! rlang::sym(endtcvar))) ~ 1,
         (is.na(TRTSDT)) ~ NA,
         TRUE ~ as.numeric(as.Date(thisendt, "%Y-%m-%d") - TRTSDT + 1)
        )
    ) %>% dplyr::select(-thisendt)
  } else if (! is.null(endtvar)) {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(
      n_aendy = as.numeric(!! rlang::sym(endtvar) - TRTSDT + 1)
    )
  } else {
    df_dsetin_2 <- df_dsetin_2 %>% dplyr::mutate(n_aendy=NA)
  }
  
  #
  # Loop over each visit. If both start date and end date are given,
  # a record can be in multiple visits and the record will be duplicated multiple times
  # in that case.
  #
  df_out <- NULL
  for (i in 1:nrow(df_visits)) {
    if (is.null(str_periodvar)) {
      df_dsetin_3 <- base::merge(df_dsetin_2, df_visits[i, ], all.x=TRUE, all.y=FALSE)
    } else {
      df_dsetin_3 <- dplyr::left_join(df_dsetin_2, df_visits[i, ], by="APERIOD", all.x=TRUE, all.y=FALSE)
    }
    df_dsetin_4 <- df_dsetin_3 %>% dplyr::mutate(
      INWINDOW_ = case_when(
        is.na(n_astdy) & is.na(n_aendy) ~ FALSE, 
        ! is.na(n_astdy) & n_astdy >= STARTDY & n_astdy <= ENDDY ~ TRUE,
        is.na(n_astdy) & STARTDY <= n_aendy ~ TRUE,        
        ! is.na(n_aendy) & STARTDY <= n_aendy & n_astdy <= ENDDY ~ TRUE,
        TRUE ~ FALSE
      ) 
    ) %>% dplyr::filter(INWINDOW_)
    if (is.null(df_out)) df_out <- df_dsetin_4 else
      df_out <- rbind(df_out, df_dsetin_4)
  }

  df_out_1 <- dplyr::anti_join(x=df_dsetin_2, y=df_out, by="seqsnap__")
  df_out_2 <- ru_setdata(df_out_1, df_out, keeprownames = FALSE) %>% dplyr::arrange(seqsnap__) %>% 
    dplyr::select(-seqsnap__, -INWINDOW_, -n_astdy, -n_aendy, -STARTDY, -ENDDY)
  
  df_out_2 <- ru_labels(df_out_2, base::labels(df_dsetin_2))
  df_out_2 <- ru_labels(df_out_2, newvarlabels)
  return(df_out_2)
}

# dsetin=d.mb
# dsetinadsl=G_POPDATA
# dsetinvisit=d.sv
# groupbyvars=c("STUDYID", "USUBJID")
# stdtvar="LBDTC"
# endtvar=NULL
# stdwindowweeks=6
# 
# df <- ru_snapshotvisit(dsetin=d.mb, dsetinadsl=G_POPDATA, dsetinvisit=d.sv, groupbyvars=c("STUDYID", "USUBJID"), stdtvar="LBDTC", endtvar=NULL, stdwindowweeks=6) 


  
