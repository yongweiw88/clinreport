#' Lab value and normal range conversion to SI units
#'
#' Pass in a data frame alignment criteria to have columns aligned for reporting.
#'
#' @param dsetin The he dataset for which SI conversion needs to be done.
#' @param convdset the SI dataset which contains the conversion factors. 
#'
#' @return A data frame with results and hi/lo results in standard unit.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'
ru_conv <- function(
    dsetin,              # Input dataset (data frame)
    convdset = NULL      # Conversion dataset (data frame or NULL)
) {
  # Validate parameters
  if (is.null(dsetin) || !is.data.frame(dsetin)) {
    stop("ru_conv: The parameter dsetin is required and must be a data frame.")
  }
  
  # Check if convdset is provided and exists
  if (is.null(convdset)) {
    message("ru_conv: convdset dataset parameter not passed - conversions not done.")
    return(dsetin)
  }

  if (!is.data.frame(convdset)) {
    message("ru_conv: convdset dataset does not exist - conversions not done.")
    return(dsetin)
  }
  
  lbdtvar <- case_when(
    "LBDTC" %in% names(dsetin) ~ "LBDTC",
    "LBDT" %in% names(dsetin) ~ "LBDT",
    TRUE ~ NA_character_
  )

  # Check for LBREFCD existence
  lbrefcd_exist <- exists("LBREFCD", convdset)
  
  # Merge dsetin with convdset
  df_dsetin <- dsetin   
  
  if ("LBORUNIT" %in% names(dsetin)) lborunitvar <- "LBORUNIT" else
    if ("LBORRESU" %in% names(dsetin)) lborunitvar <- "LBORRESU" else {
      message("ru_conv: RTERROR Can't find original test unit variable LBORUNIT or LBORRESU in dsetin")
      return(dsetin)
    }
  
  if ("LBORRES" %in% names(dsetin) & !  ("LBORRESN" %in% names(dsetin))) {
    df_dsetin <- df_dsetin %>% 
      dplyr::mutate(LBORRESN=suppressWarnings(as.numeric(LBORRES)))
  }
  
  if ( ! ("LBSTRESN" %in% names(dsetin))) {
    df_dsetin <- df_dsetin %>% 
      dplyr::mutate(LBSTRESN=NA)
  }

  if ( ! ("LBSTUNIT" %in% names(dsetin))) {
    df_dsetin <- df_dsetin %>% 
      dplyr::mutate(LBSTUNIT=NA)
  }
  
  if  ((lborunitvar %in% names(convdset))) df_convdset <- convdset else
    if ("LBORUNIT" %in% names(convdset)) {
      df_convdset <- convdset %>% dplyr::rename(LBORRESU=LBORUNIT)
    } else {
      message("ru_conv: RTERROR Can't find original test unit variable LBORUNIT or LBORRESU in convdset")
      return(dsetin)
    }
  
  if ("LBSTRESU" %in% names(convdset)) {
    df_convdset <- df_convdset %>% dplyr::rename(LBSTUNIT=LBSTRESU)
  } else if (! ("LBSTUNIT" %in% names(convdset))) {
    message("ru_conv: RTERROR Can't find standard test unit variable LBSTUNIT or LBSTRESU in convdset")
    return(dsetin)
  }
  
  # Sort and deduplicate df_labdata and df_convdata
  df_labdata <- df_dsetin %>%
    dplyr::select(LBTESTCD, !! rlang::sym(lborunitvar)) %>%
    dplyr::distinct()
  
  df_convdata <- df_convdset %>%
    dplyr::select(LBTESTCD, !! rlang::sym(lborunitvar)) %>%
    dplyr::distinct() 
  
  # Identify LBTESTCD/LBORUNIT not in convdset
  df_unmatched <- df_labdata %>% 
    dplyr::filter(! is.na(!! rlang::sym(lborunitvar))) %>%
    anti_join(df_convdata, by = c("LBTESTCD", lborunitvar))
  
  if (nrow(df_unmatched) > 0) {
    for (i in 1:min(nrow(df_unmatched), 20)) {
      message("ru_conv: Values of LBTESTCD/LBORUNIT(LBORRESU) found in dsetin but not in convdset: ",
              "LBTESTCD=", df_unmatched$LBTESTCD[i], " ", lborunitvar, "=", df_unmatched[[lborunitvar]][i])
    }
    if (nrow(df_unmatched) > 20) {
      message("ru_conv: Additional unmatched LBTESTCD/LBORUNIT(LBORRESU) pairs exist - no further notes printed.")
    }
  }

  df_labconv <- df_dsetin %>%
    dplyr::left_join(
      df_convdset %>% dplyr::select(LBTESTCD, !! rlang::sym(lborunitvar), LBSTUNIT, LBCNVFCT, all_of(if (lbrefcd_exist) "LBREFCD" else NULL)) %>%
        dplyr::rename(stunit__=LBSTUNIT),
      by = c("LBTESTCD", lborunitvar)
    ) %>%
    dplyr::mutate(LBSTUNIT = as.character(stunit__)) # Ensure LBSTUNIT is character
  
  # Split into df_convert (factor multiplication) and percent (percent/ratio) datasets
  df_convert <- df_labconv %>%
    dplyr::filter(LBCNVFCT != -1 | is.na(LBCNVFCT)) %>%
    dplyr::mutate(
      unmatch_unit = 0,
      unmatch_value = 0,
    ) %>%
    dplyr::mutate(
      stresn__ = if_else(!is.na(LBORRESN) & !is.na(LBCNVFCT), round(LBORRESN * LBCNVFCT, 8), NA_real_),
      LBSTNRHI = if_else(!is.na(LBORNRHI) & !is.na(LBCNVFCT), round(suppressWarnings(as.numeric(LBORNRHI)) * LBCNVFCT, 8), NA_real_),
      LBSTNRLO = if_else(!is.na(LBORNRLO) & !is.na(LBCNVFCT), round(suppressWarnings(as.numeric(LBORNRLO)) * LBCNVFCT, 8), NA_real_),
      unmatch_unit = if_else(LBSTUNIT != "" & !is.na(LBSTUNIT) & LBSTUNIT != stunit__, dplyr::lag(unmatch_unit) + 1, dplyr::lag(unmatch_unit)),
      unmatch_value = if_else(!is.na(LBSTRESN) & round(LBSTRESN, 7) != round(stresn__, 7), unmatch_value + 1, dplyr::lag(unmatch_value))
    )
  
  # Log unit mismatches
  df_unit_mismatches <- df_convert %>% dplyr::filter(unmatch_unit > 0)
  if (nrow(df_unit_mismatches) > 0) {
    for (i in 1:min(nrow(df_unit_mismatches), 20)) {
      if (is.na(lbdtvar)) {
        message("ru_conv: Lab converted unit does not match: ",
                "SUBJID=", df_unit_mismatches$SUBJID[i], " ", 
                " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTUNIT=", df_unit_mismatches$LBSTUNIT[i],
                " stunit__=", df_unit_mismatches$stunit__[i])        
      } else {
        message("ru_conv: Lab converted unit does not match: ",
                "SUBJID=", df_unit_mismatches$SUBJID[i], " ", lbdtvar, "=", df_unit_mismatches[[lbdtvar]][i],
                " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTUNIT=", df_unit_mismatches$LBSTUNIT[i],
                " stunit__=", df_unit_mismatches$stunit__[i])
      }
    }
    if (nrow(df_unit_mismatches) > 20) {
      message("ru_conv: Lab converted unit does not match - no further notes printed.")
    }
  }
  
  # Log value mismatches
  df_value_mismatches <- df_convert %>% dplyr::filter(unmatch_value > 0)
  if (nrow(df_value_mismatches) > 0) {
    for (i in 1:min(nrow(df_value_mismatches), 20)) {
      if (is.na(lbdtvar)) {
        message("ru_conv: Lab converted value does not match: ",
                "SUBJID=", df_value_mismatches$SUBJID[i], " ", 
                " LBTESTCD=", df_value_mismatches$LBTESTCD[i], " LBSTRESN=", df_value_mismatches$LBSTRESN[i],
                " stresn__=", df_value_mismatches$stresn__[i])        
      } else {
        message("ru_conv: Lab converted value does not match: ",
                "SUBJID=", df_value_mismatches$SUBJID[i], " ", lbdtvar, "=", df_value_mismatches[[lbdtvar]][i],
                " LBTESTCD=", df_value_mismatches$LBTESTCD[i], " LBSTRESN=", df_value_mismatches$LBSTRESN[i],
                " stresn__=", df_value_mismatches$stresn__[i])
      }
    }
    if (nrow(df_value_mismatches) > 20) {
      message("ru_conv: Lab converted value does not match - no further notes printed.")
    }
  }
  
  # Update LBSTUNIT and LBSTRESN
  df_convert <- df_convert %>%
    dplyr::mutate(
      LBSTUNIT = stunit__,
      LBSTRESN = stresn__
    ) %>%
    dplyr::select(-unmatch_unit, -unmatch_value)
  
  # Process percent/ratio conversions
  df_percent <- df_labconv %>%
    dplyr::filter(LBCNVFCT == -1)
  
  if (nrow(df_percent) > 0) {
    if (!lbrefcd_exist) {
      message("ru_conv: LBCNVFCT=-1, but LBREFCD does not exist in convdset.")
      df_percent <- df_convert # Treat as regular conversion
    } else {
      # Check for additional merge keys
      exist_lbacttm <- exists("LBACTTM", df_percent)
      exist_cycle <- exists("CYCLE", df_percent)
      exist_visitnum <- exists("VISITNUM", df_percent)
      
      # Merge for reference values
      df_refpcnt <- df_percent %>%
        dplyr::select(-stunit__) %>%
        dplyr::left_join(
          df_convert %>% dplyr::select(STUDYID, SUBJID, !!sym(lbdtvar), LBIDCD, LBACTTM, CYCLE, VISITNUM, LBTESTCD, stresn__, stunit__),
          by = c("STUDYID", "SUBJID", lbdtvar, "LBIDCD",
                 if (exist_lbacttm) "LBACTTM" else NULL,
                 if (exist_cycle) "CYCLE" else NULL,
                 if (exist_visitnum) "VISITNUM" else NULL,
                 "LBREFCD" = "LBTESTCD")
        ) %>%
        rename(ref__ = stresn__.y, stunit__ = stunit__.y)
      
      # Apply percent/ratio conversion
      df_convpcnt <- df_refpcnt %>%
        dplyr::mutate(
          unmatch_unit = 0,
          unmatch_value = 0,
          stresn__ = case_when(
            !! rlang::sym(lborunitvar) == "%" & !is.na(LBORRESN) & !is.na(ref__) ~ round(LBORRESN * ref__ / 100, 8),
            !! rlang::sym(lborunitvar) == "RATIO" & !is.na(LBORRESN) & !is.na(ref__) ~ round(LBORRESN * ref__, 8),
            TRUE ~ NA_real_
          ),
          unmatch_unit = if_else(LBSTUNIT != "" & !is.na(LBSTUNIT) & LBSTUNIT != stunit__, unmatch_unit + 1, unmatch_unit),
          unmatch_value = if_else(!is.na(LBSTRESN) & round(LBSTRESN, 7) != round(stresn__, 7), unmatch_value + 1, unmatch_value),
          LBSTUNIT = stunit__,
          LBSTRESN = stresn__
        )
      
      # Log unit mismatches for df_convpcnt
      df_unit_mismatches <- df_convpcnt %>% dplyr::filter(unmatch_unit > 0)
      if (nrow(df_unit_mismatches) > 0) {
        for (i in 1:min(nrow(df_unit_mismatches), 20)) {
          if (is.na(lbdtvar)) {
            message("ru_conv: Lab converted unit does not match: ",
                    "SUBJID=", df_unit_mismatches$SUBJID[i], " ", 
                    " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTUNIT=", df_unit_mismatches$LBSTUNIT[i],
                    " stunit__=", df_unit_mismatches$stunit__[i])            
          } else {
            message("ru_conv: Lab converted unit does not match: ",
                    "SUBJID=", df_unit_mismatches$SUBJID[i], " ", lbdtvar, "=", df_unit_mismatches[[lbdtvar]][i],
                    " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTUNIT=", df_unit_mismatches$LBSTUNIT[i],
                    " stunit__=", df_unit_mismatches$stunit__[i])
          }
        }
        if (nrow(df_unit_mismatches) > 20) {
          message("ru_conv: Lab converted unit does not match - no further notes printed.")
        }
      }
      
      # Log value mismatches for df_convpcnt
      df_value_mismatches <- df_convpcnt %>% dplyr::filter(unmatch_value > 0)
      if (nrow(df_value_mismatches) > 0) {
        for (i in 1:min(nrow(df_value_mismatches), 20)) {
          if (is.na(lbdtvar)) {
            message("ru_conv: Lab converted value does not match: ",
                    "SUBJID=", df_value_mismatches$SUBJID[i], " ", 
                    " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTRESN=", df_value_mismatches$LBSTRESN[i],
                    " stresn__=", df_value_mismatches$stresn__[i])            
          } else {
            message("ru_conv: Lab converted value does not match: ",
                    "SUBJID=", df_value_mismatches$SUBJID[i], " ", lbdtvar, "=", df_value_mismatches[[lbdtvar]][i],
                    " LBTESTCD=", df_unit_mismatches$LBTESTCD[i], " LBSTRESN=", df_value_mismatches$LBSTRESN[i],
                    " stresn__=", df_value_mismatches$stresn__[i])
          }
        }
        if (nrow(df_value_mismatches) > 20) {
          message("ru_conv: Lab converted value does not match - no further notes printed.")
        }
      }
      
      # Combine df_convert and df_convpcnt
      df_result <- bind_rows(df_convert, df_convpcnt) %>%
        dplyr::select(-stunit__, -stresn__, -LBCNVFCT, -all_of(if (lbrefcd_exist) "LBREFCD" else NULL))
      
      # Write output
      return(df_result)
    }
  }
  
  # If no percent records, output df_convert dataset
  df_result <- df_convert %>%
    dplyr::select(-stunit__, -stresn__, -LBCNVFCT, -all_of(if (lbrefcd_exist) "LBREFCD" else NULL))
  
  # Write output
  return(df_result)
}
