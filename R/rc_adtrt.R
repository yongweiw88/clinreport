#========================================================================= 
# Author:  Yongwei Wang
# Program: rc_adtrt.R
# Purpose: Create ADTRT dataset
# Date:    02/18/2025
#========================================================================= 

rc_adtrt <- function (dsetinadsl, byvars=c("STUDYID", "USUBJID"), aperiodvarname="APERIOD") {
  print(paste0("RC_ADTRT: ", "Start or RC_ADTRT"))
  
  vars <- toupper(names(dsetinadsl))
  n_period <- vars[grep("TRT\\d{2}[A|P]", vars, perl=TRUE)] %>% gsub("\\D", "", .) %>% as.numeric() %>% max()
  aperiodcvarname=paste0(aperiodvarname, "C")
  
  label.new <- list( 
    "TRTA"       ="Actual Treatment",
    "TRTP"       ="Planned Treatment",
    "TRTAN"      ="Actual Treatment (N)",
    "TRTPN"      ="Planned Treatment (N)",
    aperiodcvarname   ="Period (C)",
    aperiodvarname    ="Period",
    "TRSDT"      ="Date of First Exposure to Treatment",
    "TREDT"      ="Date of Last Exposure to Treatment",
    "TRSTM"      ="Time of First Exposure to Treatment",
    "TRETM"      ="Time of Last Exposure to Treatment",
    "APERSDT"    ="Period Start Date",
    "APERSTM"    ="Period Start Time",
    "APEREDT"    ="Period End Date",
    "APERETM"    ="Period End Time"
    )

  var.prefix <- c("TRT",   "TRT",  "TRT",   "TRT",  "TR",     "TR",     "TR",      "TR",     "AP",      "AP",      "AP",      "AP")
  var.suffix <- c("AN",    "A",    "PN",    "P",    "SDT",    "STM",    "EDT",     "ETM",    "SDT",     "STM",     "EDT",     "ETM")
  var.new <-    c("TRTAN", "TRTA", "TRTPN", "TRTP", "TRTSDT", "TRTSTM", "TRTEDT",  "TRTETM", "APERSDT", "APERSTM", "APEREDT", "APERETM")  
  var.exist <- rep(FALSE, length(var.new))
    
  df.list <- vector("list", n_period)
  var_names <- NULL
  for (j in seq_len(n_period)) {
    periodc <- sprintf("%02d", j)
    
    df.temp <- dsetinadsl %>%
      dplyr::select(all_of(byvars)) %>%
      dplyr::mutate(!!aperiodvarname := j, !!aperiodcvarname := paste0("Period ", j))
    
    for (k in seq_along(var.new)) {
      var_name <- paste0(var.prefix[k], periodc, var.suffix[k])
      var_names <- c(var_names, var_name)
      if (var_name %in% vars) {
        df.temp <- df.temp %>% dplyr::mutate(!!var.new[k] := dsetinadsl[[var_name]])
      }
    }
    
    df.list[[j]] <- df.temp
  }
  
  df.out <- as.data.frame(dplyr::bind_rows(df.list))
  var_names <- base::intersect(names(dsetinadsl), var_names)
  var.new <- base::intersect(var.new, names(df.out))
  
  df.dsetinadsl <- dsetinadsl %>% dplyr::select(-all_of(c(var_names)))
    
  df.out <- df.out %>%
    dplyr::select(all_of(c(byvars, aperiodvarname, aperiodcvarname, var.new))) %>%
    dplyr::left_join(df.dsetinadsl, by = byvars)

  df.out <- ru_labels(df.out, base::labels(dsetinadsl))
  df_lastdset <- ru_labels(df.out, label.new)
  
  
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

# setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("name", "age", "gender"))
 
# df <- rc_adtrt(d.pop.1) 
# df.1 <- df[, c(70, 72)]


