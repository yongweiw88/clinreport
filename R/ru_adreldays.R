#' Derive analysis relative day and analysis relative day in period variables
#'
#' Pass in a data and return a data with analysis relatie day being added.
#'
#' @param dsetin Input data (data frame).
#' @param domaincode SDTM domain code (e.g., "LB", "AE")
#' @param dyrefdatevar Reference date variable (e.g., "ADY", "ASTDY") 
#'
#' @return A data frame with analysis relatie day being added.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#'
#' @export
#'
ru_adreldays <- function(
    dsetin,              
    domaincode = NULL,     
    dyrefdatevar = NULL    
) {
  if (is.null(domaincode)) domaincode=""
  if (is.null(dyrefdatevar)) dyrefdatevar=""

    # Validate parameters
  if (is.null(dsetin) || !is.data.frame(dsetin)) {
    stop("ru_adreldays: The parameter dsetin is required and must be a data frame.")
  }
  if (domaincode == "" && dyrefdatevar == "") {
    stop("ru_adreldays: One of domaincode or dyrefdatevar is required.")
  }
  
  # Convert domaincode to uppercase for consistency
  domaincode <- toupper(domaincode)
  
  # Check if relative day variables exist
  str_day_vars <- base::intersect(c("ADY", "ASTDY", "AENDY"), names(dsetin))
  if (length(str_day_vars) > 0) {
    message("ru_adreldays: Relative day variable(s) ", paste(str_day_vars, collapse = ", "),
            " exist on the input dataset and may be overwritten.")
  }

  # Determine date variables
  str_adt_varlist <- c("ADT", "ASTDT", "AENDT")
  str_adt_varexist <- base::intersect(str_adt_varlist, names(dsetin))
  
  str_domain_dtvarlist <- paste0(domaincode, c("DY", "STDY", "ENDY"))
  str_domain_dtvarexist <- base::intersect(str_domain_dtvarlist, names(dsetin))  
  
  # Derive relative study days
  df_studyday <- dsetin
  if (dyrefdatevar[1] != "") {
    if (length(str_adt_varexist) > 0) {
      message("ru_adreldays: Using analysis date variables ", paste(str_adt_varexist, collapse = ", "),
              " to derive analysis relative days.")
    } else if (domaincode == "") {
      message("ru_adreldays: No analysis date variables ", paste(str_adt_varlist, collapse = ", "),
              " exist on the input dataset and DOMAINCODE is missing.")
      message("ru_adreldays: Analysis relative day variables will not be derived.")
    } else if (length(domain_dtvarexist) > 0) {
      message("ru_adreldays: No analysis date variables ", paste(str_adt_varlist, collapse = ", "),
              " exist on the input dataset.")
      message("ru_adreldays: Using domain date variables ", paste(domain_dtvarexist, collapse = ", "),
              " to derive analysis relative days.")
    } else {
      message("ru_adreldays: No analysis date variables ", paste(str_adt_varlist, collapse = ", "),
              " or domain date variables ", paste(domain_dtvarlist, collapse = ", "),
              " exist on the input dataset.")
      message("ru_adreldays: Analysis relative day variables will not be derived.")
    }
    
    # Derive ADY, ASTDY, AENDY
    for (str_this_var in c("DT", "STDT", "ENDT")) {
      if (paste0("A", str_this_var) %in% str_adt_varexist ) {
        str_this_dtvar <- paste0("A", str_this_var)
      } else if (paste0(domaincode, str_this_var) %in% str_domain_dtvarexist ) {
        str_this_dtvar <- paste0(domaincode, str_this_var)
      } else str_this_dtvar <- NULL
      
      if (! is.null(str_this_dtvar)) {
        df_studyday[[paste0("A", stringr::str_replace(str_this_var, "T$", "Y"))]] = if_else(df_studyday[[str_this_dtvar]] >= df_studyday[[dyrefdatevar]], 
                                       as.numeric(as.Date(df_studyday[[str_this_dtvar]]) - as.Date(df_studyday[[dyrefdatevar]]) + 1), 
                                       as.numeric(as.Date(df_studyday[[str_this_dtvar]]) - as.Date(df_studyday[[dyrefdatevar]])))
      }
    }
  } else {
    # Use SDTM domain relative day variables
    str_day_vars <- paste0(domaincode, c("DY", "STDY", "ENDY"))
    str_day_varexist <- base::intersect(str_day_vars, names(dsetin))
    
    if (length(str_day_varexist) == 0) {
      message("ru_adreldays: No SDTM domain relative day variables ", paste(str_day_vars, collapse = ", "),
              " exist on the input dataset.")
      message("ru_adreldays: Analysis relative day variables will not be derived.")
    } else {
      for (str_this_var in c("DY", "STDY", "ENDY")) {
        if (paste0(domaincode, str_this_var) %in% str_day_varexist) {
          df_studyday[[paste0("A", str_this_var)]] = df_studyday[[paste0(domaincode, str_this_var)]]
        }
      }
    }
  }
  
  # Derive period-specific relative days
  str_period_vars <- base::intersect(c("APERIOD", "TPERIOD"), names(dsetin))
  if (length(str_period_vars) > 0) {
    str_per_var <- if_else("APERIOD" %in% str_period_vars, "APERA", "TPERA")

    if ("TRSDT" %in% names(df_studyday)) {
      for (str_this_var in c("DT", "STDT", "ENDT")) {
        if (patse0("A", str_this_var) %in% str_adt_varexist ) {
          str_this_dtvar <- paste0("A", str_this_var)
        } else if (paste0(domaincode, str_this_var) %in% str_domain_dtvarexist ) {
          str_this_dtvar <- paste0(domaincode, str_this_var)
        } else str_this_dtvar <- NULL
        
        if (! is.null(str_this_dtvar)) {
          df_studyday[[paste0(str_per_var, stringr::str_replace(str_this_var, "T$", "Y"))]] = if_else(df_studyday[[str_this_dtvar]] >= df_studyday[["TRSDT"]], 
            as.numeric(as.Date(df_studyday[[str_this_dtvar]]) - as.Date(df_studyday[["TRSDT"]]) + 1), 
            as.numeric(as.Date(df_studyday[[str_this_dtvar]]) - as.Date(df_studyday[["TRSDT"]])))
        }
      }
    } else {
      message("ru_adreldays: Period variable ", paste(str_period_vars, collapse = " or "),
              " exists on the input dataset.")
      message("ru_adreldays: but Treatment start date variable TRSDT does not.")
      message("ru_adreldays: Analysis relative day in period variables will not be derived.")
    }
  }
  return(df_studyday)
}

# Example usage (uncomment to test):
# dsetin <- read_sas("./data/ardata_lab1.sas7bdat")
# result <- ru_adreldays(dsetin = dsetin, dsetout_name = "ardata_lab_with_days",
#                        domaincode = "LB", dyrefdatevar = "EVENT_DATE", data_path = "./data/")

