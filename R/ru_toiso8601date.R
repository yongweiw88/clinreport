#'
#' Pass in a dataset to convert date/time columns into ISO8601 date character variables.
#'
#' @param Dsetin a dataset with date columns, ending with "DT", and/or time columns, ending with "TM"
#' @param includevars Date/time column names which should be included
#'
#' @return A data frame with all records and variables from DSETIN and extra variables from DSETINCO.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' df_adsl <- ru_toiso8601date(adamdata$adsl(), NULL) 
#'
#' @export
#'

ru_toiso8601date <- function (dsetin, includevars=NULL, filltimewith=NA) {
  print(paste0("RU_DATETIME: ", "Start or RU_TOISO8601DATE"))
  s.names <- toupper(base::names(dsetin))
  base::names(dsetin) <- s.names
  
  if (is.null(includevars)) includevars <- NULL
  else includdevars <- toupper(includevars)
  
  d.out <- dsetin 
  for (i in 1:length(s.names)) {
    if (nchar(s.names[i]) > 3 && base::substring(s.names[i], nchar(s.names[i]) - 1) == "DT") {
      if (length(includevars) == 0 || (s.names[i] %in% includevars) ) {
        s.timevar <-  paste0(base::substring(s.names[i], 1, nchar(s.names[i]) - 2), "TM")
        s.datecvar <- paste0(s.names[i], "C")
        if (s.timevar %in% s.names) {
          d.timevar <- ifelse(is.na(d.out[s.timevar]), filltimewith, format(d.out[s.timevar], "%H:%M"))
          d.datecvar <- paste0(format(d.out[[s.names[i]]], "%Y-%m-%d"), "T", d.timevar)
          d.out[s.datecvar] <- d.datecvar
        } else {
          if (is.null(filltimewith) || is.na(filltimewith)) {
            d.datecvar <- format(d.out[[s.names[i]]], "%Y-%m-%d")
          } else {
            d.datecvar <- paste0(format(d.out[[s.names[i]]], "%Y-%m-%d"), "T", filltimewith)
          }
        }
        d.out[s.datecvar] <- d.datecvar  
      }
    }
  }
  d.out 
}


