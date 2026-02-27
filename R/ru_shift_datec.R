#' Shift a character date/time in "%Y-%m-%d" format by days and ratio of hours/minutes
#'
#' Pass in a character date value, shift days, HHMM ration and return a character date 
#'
#' @param str_datec A character date value .
#'
#' @return A character date with different format.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'   ru_shift_datec("2000-08-01", 20, 0.8)
#'
#' @export
#'
ru_shift_datec <- function(str_datec, shiftdy, tmratio=1) {
  str_newdatec <- dplyr::case_when(
    nchar(str_datec) == 4 ~ paste(str_datec, "06-01", sep = "-"),
    nchar(str_datec) == 7 ~ paste(str_datec, "01", sep = "-"),
    TRUE ~ str_datec
  )
  this_date <- if_else(nchar(str_newdatec) < 10, NA, as.Date(substr(str_newdatec, 1, 10)))
  this_time <- if_else(nchar(str_datec) < 12, NA, data.table::as.ITime(substring(str_newdatec, 12)))
  rtn_newdatec <- dplyr::case_when(
    is.na(str_datec) ~ NA,
    nchar(str_datec) < 4 ~ NA,
    nchar(str_datec) == 4 ~ substr(format(as.Date(str_newdatec, format="%Y-%m-%d") + shiftdy, "%Y-%m-%d"), 1, 4),
    nchar(str_datec) == 7 ~ substr(format(as.Date(str_newdatec, fomrat="%Y-%m-%d") + shiftdy, "%Y-%m-%d"), 1, 7),
    nchar(str_datec) == 10 ~ format(as.Date(this_date) + shiftdy, "%Y-%m-%d"),
    nchar(str_datec) == 16 ~ substr(paste0(format(as.Date(this_date) + shiftdy, "%Y-%m-%d"), "T", format(data.table::as.ITime(this_time)  * tmratio, "%H:%M")), 1, 16),
    nchar(str_datec) == 19 ~ paste0(format(as.Date(this_date) + shiftdy, "%Y-%m-%d"), "T", format(data.table::as.ITime(this_time)  * tmratio, "%H:%M:%S")),
    TRUE ~ NA
  )
  return(rtn_newdatec)
}
