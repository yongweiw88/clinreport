#' Merge data together
#'
#' Pass in data and return merged data
#'
#' @param ... Data to be merged.
#' @param by Merge by-column
#' @param keepvarsin If common columns are found, keep columns in first or last data
#' 
#' @return Merged data
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'

ru_mergedata <- function (..., by, keepvarsin=c("first", "last")) {
  print(paste0("RU_MERGEDATA: ", "Start of RU_MERGEDATA"))
  
  argg <- c(as.list(environment()), list(...))

  for (i in 3:length(argg)) {
    this_data <- argg[[i]] 
    if (i == 3) {df_out <-this_data
    } else {
      str_names_1 <- base::names(this_data)
      str_names_2 <- base::names(df_out)      
      str_names_c <- dplyr::intersect(str_names_1, str_names_2)
      str_names_c <- dplyr::setdiff(str_names_c, by)

      if (length(str_names_c) > 0) { 
        message(paste0("ru_mergedata: common variables (", str_names_c, ") are found in data"))
        if (keepvarsin[1] == "first") {
          this_data <- dplyr::select(this_data, -dplyr::all_of(str_names_c))
        } else {
          df_out <- dplyr::select(df_out, -dplyr::all_of(str_names_c))
        }
      }
      df_out <- base::merge(x = df_out, y = this_data, by = by, all.x = TRUE, all.y=TRUE)
      df_out <- ru_labels(df_out, base::labels(df_out))
      df_out <- ru_labels(df_out, base::labels(this_data))
    }
  }
  print(paste0("RU_MERGEDATA: ", "End of RU_MERGEDATA"))
  return(as.data.frame(df_out))
}



