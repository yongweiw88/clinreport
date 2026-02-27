#' Convert column 
#'
#' Pass in a data along with identification options and have decoded columns derived.
#'
#' @param dsetin Input data frame/tibble.
#' @param col_formats named list for column formats.
#' @param na_str formated value for NA.
#'
#' @return A data frame based on the incoming data frame with values converted to formated values.
#'
#' @author Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com} \cr
#'         Chris Rook, \email{christopher.x.rook@gsk.com}
#'
#' @examples
#'
#' @export
#'
ru_format_dataframe <- function(dsetin, col_formats = NULL, na_str = "") {
  
  dsetin <- as.data.frame(dsetin)  # ensure data.frame for tibbles
  
  # Default formats by class
  default_formats <- list(
    numeric   = "%10.2f",
    double    = "%10.2f",
    integer   = "%6d",
    character = "%-12s",
    factor    = "%-12s",
    ordered   = "%-12s",
    logical   = "%-5s",
    Date      = "%Y%m%d",
    POSIXct   = "%Y-%m-%d %H:%M:%S",
    POSIXlt   = "%Y-%m-%d %H:%M:%S",
    list      = "%-20s",
    default   = "%-12s"
  )
  
  format_col <- function(x, col_name) {
    cls <- class(x)[1]

    # Use user-specified format if provided
    user_fmt <- if (!is.null(col_formats) && col_name %in% names(col_formats)) {
      col_formats[[col_name]]
    } else NULL
    
    # Use existing column values as default format if the column is character and looks like a format string
    # input_fmt <- if (is.character(x) && length(unique(x[!is.na(x)])) == 1 &&
    #                  grepl("%", x[!is.na(x)][1])) {
    #   x[!is.na(x)][1]
    # } else NULL
    input_fmt <- NULL
    
    # Determine which format to use: user > input > class default
    fmt <- user_fmt %||% input_fmt %||% default_formats[[cls]] %||% default_formats$default
    
    # Apply formatting by class
    if (cls %in% c("numeric", "double", "integer")) {
      str_s.out <- ifelse(is.na(x), na_str, sprintf(fmt, x))
    } else if (cls == "logical") {
      str_s.out <- ifelse(is.na(x), na_str, sprintf(fmt, ifelse(x, "Y", "N")))
    } else if (cls == "Date") {
      str_s.out <- ifelse(is.na(x), na_str, format(x, fmt))
    } else if (cls %in% c("POSIXct", "POSIXlt")) {
      str_s.out <- ifelse(is.na(x), na_str, format(x, fmt))
    } else if (cls %in% c("character", "factor", "ordered")) {
      str_s.out <- ifelse(is.na(x), na_str, sprintf(fmt, as.character(x)))
    } else if (cls == "list") {
      str_s.out <- ifelse(sapply(x, is.null), na_str,
                    sprintf(fmt, sapply(x, function(v) paste(v, collapse = ","))))
    } else {
      str_s.out <- ifelse(is.na(x), na_str, sprintf(fmt, as.character(x)))
    }
    
    str_s.out
  }
  
  # Apply formatting to all columns
  df_formatted <- as.data.frame(
    base::mapply(format_col, dsetin, names(dsetin), SIMPLIFY = FALSE),
    stringsAsFactors = FALSE
  )
  
  # Preserve tibble if input was tibble
  if (inherits(dsetin, "tbl_df")) {
    df_formatted <- tibble::as_tibble(df_formatted)
  }
  
  return(df_formatted)
}

