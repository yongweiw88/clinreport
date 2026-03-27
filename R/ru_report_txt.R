#' R function to create text output for clinical reporting.
#'
#' Pass in a dataframe and reporting settings to text output generated.
#'
#' @param dsetin Incoming data frame or list of data frames.
#' @param computebeforepagelines Specifies the text to be produced for the Compute Before Page lines (pairs of label and variable name)
#' @param computebeforepagevars Names of variables that shall define the sort order for Compute Before Page lines
#' @param columns Column parameter
#' @param ordervars Order variables
#' @param descending Descending ORDERVARS
#' @param noprintvars No print vars (usually used to order the display)
#' @param flowvars Variables with flow option
#' @param widths Column widths
#' @param skipvars similar to SAS statement skipvars Break after <var> / skip
#' @param idvars ID variables
#' @param centrevars Centre justify variables
#' @param leftvars Left justify variables
#' @param rightvars Right justify variables
#' @param colspacing Overall spacing value.
#' @param formats Format specification
#' @param labels Label definitions.
#' @param nowidowvar Not in version 1
#' @param sharecolvars Order variables that share print space.
#' @param sharecolvarsindent Indentation factor
#' @param overallsummarylevel Overall summary line at top of tables
#' @param proptions PROC REPORT statement options
#' @param linesizeadjust Lines which will be added to line size.
#' @param pagesizeadjust Lines which will be added to page size
#'
#' @return A text output file
#'
#' @author Chris Rook, \email{christopher.x.rook@gsk.com} \cr
#'         Yongwei Wang, \email{yongwei.x.wang@viivhealthcare.com}
#' 
#' @examples
#'         
#' @importFrom Hmisc label
#' @import dplyr stringr purrr
#' @export
#'
ru_report_txt <- function(
    dsetin=NULL,
    columns=NULL,
    spanningheaders=NULL,                           
    noprintvars=NULL,
    ordervars=NULL,
    descending=NULL,
    nowidowvar=NULL,
    leftvars=NULL,
    rightvars=NULL,
    centrevars=NULL,
    colspacing=0,
    widths=NULL,
    formats=NULL,
    varlabels=NULL,
    skipvars=NULL,
    idvars=NULL,
    sharecolvars=NULL,              
    sharecolvarsindent=2,
    computebeforepagevars=NULL,
    computebeforepagelines=NULL,
    overallsummarylevel=0,
    fontsize=c(rfenv$G_FONTSIZE, "L10", "L09", "L08", "L07", "L11", "L12", "P10", "P09", "P08", "P07", "P11", "P12"),
    proptions=c("headline", "headskip"),
    headers=list("left"=c(paste0("Protocol: ", rfenv$G_STUDY_DESC), paste0("Population: ", rfenv$G_POPLBL)),
                 "right"=c("pageof", dplyr::if_else(is.null(rfenv$G_DATADATE) || rfenv$G_DATADATE[1] == "", "",  paste0("Data Date: ", rfenv$G_DATADATE)))),
    footers=list("left"=c(rfenv$G_FOOT1, rfenv$G_FOOT2, rfenv$G_FOOT3, rfenv$G_FOOT4, rfenv$G_FOOT5, rfenv$G_FOOT6, rfenv$G_FOOT7, rfenv$G_FOOT8, rfenv$G_FOOT9,
                          paste0(rfenv$G_USERID, ": ", rfenv$G_PGMPTH, " ", format(Sys.time(), "%Y-%m-%d %T")))),
    titles=c(paste(c("L"="Listing", "T"="Table", "F"="Figure")["T"], rfenv$G_DSPLYNUM), rfenv$G_TITLE1, rfenv$G_TITLE2, rfenv$G_TITLE3, rfenv$G_TITLE4, rfenv$G_TITLE5, rfenv$G_TITLE6, rfenv$G_TITLE7),
    marginsininch=c("top"=1.25, "bottom"=1, "left"=1, "right"=1),
    linesizeadjust=0,
    pagesizeadjust=0
) {
  
  # print(paste0("ru_report_txt: ", "Start or ru_report_txt"))
  
  max_nested_list_depth <- function(x) {
    if (!is.list(x)) return(0)
    if (length(x) == 0) return(1)
    1 + base::max(base::vapply(x, max_nested_list_depth, 1))
  }
  
  extract_by_level <- function(lst, target_depth, current_depth = 1) {
    nms <- names(lst)
    if (length(nms) == 0) nms <- rep("", length(lst))
    
    if (current_depth == target_depth) {
      # We reached the level we want! 
      # Return headers and all flattened values beneath them
      return(list(
        headers = nms,
        values  = lapply(lst,  function(x) {
          if (!is.list(x)) return(x)
          return(unname(unlist(x)))
        }
        )
      ))
    } else {
      # We need to go deeper. Combine results from all sub-lists
      headers_acc <- character()
      values_acc <- list()
      
      for (item in lst) {
        if (is.list(item)) {
          res <- extract_by_level(item, target_depth, current_depth + 1)
          headers_acc <- c(headers_acc, res$headers)
          values_acc <- c(values_acc, res$values)
        }
      }
      return(list(headers = headers_acc, values = values_acc))
    }
  }
  
  # Define a function wrap and align text
  wrap_align_indent <- function(text, width = 20, align = "left", indent = 0) {
    if (length(text) == 0 || is.na(text) || text == "") return(strrep(" ", indent))
    
    effective_width <- width - indent
    if (effective_width <= 0) stop("Indent is larger than or equal to total width.")
    
    lines <- list()
    remainder <-  base::trimws(text, which = "both")
    
    # 1. Wrap Logic using Substring
    while (nchar(remainder) > 0) {
      # If the remaining text fits, we are done
      if (nchar(remainder) <= effective_width) {
        lines <- c(lines, remainder)
        remainder <- ""
      } else {
        # Look at the character exactly at the boundary (effective_width + 1)
        # We want to know if we are cutting in the middle of a word
        boundary_char <- substr(remainder, effective_width + 1, effective_width + 1)
        
        if (boundary_char == " ") {
          # Perfect break: the character right after the limit is a space
          lines <- c(lines, substr(remainder, 1, effective_width))
          remainder <- sub("^\\s+", "", substr(remainder, effective_width + 1, nchar(remainder)))
        } else {
          # We are in a word. Find the last space BEFORE the boundary
          content_segment <- substr(remainder, 1, effective_width)
          # Regex to find the position of the last space
          last_space <- regexpr(" [^ ]*$", content_segment)
          
          if (last_space != -1) {
            # Break at the last found space
            lines <- c(lines, substr(content_segment, 1, last_space - 1))
            remainder <- sub("^\\s+", "", substr(remainder, last_space + 1, nchar(remainder)))
          } else {
            # Hard Wrap: No space found in the segment (one long word)
            # We must break exactly at the width
            lines <- c(lines, content_segment)
            remainder <- substr(remainder, effective_width + 1, nchar(remainder))
          }
        }
      }
    }
    
    # 2. Alignment and Indent Logic
    final_lines <- sapply(lines, function(l) {
      l_clean <- base::trimws(l, which = "both") # Clean trailing spaces
      n_chars <- nchar(l_clean)
      diff <- effective_width - n_chars
      
      if (align == "right") {
        res <- paste0(base::strrep(" ", diff), l_clean)
      } else if (align == "center") {
        left_pad <- floor(diff / 2)
        res <- paste0(base::strrep(" ", left_pad), l_clean, strrep(" ", diff - left_pad))
      } else {
        res <- paste0(l_clean, base::strrep(" ", max(0, diff)))
      }
      
      return(paste0(base::strrep(" ", indent), res))
    })
    
    return(paste(final_lines, collapse = "\n"))
  }
  
  # Define a function to get index 
  ru_index <- function(...) {
    rtn <- base::which(...)
    if (length(rtn) < 1) rtn <- 0
    else rtn <- rtn[1]
    rtn
  }
  
  if (tolower(Sys.info()["sysname"]) == "windows") {
    str_s.font_name <- "Courier New"
  } else {
    str_s.font_name <- "DejaVuSansMono"
  }
  
  ####################################################################################################
  
  if (overallsummarylevel > length(sharecolvars) - 1) overallsummarylevel <- length(sharecolvars) - 1 
  
  num_n.tmargin <- base::ifelse("top" %in% names(marginsininch), marginsininch["top"], 1.25)
  num_n.bmargin <- base::ifelse("bottom" %in% names(marginsininch), marginsininch["bottom"], 1)
  num_n.lmargin <- base::ifelse("left" %in% names(marginsininch), marginsininch["left"], 1)
  num_n.rmargin <- base::ifelse("right" %in% names(marginsininch), marginsininch["right"], 1)
  
  # print(paste0("ru_report: Get Line size and page size"))
  if (toupper(fontsize[1]) %in% c("L07", "L08", "L09", "L10", "L11", "L12", "P07", "P08", "P09", "P10", "P11", "P12")) {
    str_s.ext_t <- fontsize[1]
  } else if (is.na(suppressWarnings(as.numeric(fontsize[1])))) {
    str_s.ext_t <- fontsize[1]
  } else if (as.numeric(fontsize[1]) < 5) {
    str_s.ext_t <- "L10"
  } else { 
    if (as.numeric(fontsize[1]) >= 10) {
      str_s.ext_t <- paste0("L", fontsize[1]) 
    } else { 
      str_s.ext_t <- paste0("L0", fontsize[1])
    }
  }
  
  # print(paste0("ru_report_txt: Orientation"))
  if (base::substr(str_s.ext_t, 1, 1) == "L") {
    str_s.orientation="landscape"
    num_n.page_height <- 8.5
    num_n.page_width <- 11    
  } else if (base::substr(str_s.ext_t, 1, 1) == "P") {
    str_s.orientation="portrait"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
  } else {
    str_s.orientation="landscape"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
  }
  
  num_n.font_size <- as.numeric(sub(".*?(\\d+).*", "\\1", str_s.ext_t))
  # list_l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  # list_l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  # num_n.pagesize <- unlist(list_l.psmvar[str_s.ext_t])
  # num_n.linesize <- unlist(list_l.lsmvar[str_s.ext_t])
  
  num_n.linesize <- round(((num_n.page_width - num_n.lmargin - num_n.rmargin)/((num_n.font_size * 0.6)/72)), 0)
  num_n.pagesize <- floor(((num_n.page_height - num_n.tmargin - num_n.bmargin)/((num_n.font_size)/72)))
  
  print(c(str_s.ext_t, str_s.orientation, num_n.font_size, num_n.linesize, num_n.pagesize))
  print(c(num_n.page_width, num_n.page_height, num_n.lmargin, num_n.rmargin, num_n.tmargin, num_n.bmargin))
  
  num_n.linesize <- num_n.linesize + linesizeadjust
  
  # headers
  str_s.leftheaders <- if ("left" %in% names(headers)) unlist(headers[["left"]]) else ""
  str_s.rightheaders <- if ("right" %in% names(headers)) unlist(headers[["right"]]) else ""
  str_s.centerheaders <- if ("center" %in% names(headers)) unlist(headers[["center"]]) else ""
  
  num_n.headers <- base::max(length(str_s.rightheaders), length(str_s.leftheaders), length(str_s.centerheaders))
  
  if (num_n.headers == 1 &  base::gsub(" *", "", paste(str_s.rightheaders, str_s.leftheaders, str_s.centerheaders, sep="", collapse="")) == "") num_n.headers <- 0
  
  if (! is.null(str_s.rightheaders) & num_n.headers > length(str_s.rightheaders)) str_s.rightheaders <- c(str_s.rightheaders, base::rep("", num_n.headers - length(str_s.rightheaders)))
  if (! is.null(str_s.leftheaders) & num_n.headers > length(str_s.leftheaders)) str_s.leftheaders <- c(str_s.leftheaders, base::rep("", num_n.headers - length(str_s.leftheaders)))
  if (! is.null(str_s.centerheaders) & num_n.headers > length(str_s.centerheaders)) str_s.centerheaders <- c(str_s.centerheaders, base::rep("", num_n.headers - length(str_s.centerheaders)))
  
  # footers
  str_s.leftfooters <- if ("left" %in% names(footers)) unlist(footers[["left"]]) else ""
  str_s.rightfooters <- if ("right" %in% names(footers)) unlist(footers[["right"]]) else ""
  str_s.centerfooters <- if ("center" %in% names(footers)) unlist(footers[["center"]]) else ""
  
  num_n.footers <- base::max(length(str_s.rightfooters), length(str_s.leftfooters), length(str_s.centerfooters))
  if (num_n.footers == 1 && base::gsub(" *", "", paste0(str_s.rightfooters, str_s.leftfooters, str_s.centerfooters)) == "") num_n.footers <- 0
  
  if (! is.null(str_s.rightfooters) & num_n.footers > length(str_s.rightfooters)) str_s.rightfooters <- c(str_s.rightfooters, base::rep(" ", num_n.footers - length(str_s.rightfooters)))
  if (! is.null(str_s.leftfooters) & num_n.footers > length(str_s.leftfooters)) str_s.leftfooters <- c(str_s.leftfooters, base::rep(" ", num_n.footers - length(str_s.leftfooters)))
  if (! is.null(str_s.centerfooters) & num_n.footers > length(str_s.centerfooters)) str_s.centerfooters <- c(str_s.centerfooters, base::rep(" ", num_n.footers - length(str_s.centerfooters)))
  
  # Variables for graphics
  str_s.titles <- titles

  num_g_ntitles <- num_n.headers + 1 # 1 skip after title.
  num_g_nfooters <- 0
  
  if (length(str_s.titles) > 0) for (i in 1:length(str_s.titles)) {
    if (! str_s.titles[i] %in% c("", " ")) num_g_ntitles <- num_g_ntitles + 1
  }

  num_n.outsidebodylines <- num_g_ntitles + num_n.footers + 2  - pagesizeadjust # 1: line above footer 1: line below title.
  
  if (is.null(dsetin) || base::nrow(dsetin) == 0) {
    df_dsetin <- dplyr::tibble(ggplot__ = " ")
    columns <- "ggplot__"
    varlabels <- list("ggplot__"=" ")
  } else{
    df_dsetin <- dsetin
  }
  
  str_s.columns_all <- ru_expvarlist(df_dsetin, columns)
  str_s.columns_all <- base::intersect(str_s.columns_all, names(df_dsetin))
  computebeforepagevars <- base::intersect(computebeforepagevars, names(df_dsetin))
  ordervars <- base::intersect(ordervars, names(df_dsetin))
  nowidowvar <- base::intersect(nowidowvar, names(df_dsetin))
  leftvars <- base::intersect(leftvars, names(df_dsetin))
  rightvars <- base::intersect(rightvars, names(df_dsetin))
  centrevars <- base::intersect(centrevars, names(df_dsetin))
  skipvars <- base::intersect(skipvars, names(df_dsetin))
  idvars <- base::intersect(idvars, names(df_dsetin))
  sharecolvars <- base::intersect(sharecolvars, names(df_dsetin))              
  # descending <- base::intersect(names(df_dsetin), descending) 
  
  str_s.ordervars <- ordervars
  
  if (length(computebeforepagevars) > 0) {
    noprintvars <- c(computebeforepagevars, noprintvars)
    str_s.ordervars <- c(computebeforepagevars, str_s.ordervars)
    str_s.columns_all <- c(computebeforepagevars, str_s.columns_all)
  }
  
  str_s.ordervars <- intersect(str_s.columns_all, str_s.ordervars)

  if (length(varlabels) > 0 && ! is.na(varlabels[1]) ) {
    df_dsetin <- ru_labels(df_dsetin, varlabels=varlabels)
  }
  str_s.var_labels <- base::lapply(df_dsetin, base::attr, "label")
  
  # print(paste0("ru_report_txt: ORDERVARS: ", str_s.ordervars))
  
  if (! (length(str_s.ordervars) == 0 || is.na(str_s.ordervars[1]) || str_s.ordervars[1] =="")) {
    df_dsetin <- data.table::setorderv(df_dsetin, cols=str_s.ordervars, order=ifelse(str_s.ordervars %in% descending, -1, 1))
  }     
  
  # print("ru_report_txt: Modify COLUMNS") 
  df_dsetin <- ru_format_dataframe(df_dsetin, col_formats = formats, na_str = "")
  df_dsetin <- ru_labels(df_dsetin, varlabels=str_s.var_labels)
  # str_s.columns_1 <- base::setdiff(str_s.columns_all, base::setdiff(noprintvars, sharecolvars))
  
  str_s.columns <- NULL
  num_n.sharecolvars <- 0
  str_s.sharecols <- NULL
  # derive columns in table by removing non-print columns and shared columns
  for (i in 1:length(str_s.columns_all)) {
    str_s.name <-str_s.columns_all[i]
    if (str_s.name %in% sharecolvars && num_n.sharecolvars < length(sharecolvars) - 1) {
      num_n.sharecolvars <- num_n.sharecolvars + 1
      if (length(str_s.sharecols) == 0) str_s.sharecols <- str_s.name
      else str_s.sharecols <- c(str_s.sharecols, str_s.name)
    } else if (! (str_s.name %in% noprintvars)) {
      if ( ! (str_s.name %in% sharecolvars && num_n.sharecolvars >= length(sharecolvars))) {
        if (length(str_s.columns) == 0) str_s.columns <- str_s.name else 
          str_s.columns <- c(str_s.columns, str_s.name)
      }
    }
  }
  
  # derive columns in table by removing nonprint columns
  str_s.rptcolumns <- sharecolvars
  for (i in 1:length(str_s.columns_all)) {
    str_s.name <- str_s.columns_all[i]
    if (! (str_s.name %in% noprintvars) && ! str_s.name %in% sharecolvars) {
      if (length(str_s.rptcolumns) == 0) str_s.rptcolumns <- str_s.name
      else str_s.rptcolumns <- c(str_s.rptcolumns, str_s.name)
    }
  }
  
  # print(paste0("ru_report_txt: Get Widths"))
  # print(str_s.columns)
  if (length(str_s.columns) > length(widths)) num_n.col_char_widths <-  c(widths, rep(15, length(str_s.columns) - length(widths))) else
    num_n.col_char_widths <- widths
  
  # if (length(str_s.columns_all) > length(widths)) num_n.col_char_widths <-  c(widths, rep(15, length(str_s.columns_all) - length(widths))) else
  #   num_n.col_char_widths <- widths
  names(num_n.col_char_widths) <- str_s.columns
  
  if (length(str_s.sharecols) > 0) { 
    if (! (sharecolvars[length(sharecolvars)] %in% names(num_n.col_char_widths))) {
      num_n.col_char_widths[sharecolvars[length(sharecolvars)]] <- num_n.col_char_widths[sharecolvars[length(sharecolvars)]] + (length(sharecolvars) - 1) * sharecolvarsindent
    }
    for (i in 1:length(str_s.sharecols)) {
      num_n.col_char_widths[str_s.sharecols[i]] <- num_n.col_char_widths[sharecolvars[length(sharecolvars)]]
      noprintvars <- c(noprintvars, str_s.sharecols)
    }
  }
  
  str_s.centrevars <- centrevars
  
  if (length(centrevars) > 0) {
    centrevars <- base::intersect(centrevars, columns)
    str_s.centrevars <- NULL
    for (i in 1:length(centrevars)) {
      if (base::any(grepl("^\\s", df_dsetin[[centrevars[i]]]))) str_s.centrevars <- c(str_s.centrevars, centrevars[i])
    }
    str_s.centrevars <- base::setdiff(str_s.centrevars, centrevars)
    if (length(str_s.centrevars) == 0) str_s.centrevars <- NULL
  }
  
  # print(paste0("ru_report_txt: Sharecolvars Label wrapping"))
  
  str_s.line_break <-"\n"
  str_s.sharecolvarlabel <- NULL
  # num_n.colheaderlines <- 0
  
  if (length(sharecolvars) > 0) {
    for (i in 1:length(sharecolvars)) {
      str_s.thislabel <- base::attr(df_dsetin[[sharecolvars[i]]], "label")
      if (is.null(str_s.thislabel)) str_s.thislabel <- ""
      str_s.thislabel <- stringr::str_wrap(str_s.thislabel, width = num_n.col_char_widths[sharecolvars[i]], 
                                           indent = sharecolvarsindent * (i-1), exdent = (i - 1) * sharecolvarsindent, whitespace_only = TRUE)
      if (length(str_s.thislabel) > 0 && str_s.thislabel[1] != "") {
        if (length(str_s.sharecolvarlabel) == 0) str_s.sharecolvarlabel <- str_s.thislabel else
          str_s.sharecolvarlabel <- paste0(str_s.sharecolvarlabel, "\n", str_s.thislabel)
      }
    }
    base::attr(df_dsetin[[sharecolvars[length(sharecolvars)]]], "label") <- str_s.sharecolvarlabel
  } 
  
  str_s.l.wrap_labels <-list()
  for (i in 1:length(str_s.columns)) {
    str_s.this_align <- "left"
    if (str_s.columns[i] %in% c(str_s.centrevars, centrevars)) str_s.this_align <- "center" else
      if (str_s.columns[i] %in% c(rightvars)) str_s.this_align <- "right"
      
    str_s.thislabel_lines_1 <- base::attr(df_dsetin[[str_s.columns[i]]], "label")
    if (is.null(str_s.thislabel_lines_1)) str_s.thislabel_lines_1 <- ""
    str_s.thislabel_lines <- unlist(base::strsplit(str_s.thislabel_lines_1, "\n"))
    str_s.thislabel_lines_1 <- NULL
    for (n in 1:length(str_s.thislabel_lines)) {
      str_s.thislabel_lines_2 <- wrap_align_indent(str_s.thislabel_lines[n], width= num_n.col_char_widths[str_s.columns[i]], align=str_s.this_align, indent=nchar(stringr::str_extract(str_s.thislabel_lines[n], "^\\s*")))
      str_s.thislabel_lines_2 <- stringr::str_replace_all(str_s.thislabel_lines_2, "\n", str_s.line_break)
      str_s.thislabel_lines_1 <- c(str_s.thislabel_lines_1, str_s.thislabel_lines_2)
    }
    str_s.thislabel <- paste(str_s.thislabel_lines_1, collapse = str_s.line_break)
    
    str_s.l.wrap_labels[[str_s.columns[i]]] <-   str_s.thislabel
    # num_n.colheaderlines <- base::max(num_n.colheaderlines, stringr::str_count(str_s.thislabel, "\n") + 1)
  }  
  str_s.var_labels <- str_s.l.wrap_labels
  df_dsetin <- ru_labels(df_dsetin, str_s.l.wrap_labels)
  
  num_n.spanning_headers <- max_nested_list_depth(spanningheaders)
  
  # Number of header lines 
  num_n.colheaderlines <- 0
  for (str_s.this_col in c(str_s.columns)) {
    num_n.spanning_header_lines <- 0
    if (num_n.spanning_headers > 0) for (k in 1:num_n.spanning_headers) {
      this_spanning_headers <- extract_by_level(spanningheaders, k)
      for (l in 1:length(this_spanning_headers[["headers"]])) {
        str_s.this_spanning_cols <- unlist(this_spanning_headers[["values"]][l])
        if (str_s.this_col %in% str_s.this_spanning_cols) {
          num_n.spanning_header_lines <- num_n.spanning_header_lines + 1
          break
        }
      }
    }
    num_n.colheaderlines <- base::max(num_n.colheaderlines, num_n.spanning_header_lines + stringr::str_count(str_s.l.wrap_labels[str_s.this_col], "\n") + 1)
  }
  
  num_n.outsidebodylines <- num_n.outsidebodylines + length(computebeforepagevars)
  num_n.outsidebodylines <- num_n.outsidebodylines + (num_n.colheaderlines + 1) # 1: line under column headers
  if (length(proptions) > 0 && "HEADSKIP" %in% toupper(proptions)) num_n.outsidebodylines <- num_n.outsidebodylines + 1
  if (length(proptions) > 0 && "HEADLINE" %in% toupper(proptions)) num_n.outsidebodylines <- num_n.outsidebodylines + 1
  
  num_n.pagesz <- round(num_n.pagesize - num_n.outsidebodylines, 0) 
  
  if (is.null(dsetin) || base::nrow(dsetin) == 0) {
    df_dsetin <- as.data.frame(dplyr::tibble(ggplot__ = c(base::rep(" ", !! num_n.pagesz))))
    df_dsetin[round((num_n.pagesz)/2, 0), "ggplot__"] <- nodatatoreporttext
    columns <- "ggplot__"
    centrevars <- "ggplot__"
    widths <- num_n.linesize
  } 
  
  # print(paste0("ru_report_txt: Wrap Column Values"))
  # 1. Pre-calculate column alignments to avoid repeated %in% checks
  str_s.this_align <- setNames(rep("left", length(str_s.rptcolumns)), str_s.rptcolumns)
  str_s.this_align[str_s.rptcolumns %in% str_s.centrevars] <- "center"
  str_s.this_align[str_s.rptcolumns %in% rightvars] <- "right"
  
  # 3. Process by column (Vectorized)
  for (col_name in str_s.rptcolumns) {
    this_align <- str_s.this_align[col_name]
    df_col_data <- df_dsetin[[col_name]]
    
    # Determine indents for the whole column at once
    if (col_name %in% centrevars &&  ! col_name %in% str_s.centrevars) {
      num_n.this_indent_extra <- base::max(0, base::floor((num_n.col_char_widths[col_name] - base::max(nchar(stringr::str_replace_all(df_col_data, "\\s+$", ""))))/2))
    } else {
      num_n.this_indent_extra <- 0
    }
    num_n.this_indents <- rep(0, length(df_col_data))
    
    if (length(sharecolvars) > 0 && col_name %in% sharecolvars) {
      n_index <- which(sharecolvars == col_name)
      num_n.this_indents <- (n_index - 1) * sharecolvarsindent
    } else if (col_name %in% centrevars && this_align != "center") {
      num_n.this_indents <- nchar(stringr::str_extract(df_col_data, "^\\s*"))
      num_n.this_indents[is.na(num_n.this_indents)] <- 0
    }
    
    # Apply the wrapping and line break substitution to the whole column
    df_formatted_col <- base::mapply(wrap_align_indent,
      df_col_data, width = num_n.col_char_widths[col_name], 
      align = this_align, indent = num_n.this_indents + num_n.this_indent_extra)
    df_dsetin[[col_name]] <- gsub("\n", str_s.line_break, df_formatted_col)
  }

  # 4. Calculate nlines__ for all rows at once
  # We only count lines for columns that are "visible"
  str_s.visible_cols <- str_s.rptcolumns
  if (length(sharecolvars) > 0) {
    # Logic: bln_b.visible is false if it's in sharecolvars but NOT the last index
    hidden_share_vars <- sharecolvars[-length(sharecolvars)]
    str_s.visible_cols <- setdiff(str_s.rptcolumns, hidden_share_vars)
  }
  
  # Use pmax to find the max line count across visible columns for each row
  list_l.line_counts <- lapply(df_dsetin[str_s.visible_cols], function(column) {
    stringr::str_count(column, stringr::fixed(str_s.line_break)) + 1
  })
  
  df_dsetin$nlines__ <- do.call(base::pmax, as.data.frame(list_l.line_counts))
  
  # Ordervars for nowindowvar calcuation
  str_s.visible_ordervars <- base::intersect(str_s.visible_cols, str_s.ordervars)
  if (!is.null(nowidowvar)) {
    df_dsetin_cat <- df_dsetin
    if (!is.null(str_s.visible_ordervars)) {
      for (i in nrow(df_dsetin)) {
        for (j in (length(str_s.visible_ordervars)):1) {
          num_n.n <- ru_index(str_s.columns_all == str_s.visible_ordervars[j])
          bln_b.changed <- base::any(
            df_dsetin[i, str_s.columns_all[num_n.n]] != df_dsetin[i - 1, str_s.columns_all[num_n.n]]
          )
          
          if (!bln_b.changed) {
            df_dsetin_cat[i, str_s.visible_ordervars[1:j]] <- " "
            break
          }
        } 
      }
    }
    list_l.line_counts_2 <- lapply(df_dsetin_cat[str_s.visible_cols], function(column) {
      stringr::str_count(column, stringr::fixed(str_s.line_break)) + 1
    })
    
    df_dsetin_cat$nlines__ <- do.call(base::pmax, as.data.frame(list_l.line_counts_2))
  }
  
  str_s.skipvars <- skipvars
  if (length(skipvars) > 0 && length(nowidowvar) > 0) {
    str_s.skipvars <- NULL
    for (i in 1:length(skipvars)) {
      num_n.ind <- ru_index(nowidowvar == str_s.columns_all)
      if ( num_n.ind <= ru_index(skipvars[i] == str_s.columns_all)) {
        if (length(str_s.skipvars)== 0) str_s.skipvars <- skipvars[i]
        else str_s.skipvars <- c(str_s.skipvars, skipvars[i])
      } 
    }
  }
  
  str_s.idvars <- idvars
  if (length(intersect(str_s.idvars, sharecolvars)) > 0) str_s.idvars <- intersect(str_s.columns, unique(c(str_s.idvars, sharecolvars)))
  
  str_s.this_cols <- NULL
  str_s.panel_cols <- NULL
  num_n.panel_left_col_char_width <- NULL
  num_n.panel_right_col_char_width <- NULL
  
  num_n.panels <- 1
  num_n.this_length <- base::sum(num_n.col_char_widths[str_s.idvars]) + length(str_s.idvars) * colspacing
  for (str_s.this_col in setdiff(str_s.columns, str_s.idvars)) {
    # print(c(num_n.this_length, num_n.col_char_widths[str_s.this_col], num_n.this_length + num_n.col_char_widths[str_s.this_col]))
    if (length(str_s.this_cols) == 0) str_s.this_cols <- str_s.this_col 
    if (num_n.this_length + num_n.col_char_widths[str_s.this_col] <= num_n.linesize) {
      str_s.this_cols <- c(str_s.this_cols, str_s.this_col)
      num_n.this_length <- num_n.this_length + num_n.col_char_widths[str_s.this_col] + colspacing
    } else {
      num_n.panel_left_col_char_width[num_n.panels] <- floor((num_n.linesize - num_n.this_length)/2)
      num_n.panel_right_col_char_width[num_n.panels] <- max(num_n.linesize - num_n.this_length - num_n.panel_left_col_char_width[num_n.panels],  0)
      str_s.panel_cols[[num_n.panels]] <- base::unique(c(str_s.idvars, str_s.this_cols))
      num_n.this_length <- sum(num_n.col_char_widths[str_s.idvars]) + length(str_s.idvars) * colspacing + num_n.col_char_widths[str_s.this_col] + colspacing
      str_s.this_cols <- str_s.this_col
      num_n.panels <- num_n.panels + 1
    }
  }
  
  num_n.panel_left_col_char_width[num_n.panels] <- floor((num_n.linesize - num_n.this_length )/2)
  num_n.panel_right_col_char_width[num_n.panels] <- max(num_n.linesize - num_n.this_length - num_n.panel_left_col_char_width[num_n.panels],  0)
  str_s.panel_cols[[num_n.panels]] <- base::unique(c(str_s.idvars, str_s.this_cols)) 
  
  print(c(num_n.linesize, num_n.this_length))
  print(num_n.panel_left_col_char_width)
  print(str_s.panel_cols)
  print(num_n.col_char_widths)
  
  # df_lastdset1 <- df_dsetin
  
  ############################################################################################################################
  ############################################################################################################################
  # 
  # print(paste0("ru_report: Add page number to data row"))  
  
  num_n.pagenumber <- 0
  num_n.linenumber <- 0
  num_n.addlines <- 0
  num_n.numofpages <- 1
  num_n.catlines <- 0
  str_s.sharecolvarvalue <- NULL
  num_n.sharecolvars_level <- 0
  num_n.overall_level <- length(sharecolvars)
  num_n.overall_level_1 <- length(sharecolvars)
  
  # df_dsetin <- df_lastdset1
  df_dsetin_1 <- NULL
  list_df_rows_list <- list()
  
  print(paste0("ru_report: num_n.pagesz: ", num_n.pagesz, "\tnum_n.pagesize", num_n.pagesize))
  
  for (i in 1:base::nrow(df_dsetin)) {
    # for (i in 1:40) {
    # num_n.catlines <- 0
    bln_b.newpage <- FALSE
    bln_b.newcompute <- FALSE

    if (i == 1) {
      bln_b.newpage <- TRUE
      bln_b.newcompute <- TRUE
    }
    
    if (i > 1 && length(computebeforepagevars) > 0) {
      for (j in 1:length(computebeforepagevars)) {
        if (df_dsetin[i, computebeforepagevars[j]] != df_dsetin[i - 1, computebeforepagevars[j]]) {
          bln_b.newcompute <- TRUE
          bln_b.newpage <- TRUE
          break
        }
      }
    } 
    
    for (n in 1:2) {
      bln_b.nowidowchk <- FALSE    
      num_n.addlines <- 0
      bln_b.addline <- FALSE
      
      # sharecolvars
      if (length(sharecolvars) > 0) {
        num_n.sharecolvars_level <- 0
        num_n.overall_level <- length(sharecolvars)
        if (i == 1 || bln_b.newcompute) { 
          num_n.sharecolvars_level <- length(sharecolvars) - 1
          num_n.overall_level <- 0
        }  else { 
          for (j in 1:(length(sharecolvars) - 1)) {
            num_n.n <- ru_index(str_s.columns_all == sharecolvars[j])
            if (sharecolvars[j] %in% str_s.ordervars) for (l in 1:num_n.n) {
              if (df_dsetin[i, str_s.columns_all[l]] != df_dsetin[i - 1, str_s.columns_all[l]]) {
                num_n.sharecolvars_level <- length(sharecolvars) - j
                num_n.overall_level <- j
                break
              } 
            } else {
              num_n.sharecolvars_level <- length(sharecolvars) - j
              num_n.overall_level <- j
              break
            }
            if (num_n.sharecolvars_level > 0 ) break 
          }
        } 
        if (bln_b.newpage) num_n.sharecolvars_level <- length(sharecolvars) - 1
        if (num_n.overall_level <  length(sharecolvars) - 1) num_n.overall_level_1 <- num_n.overall_level
        if (num_n.overall_level > overallsummarylevel) num_n.overall_level <- length(sharecolvars)
        
        if (num_n.sharecolvars_level > 0) {
          str_s.sharecolvarvalue <- NULL  
          num_n.n <- length(sharecolvars) - num_n.sharecolvars_level - 1
          for (j in (length(sharecolvars) - num_n.sharecolvars_level):(length(sharecolvars) -  1)) {
            if (j <= num_n.overall_level || j == length(sharecolvars) - 1) {
              num_n.n <- num_n.n + 1
              if (is.null(str_s.sharecolvarvalue)) {
                str_s.sharecolvarvalue <- base::substring(df_dsetin[i, sharecolvars[j]], sharecolvarsindent * (j - num_n.n) + 1)
              } else {
                str_s.sharecolvarvalue <- paste0(str_s.sharecolvarvalue, str_s.line_break, 
                                                 base::substring(df_dsetin[i, sharecolvars[j]], sharecolvarsindent * (j - num_n.n) + 1))
              }
            }
          }
          if (! is.null(str_s.sharecolvarvalue)) {
            num_n.numoflines1 <- stringr::str_count(str_s.sharecolvarvalue, str_s.line_break) + 1
            num_n.addlines <- num_n.addlines + num_n.numoflines1
            bln_b.addline <- TRUE
          }
        }
      }
      
      num_n.nowidowrows <- 1
      bln_b.nowidowchk <- FALSE
      if (length(nowidowvar) > 0 & nrow(df_dsetin) > 1) {
        num_n.n <- ru_index(str_s.columns_all == nowidowvar)
        if (i > 1) for (l in 1:num_n.n) {
          if (df_dsetin[i, str_s.columns_all[l]] != df_dsetin[i - 1, str_s.columns_all[l]]) {
            bln_b.nowidowchk <- TRUE
            break
          } 
        } else bln_b.nowidowchk <- TRUE 
        
        bln_b.nowidowchk2 <- FALSE
        if (bln_b.nowidowchk & nrow(df_dsetin) >= i + 1) for (j in (i + 1):nrow(df_dsetin)) {
          for (l in 1:num_n.n) {
            if (df_dsetin[i, str_s.columns_all[l]] != df_dsetin[j, str_s.columns_all[l]]) {
              bln_b.nowidowchk2 <- TRUE
              break
            } 
          }
          
          if (bln_b.nowidowchk2) break
          num_n.nowidowrows <- num_n.nowidowrows + 1
        }
      }
      
      # nowidowvar and skipvars
      if ( length(nowidowvar) > 0 && (bln_b.newcompute || bln_b.nowidowchk)) {
        num_n.catlines <- num_n.addlines
        
        for (j in 1:num_n.nowidowrows) {
          num_n.catlines <- num_n.catlines + ifelse(bln_b.newcompute, df_dsetin[i + j - 1, "nlines__"], df_dsetin_cat[i + j - 1, "nlines__"])
            
          if (! is.null(str_s.skipvars)) for (k in 1:length(str_s.skipvars)) {
            if ((ru_index(str_s.columns_all == str_s.skipvars[k]) > ru_index(str_s.columns_all == nowidowvar)) &
                j < num_n.nowidowrows) {
              num_n.index <- ru_index(str_s.columns_all == str_s.skipvars[k])
              if (i + j - 1 < nrow(df_dsetin)) for (l in 1:num_n.index) { 
                if (df_dsetin[i + j - 1, str_s.columns_all[l]] != df_dsetin[i + j, str_s.columns_all[l]]) {
                  num_n.catlines <- num_n.catlines + 1
                  break
                }
              }
            }
          }
        }
      }
      
      # if (i > 28) {
      #   print("debug here")
      #   browser()
      # }
      
      if (n > 1) break
      
      if (bln_b.newcompute) bln_b.newpage <- TRUE else 
        if ( num_n.linenumber > 3 &&  length(nowidowvar) > 0 && bln_b.nowidowchk && num_n.catlines >= num_n.pagesz - num_n.linenumber + 2) bln_b.newpage <- TRUE else 
          if (bln_b.addline && (num_n.addlines + df_dsetin[i, "nlines__"] >= num_n.pagesz - num_n.linenumber + 2)) bln_b.newpage <- TRUE else 
            if (df_dsetin[i, "nlines__"] >= num_n.pagesz - num_n.linenumber + 2) bln_b.newpage <- TRUE else 
              if (i > 1) bln_b.newpage <- FALSE
      
      if (! bln_b.newpage | bln_b.newcompute) break
    }
    
    # Add extra lines to fill all rows on a page
    if (i > 1 && bln_b.newpage && (num_n.pagesz - num_n.linenumber + 1 > 0)) {
      for (k in 1:(num_n.pagesz - num_n.linenumber + 1 )) {
        df_dsetin_2 <- list_df_rows_list[[1]][0, ] 
        df_dsetin_2[1, "nlines__"] <- 1
        df_dsetin_2[1, "linenum__"] <- num_n.linenumber
        df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber   
        df_dsetin_2[1, "newpage__"] <- FALSE
        df_dsetin_2[1, "addline__"] <- TRUE
        df_dsetin_2[1, "sharelvl__"] <- num_n.sharecolvars_level
        df_dsetin_2[1, "overalllvl__"] <- num_n.overall_level
        df_dsetin_2[1, "catline__"] <- num_n.catlines
        
        df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
        list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
        num_n.linenumber <- num_n.linenumber + 1
      }
    }
    
    if (bln_b.newpage) {
      num_n.pagenumber <- num_n.pagenumber + 1
      num_n.linenumber <- 1
      num_n.sharecolvars_level <- length(sharecolvars) - 1
      if (length(computebeforepagevars) > 0) {
        num_n.linenumber <- num_n.linenumber + length(computebeforepagevars)
      }
    } 
    
    if (num_n.sharecolvars_level > 0) {
      df_dsetin_2 <- df_dsetin[i, ]
      df_dsetin_2[1, "linenum__"] <- num_n.linenumber
      df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber   
      df_dsetin_2[1, "newpage__"] <- bln_b.newpage
      df_dsetin_2[1, "addline__"] <- bln_b.addline
      df_dsetin_2[1, "sharelvl__"] <- num_n.sharecolvars_level
      df_dsetin_2[1, "overalllvl__"] <- num_n.overall_level
      df_dsetin_2[1, "catline__"] <- num_n.catlines
      
      num_n.index <- ru_index(sharecolvars[length(sharecolvars)] == str_s.columns_all)
      for (j in 1:length(str_s.columns_all)) {
        if (j > num_n.index) df_dsetin_2[1, str_s.columns_all[j]] <- " "
      }
      if (! is.null(str_s.sharecolvarvalue)) {
        df_dsetin_2[1, sharecolvars[length(sharecolvars)]]  <- str_s.sharecolvarvalue
        num_n.linenumber <- num_n.linenumber + num_n.addlines
        df_dsetin_2[1, "nlines__"] <- num_n.addlines
        list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
      }
    } 
    
    df_dsetin_2 <- df_dsetin[i, ]
    if (num_n.overall_level_1 < length(sharecolvars) & overallsummarylevel > num_n.overall_level_1) {
      df_dsetin_2[1, sharecolvars[length(sharecolvars)]] <- base::substring(df_dsetin_2[1, sharecolvars[length(sharecolvars)]],
                                                                            sharecolvarsindent * (overallsummarylevel - num_n.overall_level_1 - 1) + 1)
      
      if ( is.null(str_s.ordervars) | bln_b.newpage) {
        list_l.line_counts_1 <- lapply(df_dsetin_2[str_s.visible_cols], function(column) {
          stringr::str_count(column, stringr::fixed(str_s.line_break)) + 1
        })
        
        df_dsetin_2$nlines__ <- do.call(base::pmax, as.data.frame(list_l.line_counts_1))
      }
    }
    
    # Ordervars
    if (! is.null(str_s.ordervars) & (! bln_b.newpage)) {
      for (j in (length(str_s.visible_ordervars)):1) {
        num_n.n <- ru_index(str_s.columns_all == str_s.visible_ordervars[j])
        bln_b.changed <- base::any(
          df_dsetin[i, str_s.columns_all[num_n.n]] != df_dsetin[i - 1, str_s.columns_all[num_n.n]]
        )
        
        if (!bln_b.changed) {
          df_dsetin_2[1, str_s.visible_ordervars[1:j]] <- " "
          break
        }
      } 
      list_l.line_counts_1 <- lapply(df_dsetin_2[str_s.visible_cols], function(column) {
        stringr::str_count(column, stringr::fixed(str_s.line_break)) + 1
      })
      df_dsetin_2$nlines__ <- do.call(base::pmax, as.data.frame(list_l.line_counts_1))
    }
    
    if (bln_b.newpage & num_n.sharecolvars_level > 0 & ! is.null(str_s.sharecolvarvalue)) bln_b.newpage <- FALSE
    
    df_dsetin_2[1, "linenum__"] <- num_n.linenumber
    df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber   
    df_dsetin_2[1, "newpage__"] <- bln_b.newpage
    df_dsetin_2[1, "addline__"] <- FALSE
    df_dsetin_2[1, "sharelvl__"] <- num_n.sharecolvars_level
    df_dsetin_2[1, "overalllvl__"] <- num_n.overall_level
    df_dsetin_2[1, "catline__"] <- num_n.catlines
    
    # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_2)  
    list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2

    num_n.linenumber <- num_n.linenumber + unlist(df_dsetin[i, "nlines__"])

    # Add skip lines
    if (length(str_s.skipvars) > 0 & num_n.linenumber + length(str_s.skipvars) <= num_n.pagesz) for (k in 1:length(str_s.skipvars)) {
      num_n.index <- ru_index(str_s.columns_all == str_s.skipvars[k]) 

      if ( i < nrow(df_dsetin)) for (l in 1:num_n.index) {
        if (df_dsetin[i, str_s.columns_all[l]] != df_dsetin[i + 1, str_s.columns_all[l]]) {
          df_dsetin_2 <- list_df_rows_list[[1]][0, ]
          df_dsetin_2[1, "nlines__"] <- 1
          df_dsetin_2[1, "linenum__"] <- num_n.linenumber
          df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber
          df_dsetin_2[1, "newpage__"] <- FALSE
          df_dsetin_2[1, "addline__"] <- TRUE
          df_dsetin_2[1, "sharelvl__"] <- num_n.sharecolvars_level
          df_dsetin_2[1, "overalllvl__"] <- num_n.overall_level
          df_dsetin_2[1, "catline__"] <- num_n.catlines
          df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
          # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_2)
          list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
          num_n.linenumber <- num_n.linenumber + 1
          break
        }
      }
    } 
  }
  
  # Add lines to end of the page
  if (num_n.pagesz - num_n.linenumber + 1 > 0) {
    for (k in 1:(num_n.pagesz - num_n.linenumber + 1)) {
      df_dsetin_2 <- list_df_rows_list[[1]][0, ]
      df_dsetin_2[1, "nlines__"] <- 1
      df_dsetin_2[1, "linenum__"] <- num_n.linenumber
      df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber
      df_dsetin_2[1, "newpage__"] <- FALSE
      df_dsetin_2[1, "addline__"] <- FALSE
      df_dsetin_2[1, "sharelvl__"] <- num_n.sharecolvars_level
      df_dsetin_2[1, "overalllvl__"] <- num_n.overall_level
      df_dsetin_2[1, "catline__"] <- num_n.catlines
      df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
      list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
      num_n.linenumber <- num_n.linenumber + 1
    }
  }
  
  df_dsetin_1 <- dplyr::bind_rows(list_df_rows_list)
  df_dsetin_1 |> dplyr::filter(pagenum__==1)
  
  df_dsetin <- ru_labels(df_dsetin_1, str_s.var_labels) |> dplyr::select(-catline__, -addline__, -overalllvl__, -sharelvl__, -newpage__)
  
  str_s.columns_all <- c("pagenum__", str_s.columns_all)
  str_s.ordervars <- c(str_s.ordervars, "pagenum__")
  
  num_n.numofpages <- num_n.pagenumber
  noprintvars <- c(noprintvars, "pagenum__")
  
  # print(paste0("ru_report: Start to output file"))

  ############################################################################################################################
  ############################################################################################################################
  
  # df_dsetin <- df_lastdset
  
  str_s.all_lines <- NULL
  
  for (i in 1:num_n.numofpages) {
    str_s.computebeforepagelines <- NULL
    if (length(computebeforepagevars) > 0) {
      for (j in 1:length(computebeforepagevars)) {
        if (j==1) {
          str_s.computebeforepagelines <- paste0(computebeforepagelines[(j-1) * 2 + 1], unlist(df_dsetin %>% 
            dplyr::filter(pagenum__ == !! i) %>% dplyr::select(!!! rlang::syms(computebeforepagelines[(j-1) * 2 + 2]))
            %>% dplyr::distinct())[1])
        } else {
          str_s.computebeforepagelines <- c(str_s.computebeforepagelines, paste0(computebeforepagelines[(j-1) * 2 + 1], unlist(df_dsetin %>% 
            dplyr::filter(pagenum__ == !! i) %>% dplyr::select(!!! rlang::syms(computebeforepagelines[(j-1) * 2 + 2])) 
            %>% dplyr::distinct())[1]))
        }
      }
    }

    for (j in 1:num_n.panels) {
      num_n.page <- (i - 1) * num_n.panels + j
      # headers
      str_s.this_pagebreak <- base::ifelse(num_n.page > 1, "\f", "")
      
      if (num_n.headers > 0) for (k in 1:num_n.headers) {
        str_s.tmp_left <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.leftheaders[k], fixed = TRUE)
        str_s.tmp_right <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.rightheaders[k], fixed = TRUE)
        str_s.tmp_center <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.centerheaders[k], fixed = TRUE)
        
        str_s.tmp <- paste0(str_s.tmp_left, base::strrep(" ", num_n.linesize))
        if (! str_s.tmp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.linesize - base::nchar(str_s.tmp_center))/2)), str_s.tmp_center)
        if (str_s.tmp_right %in% c("", " ")) str_s.tmp_right <- ""
        str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.linesize - base::nchar(str_s.tmp_right)),  str_s.tmp_right)
        str_s.all_lines <- c(str_s.all_lines,  paste0(str_s.this_pagebreak, base::substr(str_s.tmp, 1, num_n.linesize)))
        str_s.this_pagebreak <- ""
      } 
      if (length(str_s.titles) > 0) {
        for (k in 1:length(str_s.titles)) {
          str_s.tmp <- base::strrep(" ", num_n.linesize)
          str_s.tmp_center <- str_s.titles[k]
          str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.linesize - base::nchar(str_s.tmp_center))/2)), str_s.tmp_center)
          str_s.all_lines <- c(str_s.all_lines, paste0(str_s.this_pagebreak, base::substr(str_s.tmp, 1, num_n.linesize)))
          str_s.this_pagebreak <- ""
        }
      }

      str_s.all_lines <- c(str_s.all_lines, " ")
      if (length(str_s.computebeforepagelines) > 0) for (k in 1:length(str_s.computebeforepagelines)) {
        str_s.all_lines <- c(str_s.all_lines, paste0(str_s.this_pagebreak, str_s.computebeforepagelines[k]))
        str_s.this_pagebreak <- ""
      }
      
      df_this_dset <- df_dsetin %>% dplyr::filter(pagenum__ == !! i) 
      str_s.this_cols <- base::unique(str_s.panel_cols[[j]])
      
      if (length(proptions) > 0 && "HEADLINE" %in% toupper(proptions)) {
        full_line <- paste0(base::strrep(" ", num_n.panel_left_col_char_width[j]), base::strrep("_", num_n.linesize - num_n.panel_left_col_char_width[j] - num_n.panel_right_col_char_width[j]))
        str_s.all_lines <- c(str_s.all_lines, paste0(str_s.this_pagebreak, full_line)) 
        str_s.this_pagebreak <- ""
      } else {
        # str_s.all_lines <- c(str_s.all_lines, " ") 
        str_s.this_pagebreak <- ""       
      }
      
      str_s.right_pad <- base::strrep(" ", num_n.pagesize)
      
      if (num_n.colheaderlines > 0) {      
        str_s.header_lines <- base::rep(base::strrep(" ", num_n.linesize), num_n.colheaderlines)
        if (num_n.spanning_headers > 0) {
          for (k in 1:num_n.spanning_headers) {
            full_line <- ""
            this_spanning_headers <- extract_by_level(spanningheaders, k)
            for (l in 1:length(this_spanning_headers[["headers"]])) {
              str_s.this_col_value <- this_spanning_headers[["headers"]][l]
              str_s.this_spanning_cols <- unlist(this_spanning_headers[["values"]][l])
              num_n.this_location <- num_n.panel_left_col_char_width[j] + 1
              for (str_s.this_col in c(str_s.this_cols)) {
                if (str_s.this_col %in% str_s.this_spanning_cols) break else
                    num_n.this_location <- num_n.this_location + num_n.col_char_widths[str_s.this_col] + colspacing
              }
              num_n.this_span_with <- base::sum(0, num_n.col_char_widths[str_s.this_spanning_cols], na.rm=TRUE) + length(str_s.this_spanning_cols) * colspacing
              if (num_n.this_span_with > 0) {
                if (nchar(full_line) < 1) full_line <- base::strrep(" ", num_n.this_location - 1)
                if (nchar(str_s.this_col_value) < 1) str_s.this_col_value <- base::strrep(" ", num_n.this_span_with)
                str_s.left_pad <- base::strrep("_", max(0, floor(num_n.this_span_with - nchar(str_s.this_col_value))/2))
                str_s.this_col_value <- base::substr(paste0(str_s.left_pad, str_s.this_col_value, stringr::str_replace_all(str_s.right_pad, " ", "_")), 1, num_n.this_span_with)
                full_line <- base::substring(paste0(full_line, str_s.this_col_value), 1, num_n.linesize - num_n.panel_right_col_char_width[j])    
              }
            }
            str_s.header_lines[k] <- full_line 
          }
        }

        num_n.this_location <- num_n.panel_left_col_char_width[j] + 1
        
        for (str_s.this_col in c(str_s.this_cols)) {
          # Column headers
          str_s.this_col_values <- unlist(base::strsplit(unlist(str_s.var_labels[[str_s.this_col]]), "\n"))
          for (m in 1:length(str_s.this_col_values)) {
            num_n.n <- num_n.colheaderlines - length(str_s.this_col_values) + m
            str_s.header_lines[num_n.n] <- paste0(base::substring(str_s.header_lines[num_n.n], 1, num_n.this_location - 1 ), 
                                                  base::substring(paste0(str_s.this_col_values[m], str_s.right_pad), 1, num_n.col_char_widths[str_s.this_col]),
                                                  base::substring(str_s.header_lines[num_n.n], num_n.this_location + num_n.col_char_widths[str_s.this_col], num_n.linesize))             
          }
          num_n.this_location <- num_n.this_location + num_n.col_char_widths[str_s.this_col] + colspacing
        } 
      
        str_s.header_lines[1] <- paste0(str_s.this_pagebreak, str_s.header_lines[1])
        str_s.this_pagebreak <- ""
        str_s.all_lines <- c(str_s.all_lines, str_s.header_lines)
      }
      
      full_line <- paste0(base::strrep(" ", num_n.panel_left_col_char_width[j]), base::strrep("_", num_n.linesize - num_n.panel_left_col_char_width[j] - num_n.panel_right_col_char_width[j]))
      # full_line <- paste0(base::strrep(" ", num_n.panel_left_col_char_width[j]), base::substr(base::strrep("_", num_n.linesize), num_n.panel_left_col_char_width[j] + 1, num_n.this_location))
      str_s.all_lines <- c(str_s.all_lines, paste0(str_s.this_pagebreak, full_line)) 
      str_s.this_pagebreak <- ""
      
      if (length(proptions) > 0 && "HEADSKIP" %in% toupper(proptions)) str_s.all_lines <- c(str_s.all_lines, " ") 
      
      for (k in seq_len(nrow(df_this_dset))) {
        # column values
        for (l in 1:df_this_dset[k, "nlines__"]) {
          full_line <- ""
          if (num_n.panel_left_col_char_width[j] > 0) full_line <- base::strrep(" ", num_n.panel_left_col_char_width[j])
          num_n.this_location <- num_n.panel_left_col_char_width[j]
          for (str_s.this_col in c(str_s.this_cols)) {
            num_n.this_location <- num_n.this_location + num_n.col_char_widths[str_s.this_col] + colspacing
            str_s.this_col_values <- unlist(base::strsplit(unlist(df_this_dset[k, str_s.this_col]), "\n"))
            if (length(str_s.this_col_values) >= l) str_s.this_col_value <-str_s.this_col_values[l] else
              str_s.this_col_value <- " "
            full_line <- base::substr(paste0(full_line, str_s.this_col_value, str_s.right_pad), 1, num_n.this_location)
          } 
          str_s.all_lines <- c(str_s.all_lines, full_line) 
        }
      }
      
      # footnotes
      
      if (num_n.footers > 0) for (k in 1:num_n.footers) {
        if (k == 1)  str_s.all_lines <- c(str_s.all_lines, " ") # Add a line above the footer
        str_s.tmp_left <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.leftfooters[k], fixed = TRUE)
        str_s.tmp_right <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.rightfooters[k], fixed = TRUE)
        str_s.tmp_center <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.centerfooters[k], fixed = TRUE)
        
        str_s.tmp <- paste0(str_s.tmp_left, base::strrep(" ", num_n.linesize))
        if (! str_s.tmp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.linesize - base::nchar(str_s.tmp_center)/2))), str_s.tmp_center)
        if (! str_s.tmp_right %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.linesize - base::nchar(str_s.tmp_right)),  str_s.tmp_right)
        str_s.all_lines <- c(str_s.all_lines, base::substr(str_s.tmp, 1, num_n.linesize))
      }   
    }
  }
  
  return(str_s.all_lines)
}
