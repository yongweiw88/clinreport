#' R function to create RTF/PDF output for clinical reporting.
#'
#' Pass in a dataframe and reporting settings to RTF/PDF output generated.
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
#' @param overallsummary Overall summary line at top of tables
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

ru_report <- function(
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
    overallsummary=FALSE,
    proptions=c("noheadline", "noheadskip"),
    fontsize=c(rfenv$G_FONTSIZE, "L10", "L09", "L08", "L07", "L11", "L12", "P10", "P09", "P08", "P07", "P11", "P12"),
    headers=list("left"=c(paste0("Protocol: ", rfenv$G_STUDY_DESC), paste0("Population: ", rfenv$G_POPLBL)),
                 "right"=c("pageof", dplyr::if_else(is.null(rfenv$G_DATADATE) || rfenv$G_DATADATE[1] == "", "",  paste0("Data Date: ", rfenv$G_DATADATE)))),
    footers=list("left"=c(rfenv$G_FOOT1, rfenv$G_FOOT2, rfenv$G_FOOT3, rfenv$G_FOOT4, rfenv$G_FOOT5, rfenv$G_FOOT6, rfenv$G_FOOT7, rfenv$G_FOOT8, rfenv$G_FOOT9,
                          paste0(rfenv$G_USERID, ": ", rfenv$G_PGMPTH, " ", format(Sys.time(), "%Y-%m-%d %T")))),
    titles=c(paste(c("L"="Listing", "T"="Table", "F"="Figure")["T"], rfenv$G_DSPLYNUM), rfenv$G_TITLE1, rfenv$G_TITLE2, rfenv$G_TITLE3, rfenv$G_TITLE4, rfenv$G_TITLE5, rfenv$G_TITLE6, rfenv$G_TITLE7),
    titlelinebreakchar="\a",
    titleblanku8char="\u0A00",
    linesizeadjust=0,
    pagesizeadjust=0
) {
  
  # print(paste0("ru_report: ", "Start or ru_report"))
  
  nodatatoreporttext <- "No data to report"

  num_n.tmargin <- 1.25
  num_n.bmargin <- 1
  num_n.lmargin <- 1
  num_n.rmargin <- 1
  
  list_l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  list_l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  
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
  
  # print(paste0("ru_report: Orientation"))
  if (base::substr(str_s.ext_t, 1, 1) == "L") {
    s.orientation="landscape"
    num_n.page_height <- 8.5
    num_n.page_width <- 11    
  } else if (base::substr(str_s.ext_t, 1, 1) == "P") {
    s.orientation="portrait"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
  } else {
    s.orientation="landscape"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
  }
  
  num_n.fontsize <- as.numeric(sub(".*?(\\d+).*", "\\1", str_s.ext_t))

  num_n.linesize <- unlist(list_l.lsmvar[str_s.ext_t])
  num_n.pagesize <- unlist(list_l.psmvar[str_s.ext_t])
  str_s.hold_space <- titleblanku8char # character to replace " " in titles, headers and footnotes to keep alignment.

  num_n.linesize <- num_n.linesize + linesizeadjust

  str_s.padding_default <- "0pt"
  str_s.fontsize_default <- paste0(num_n.fontsize, "pt")

  #
  # With this two options, column label upper and lower borders are hidden for RTF. but doesn't work for PDF.
  # 
  str_s.border_width_def1 <- "0pt"
  str_s.default_border_style1 <- "none"

  str_s.border_width_def <- "0pt"
  str_s.default_border_style <- "none"
  
  # str_s.border_width_def <- "2px"
  # str_s.default_border_style <- "solid"
  
  # str_s.border_width_def <- NULL
  # str_s.default_border_style <- NULL
  
  if (tolower(Sys.info()["sysname"]) == "windows") {
    str_s.font_name <- "Courier New"
    str_s.font_name2 <- "Courier New"
  } else {
    str_s.font_name <- "DejaVu Sans Mono"
    str_s.font_name2 <- "DejaVuSansMono"
  }
  
  list_tab_options <- list(
    table.width = "648pt",
    table.layout = NULL,
    table.align = "center",
    table.margin.left = "0pt",
    table.margin.right = "0pt",
    table.background.color = NULL,
    table.additional_css = NULL,
    table.font.names = str_s.font_name,
    table.font.size = str_s.fontsize_default,
    table.font.weight = NULL,
    table.font.style = NULL,
    table.font.color = NULL,
    table.font.color.light = NULL,
    table.border.top.style = str_s.default_border_style1,
    table.border.top.width = str_s.border_width_def1,
    table.border.top.color = NULL,
    table.border.right.style = str_s.default_border_style1,
    table.border.right.width = str_s.border_width_def1,
    table.border.right.color = NULL,
    table.border.bottom.style = str_s.default_border_style1,
    table.border.bottom.width = str_s.border_width_def1,
    table.border.bottom.color = NULL,
    table.border.left.style = str_s.default_border_style1,
    table.border.left.width = str_s.border_width_def1,
    table.border.left.color = NULL,
    heading.background.color = NULL,
    heading.align = "left",
    heading.title.font.size = str_s.fontsize_default,
    heading.title.font.weight = NULL,
    heading.subtitle.font.size = str_s.fontsize_default,
    heading.subtitle.font.weight = NULL,
    heading.padding = str_s.padding_default,
    heading.padding.horizontal = str_s.padding_default,
    heading.border.bottom.style = str_s.default_border_style1,
    heading.border.bottom.width = str_s.border_width_def1,
    heading.border.bottom.color = NULL,
    heading.border.lr.style = NULL,
    heading.border.lr.width = NULL,
    heading.border.lr.color = NULL,
    column_labels.background.color = NULL,
    column_labels.font.size = str_s.fontsize_default,
    column_labels.font.weight = NULL,
    column_labels.text_transform = NULL,
    column_labels.padding = str_s.padding_default,
    column_labels.padding.horizontal = str_s.padding_default,
    column_labels.vlines.style = NULL,
    column_labels.vlines.width = NULL,
    column_labels.vlines.color = NULL,
    column_labels.border.top.style = str_s.default_border_style,
    column_labels.border.top.width = str_s.border_width_def,
    column_labels.border.top.color = NULL,
    column_labels.border.bottom.style = str_s.default_border_style,
    column_labels.border.bottom.width = str_s.border_width_def,
    column_labels.border.bottom.color = NULL,
    column_labels.border.lr.style = NULL,
    column_labels.border.lr.width = NULL,
    column_labels.border.lr.color = NULL,
    column_labels.hidden = NULL,
    column_labels.units_pattern = NULL,
    row_group.background.color = NULL,
    row_group.font.size = str_s.fontsize_default,
    row_group.font.weight = NULL,
    row_group.text_transform = NULL,
    row_group.padding = str_s.padding_default,
    row_group.padding.horizontal = str_s.padding_default,
    row_group.border.top.style = str_s.default_border_style,
    row_group.border.top.width = NULL,
    row_group.border.top.color = NULL,
    row_group.border.bottom.style = str_s.default_border_style,
    row_group.border.bottom.width = NULL,
    row_group.border.bottom.color = NULL,
    row_group.border.left.style = NULL,
    row_group.border.left.width = NULL,
    row_group.border.left.color = NULL,
    row_group.border.right.style = NULL,
    row_group.border.right.width = NULL,
    row_group.border.right.color = NULL,
    row_group.default_label = NULL,
    row_group.as_column = NULL,
    table_body.hlines.style = str_s.default_border_style1,
    table_body.hlines.width = str_s.border_width_def1,
    table_body.hlines.color = NULL,
    table_body.vlines.style = str_s.default_border_style1,
    table_body.vlines.width = str_s.border_width_def1,
    table_body.vlines.color = NULL,
    table_body.border.top.style = str_s.default_border_style1,
    table_body.border.top.width = str_s.border_width_def1,
    table_body.border.top.color = NULL,
    table_body.border.bottom.style = str_s.default_border_style,
    table_body.border.bottom.width = str_s.border_width_def,
    table_body.border.bottom.color = NULL,
    stub.background.color = NULL,
    stub.font.size = str_s.fontsize_default,
    stub.font.weight = NULL,
    stub.text_transform = NULL,
    stub.border.style = str_s.default_border_style1,
    stub.border.width = str_s.border_width_def1,
    stub.border.color = NULL,
    stub.indent_length = NULL,
    stub_row_group.font.size = str_s.fontsize_default,
    stub_row_group.font.weight = NULL,
    stub_row_group.text_transform = NULL,
    stub_row_group.border.style = str_s.default_border_style1,
    stub_row_group.border.width = str_s.border_width_def1,
    stub_row_group.border.color = NULL,
    data_row.padding = str_s.padding_default,
    data_row.padding.horizontal = str_s.padding_default,
    summary_row.background.color = NULL,
    summary_row.text_transform = NULL,
    summary_row.padding = str_s.padding_default,
    summary_row.padding.horizontal = str_s.padding_default,
    summary_row.border.style = NULL,
    summary_row.border.width = NULL,
    summary_row.border.color = NULL,
    grand_summary_row.background.color = NULL,
    grand_summary_row.text_transform = NULL,
    grand_summary_row.padding = str_s.padding_default,
    grand_summary_row.padding.horizontal = str_s.padding_default,
    grand_summary_row.border.style = NULL,
    grand_summary_row.border.width = NULL,
    grand_summary_row.border.color = NULL,
    footnotes.background.color = NULL,
    footnotes.font.size = str_s.fontsize_default,
    footnotes.padding = str_s.padding_default,
    footnotes.padding.horizontal = str_s.padding_default,
    footnotes.border.bottom.style = str_s.default_border_style,
    footnotes.border.bottom.width = NULL,
    footnotes.border.bottom.color = NULL,
    footnotes.border.lr.style = NULL,
    footnotes.border.lr.width = NULL,
    footnotes.border.lr.color = NULL,
    footnotes.marks = NULL,
    footnotes.spec_ref = NULL,
    footnotes.spec_ftr = NULL,
    footnotes.multiline = NULL,
    footnotes.sep = NULL,
    source_notes.background.color = NULL,
    source_notes.font.size = str_s.fontsize_default,
    source_notes.padding = str_s.padding_default,
    source_notes.padding.horizontal = str_s.padding_default,
    source_notes.border.bottom.style = str_s.default_border_style,
    source_notes.border.bottom.width = NULL,
    source_notes.border.bottom.color = NULL,
    source_notes.border.lr.style = NULL,
    source_notes.border.lr.width = NULL,
    source_notes.border.lr.color = NULL,
    source_notes.multiline = NULL,
    source_notes.sep = NULL,
    row.striping.background_color = NULL,
    row.striping.include_stub = NULL,
    row.striping.include_table_body = NULL,
    container.width = NULL,
    container.height = NULL,
    container.padding.x = str_s.padding_default,
    container.padding.y = str_s.padding_default,
    container.overflow.x = NULL,
    container.overflow.y = NULL,
    ihtml.active = NULL,
    ihtml.use_pagination = NULL,
    ihtml.use_pagination_info = NULL,
    ihtml.use_sorting = NULL,
    ihtml.use_search = NULL,
    ihtml.use_filters = NULL,
    ihtml.use_resizers = NULL,
    ihtml.use_highlight = NULL,
    ihtml.use_compact_mode = NULL,
    ihtml.use_text_wrapping = NULL,
    ihtml.use_page_size_select = NULL,
    ihtml.page_size_default = NULL,
    ihtml.page_size_values = NULL,
    ihtml.pagination_type = NULL,
    ihtml.height = NULL,
    page.orientation = s.orientation,
    page.numbering = NULL,
    page.header.use_tbl_headings = NULL,
    page.footer.use_tbl_notes = NULL,
    page.width = paste0(num_n.page_width, "in"),
    page.height = paste0(num_n.page_height, "in"),
    page.margin.left = paste0(num_n.lmargin, "in"),
    page.margin.right = paste0(num_n.rmargin, "in"),
    page.margin.top = paste0(num_n.tmargin, "in"),
    page.margin.bottom = paste0(num_n.bmargin, "in"),
    page.header.height = NULL,
    page.footer.height = NULL,
    quarto.use_bootstrap = NULL,
    quarto.disable_processing = NULL,
    latex.use_longtable = NULL,
    latex.tbl.pos = NULL
  )
  
  list_tab_options["page.footer.use_tbl_notes"] <- FALSE

  # Define a function to get depth of nexted spanning headers
  max_nested_list_depth <- function(x) {
    if (!is.list(x)) return(0)
    if (length(x) == 0) return(1)
    1 + base::max(base::vapply(x, max_nested_list_depth, 1))
  }
  
  # Define a function to extract headers and columns of each level of spanning headers
  extract_by_level <- function(lst, target_depth, current_depth = 1) {
    nms <- names(lst)
    if (is.null(nms)) nms <- rep("", length(lst))
    
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
    remainder <- text
    
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
    if (length(rtn) < 1) rtn <- 0 else rtn <- rtn[1]
    rtn
  }
  
  #####################################################################################################
  
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
  
  str_s.titles <- titles
  num_n.g_ntitles <- num_n.headers + 1 # 1 skip after title.
  
  for (i in 1:length(str_s.titles)) {
    if (! is.null(str_s.titles[i])) num_n.g_ntitles <- num_n.g_ntitles + 1
  }
  
  num_n.outsidebodylines <- num_n.g_ntitles + num_n.footers - pagesizeadjust 
  
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
  
  str_s.ordervars <- base::intersect(str_s.columns_all, str_s.ordervars)

  if (length(varlabels) > 0 && ! is.na(varlabels[1]) ) {
    df_dsetin <- ru_labels(df_dsetin, varlabels=varlabels)
  }
  str_s.var_labels <- base::lapply(df_dsetin, base::attr, "label")
  
  # print(paste0("ru_report: ORDERVARS: ", str_s.ordervars))
  
  if (! (length(str_s.ordervars) == 0 || is.na(str_s.ordervars[1]) || str_s.ordervars[1] =="")) {
    df_dsetin <- data.table::setorderv(df_dsetin, cols=str_s.ordervars, order=ifelse(str_s.ordervars %in% descending, -1, 1))
  }     
 
  # print("ru_report: Modify COLUMNS") 
  df_dsetin <- ru_format_dataframe(df_dsetin, col_formats = formats, na_str = "")
  df_dsetin <- ru_labels(df_dsetin, varlabels=str_s.var_labels)
  
  str_s.columns <- NULL
  num_n.sharecolvars <- 0
  str_s.sharecols <- NULL
  # derive columns in table by removing non-print columns and shared columns
  for (i in 1:length(str_s.columns_all)) {
    s.name <-str_s.columns_all[i]
    if (s.name %in% sharecolvars && num_n.sharecolvars < length(sharecolvars) - 1) {
      num_n.sharecolvars <- num_n.sharecolvars + 1
      if (is.null(str_s.sharecols)) str_s.sharecols <- s.name
      else str_s.sharecols <- c(str_s.sharecols, s.name)
    } else if (! (s.name %in% noprintvars)) {
      if ( ! (s.name %in% sharecolvars && num_n.sharecolvars >= length(sharecolvars))) {
        if (is.null(str_s.columns)) str_s.columns <- s.name else 
          str_s.columns <- c(str_s.columns, s.name)
      }
    }
  }
  
  # derive columns in table by removing nonprint columns
  s.rptcolumns <- sharecolvars
  for (i in 1:length(str_s.columns_all)) {
    s.name <- str_s.columns_all[i]
    if (! (s.name %in% noprintvars) && ! s.name %in% sharecolvars) {
      if (length(s.rptcolumns) == 0) s.rptcolumns <- s.name
      else s.rptcolumns <- c(s.rptcolumns, s.name)
    }
  }
  
  # print(paste0("ru_report: Get Widths"))
  # print(str_s.columns)
  if (length(str_s.columns) > length(widths)) str_s.col_widths <-  c(widths, rep(15, length(str_s.columns) - length(widths))) else
    str_s.col_widths <- widths
  
  # if (length(str_s.columns_all) > length(widths)) str_s.col_widths <-  c(widths, rep(15, length(str_s.columns_all) - length(widths))) else
  #   str_s.col_widths <- widths
  names(str_s.col_widths) <- str_s.columns

  if (length(str_s.sharecols) > 0) { 
    if (! (sharecolvars[length(sharecolvars)] %in% names(str_s.col_widths))) {
      str_s.col_widths[sharecolvars[length(sharecolvars)]] <- str_s.col_widths[sharecolvars[length(sharecolvars)]] + (length(sharecolvars) - 1) * sharecolvarsindent
    }
    for (i in 1:length(str_s.sharecols)) {
      str_s.col_widths[str_s.sharecols[i]] <- str_s.col_widths[sharecolvars[length(sharecolvars)]]
      noprintvars <- c(noprintvars, str_s.sharecols)
    }
  }
  
  str_s.centrevars <- centrevars
  
  if (length(centrevars) > 0 ) {
    centrevars <- base::intersect(centrevars, columns)
    str_s.centrevars <- NULL
    for (i in 1:length(centrevars)) {
      if (base::any(grepl("^\\s", df_dsetin[[centrevars[i]]]))) str_s.centrevars <- c(str_s.centrevars, centrevars[i])
    }
    str_s.centrevars <- base::setdiff(str_s.centrevars, centrevars)
    if (length(str_s.centrevars) == 0) str_s.centrevars <- NULL
  }
  
  # print(paste0("ru_report: Sharecolvars Label wrapping"))

  # str_s.line_break <- base::ifelse(toupper(str_s.ext) == "RTF", "\a", "\n")

  str_s.line_break <- "\n"    # line break in column values
  str_s.line_break2 <- titlelinebreakchar  # line break for header, footer, title, and subtitle.
  str_s.line_break2 <- paste0("\n", titlelinebreakchar) # line break for header, footer, title, and subtitle.
  str_s.sharecolvarlabel <- NULL
  num_n.colheaderlines <- 0
  
  if (length(sharecolvars) > 0) {
    for (i in 1:length(sharecolvars)) {
      str_s.thislabel <- base::attr(df_dsetin[[sharecolvars[i]]], "label")
      str_s.thislabel <- stringr::str_wrap(str_s.thislabel, width = str_s.col_widths[sharecolvars[i]], 
                                           indent = sharecolvarsindent * (i-1), exdent = (i - 1) * sharecolvarsindent, whitespace_only = TRUE)
      if (length(str_s.thislabel) > 0 && str_s.thislabel[1] != "") {
        if (is.null(str_s.sharecolvarlabel)) str_s.sharecolvarlabel <- str_s.thislabel else
          str_s.sharecolvarlabel <- paste0(str_s.sharecolvarlabel, "\n", str_s.thislabel)
      }
    }
    base::attr(df_dsetin[[sharecolvars[-1]]], "label") <- str_s.sharecolvarlabel
  } 
  
  str_l.wrap_labels <-list()
  for (i in 1:length(str_s.columns)) {
    str_s.this_align <- "left"
    if (str_s.columns[i] %in% c(str_s.centrevars, centrevars)) str_s.this_align <- "center" else
      if (str_s.columns[i] %in% c(rightvars)) str_s.this_align <- "right"
      
    str_s.thislabel_lines <- unlist(base::strsplit(base::attr(df_dsetin[[str_s.columns[i]]], "label"), "\n"))
    str_s.thislabel_lines_1 <- NULL
    for (n in 1:length(str_s.thislabel_lines)) {
      str_s.thislabel_lines_2 <- wrap_align_indent(str_s.thislabel_lines[n], width= str_s.col_widths[str_s.columns[i]], align=str_s.this_align, indent=nchar(stringr::str_extract(str_s.thislabel_lines[n], "^\\s*")))

      str_s.thislabel_lines_2 <- stringr::str_replace_all(str_s.thislabel_lines_2, " +$",  "")
      str_s.thislabel_lines_2 <- stringr::str_replace_all(str_s.thislabel_lines_2, " +\n",  "\n")
      
      str_s.thislabel_lines_2 <- stringr::str_replace_all(str_s.thislabel_lines_2, "\n", str_s.line_break)
      str_s.thislabel_lines_1 <- c(str_s.thislabel_lines_1, str_s.thislabel_lines_2)
    }
    str_s.thislabel <- paste(str_s.thislabel_lines_1, collapse = str_s.line_break)
    
    str_l.wrap_labels[[str_s.columns[i]]] <-   str_s.thislabel
    num_n.colheaderlines <- base::max(num_n.colheaderlines, stringr::str_count(str_s.thislabel, str_s.line_break) + 1)
  }  
  df_dsetin <- ru_labels(df_dsetin, str_l.wrap_labels)
  str_s.var_labels <- base::lapply(df_dsetin, base::attr, "label")
  
  num_n.outsidebodylines <- num_n.outsidebodylines + num_n.colheaderlines
  if (length(proptions) > 0 && "HEADLINE" %in% toupper(proptions)) num_n.outsidebodylines <- num_n.outsidebodylines + 1
  if (length(proptions) > 0 && "HEADSKIP" %in% toupper(proptions)) num_n.outsidebodylines <- num_n.outsidebodylines + 1
  
  num_n.spanning_headers <- max_nested_list_depth(spanningheaders)
  num_n.outsidebodylines <- num_n.outsidebodylines + num_n.spanning_headers
  
  # footer: blank_row="above" +1, title: blank_row="below" + 1, column label: bank_row-"above" + 1
  # print(paste0("n title/footer :", num_n.g_ntitles, "-", num_n.footers))
  
  # print(num_n.outsidebodylines)
  
  num_n.pagesz <- round(num_n.pagesize - num_n.outsidebodylines, 0) 
  
  if (is.null(dsetin) || base::nrow(dsetin) == 0) {
    df_dsetin <- as.data.frame(dplyr::tibble(ggplot__ = c(base::rep(" ", !! num_n.pagesz))))
    df_dsetin[round((num_n.pagesz)/2, 0), "ggplot__"] <- nodatatoreporttext
    columns <- "ggplot__"
    widths <- num_n.linesize
  } 
  
  # print(paste0("ru_report: Wrap Column Values"))
  
  # 1. Pre-calculate column alignments to avoid repeated %in% checks
  str_s.this_align <- setNames(rep("left", length(s.rptcolumns)), s.rptcolumns)
  str_s.this_align[s.rptcolumns %in% str_s.centrevars] <- "center"
  str_s.this_align[s.rptcolumns %in% rightvars] <- "right"

  # 2. Create a vectorized version of your custom function
  # This allows the function to take a vector of strings instead of one string

  # 3. Process by column (Vectorized)
  for (col_name in s.rptcolumns) {
    this_align <- str_s.this_align[col_name]
    df_col_data <- df_dsetin[[col_name]]
    
    # Determine indents for the whole column at once
    if (col_name %in% centrevars &&  ! col_name %in% str_s.centrevars) {
      num_n.this_indent_extra <- base::max(0, base::floor((str_s.col_widths[col_name] - base::max(nchar(stringr::str_replace_all(df_col_data, "\\s+$", ""))))/2))
    } else {
      num_n.this_indent_extra <- 0
    }
    num_n.this_indents <- rep(0, length(df_col_data))
    
    if (length(sharecolvars) > 0 && col_name %in% sharecolvars) {
      n_index <- which(sharecolvars == col_name)
      num_n.this_indents <- (n_index - 1) * sharecolvarsindent
    } else if (col_name %in% centrevars && this_align != "center") {
      # Extract leading whitespace count for all rows in column
      num_n.this_indents <- nchar(stringr::str_extract(df_col_data, "^\\s*"))
      num_n.this_indents[is.na(num_n.this_indents)] <- 0
    }
    
    # Apply the wrapping and line break substitution to the whole column
    df_formatted_col <- base::mapply(wrap_align_indent,
                                     df_col_data, 
                                     width = str_s.col_widths[col_name], 
                                     align = this_align, 
                                     indent = num_n.this_indents + num_n.this_indent_extra)
    
    df_dsetin[[col_name]] <- base::gsub("\n", str_s.line_break, df_formatted_col)
  }
  
  # 4. Calculate nlines__ for all rows at once
  # We only count lines for columns that are "visible"
  visible_cols <- s.rptcolumns
  if (length(sharecolvars) > 0) {
    # Logic: bln_b.visib le is false if it's in sharecolvars but NOT the last index
    hidden_share_vars <- sharecolvars[-length(sharecolvars)]
    visible_cols <- setdiff(s.rptcolumns, hidden_share_vars)
  }
  
  # Use pmax to find the max line count across visible columns for each row
  num_n.line_counts <- lapply(df_dsetin[visible_cols], function(column) {
    stringr::str_count(column, stringr::fixed(str_s.line_break)) + 1
  })
  
  df_dsetin$nlines__ <- do.call(base::pmax, num_n.line_counts)
  
  str_s.skipvars <- skipvars
  if (! (length(skipvars) == 0 || length(nowidowvar) == 0)) {
    str_s.skipvars <- NULL
    for (i in 1:length(skipvars)) {
      num_n.ind <- ru_index(nowidowvar == str_s.columns_all)
      if ( num_n.ind <= ru_index(skipvars[i] == str_s.columns_all)) {
        if (is.null(str_s.skipvars)) str_s.skipvars <- skipvars[i]
        else str_s.skipvars <- c(str_s.skipvars, skipvars[i])
      } 
    }
  }
  
  # df_lastdset1 <- df_dsetin
  
  ############################################################################################################################
  
  # print(paste0("ru_report: Split columns into panels and keep idvars for each panel"))  
  
  s.idvars <- idvars
  if (length(intersect(s.idvars, sharecolvars)) > 0) s.idvars <- intersect(str_s.columns, unique(c(s.idvars, sharecolvars)))
  
  str_s.this_cols <- NULL
  str_s.panel_cols <- NULL
  num_n.panel_left_col <- NULL
  num_n.panel_right_col <- NULL
  num_n.panels <- 1
  num_n.this_length <- sum(str_s.col_widths[s.idvars]) + length(s.idvars) * colspacing
  for (str_s.this_col in setdiff(str_s.columns, s.idvars)) {
    # print(c(num_n.this_length, str_s.col_widths[str_s.this_col], num_n.this_length + str_s.col_widths[str_s.this_col]))
    if (is.null(str_s.this_cols)) str_s.this_cols <- str_s.this_col 
    if (num_n.this_length + str_s.col_widths[str_s.this_col] <= num_n.linesize) {
      str_s.this_cols <- c(str_s.this_cols, str_s.this_col)
      num_n.this_length <- num_n.this_length + str_s.col_widths[str_s.this_col] + colspacing
    } else {
      num_n.panel_left_col[num_n.panels] <- round((num_n.linesize - num_n.this_length)/2, 0)
      num_n.panel_right_col[num_n.panels] <- num_n.linesize - num_n.this_length - num_n.panel_left_col[num_n.panels]
      if (num_n.panel_right_col[num_n.panels] < 0) num_n.panel_right_col[num_n.panels] <- 0
      if (num_n.panel_left_col[num_n.panels] < 0) num_n.panel_left_col[num_n.panels] <- 0
      
      num_n.this_length <- sum(str_s.col_widths[s.idvars]) + length(s.idvars) * colspacing
      str_s.panel_cols[[num_n.panels]] <- base::unique(c(s.idvars, str_s.this_cols))
      str_s.this_cols <- str_s.this_col
      num_n.panels <- num_n.panels + 1
    }
  }
  num_n.panel_left_col[num_n.panels] <- round((num_n.linesize - num_n.this_length )/2, 0)
  num_n.panel_right_col[num_n.panels] <- num_n.linesize - num_n.this_length - num_n.panel_left_col[num_n.panels]
  if (num_n.panel_right_col[num_n.panels] < 0) num_n.panel_right_col[num_n.panels] <- 0
  if (num_n.panel_left_col[num_n.panels] < 0) num_n.panel_left_col[num_n.panels] <- 0
  
  str_s.panel_cols[[num_n.panels]] <- base::unique(c(s.idvars, str_s.this_cols)) 
  
  num_n.col_widths <- round((str_s.col_widths + colspacing) * num_n.fontsize * 0.595, 0)   
  for (i in 1:num_n.panels) {
    num_n.panel_right_col[num_n.panels] <- round(num_n.panel_right_col[num_n.panels] * num_n.fontsize * 0.595, 0)
    num_n.panel_left_col[num_n.panels] <- round(num_n.panel_left_col[num_n.panels] * num_n.fontsize * 0.595, 0)      
  }

  # print(paste0("ru_report: Check parameter PROPTIONS=", proptions))
  
  ############################################################################################################################
 
  # print(paste0("ru_report: Add page number to data row"))  
  
  num_n.pagenumber <- 0
  num_n.linenumber <- 0
  num_n.addlines <- 0
  num_n.numofpages <- 1
  num_n.wraplines <- 0
  str_s.sharecolvarvalue <- NULL
  bln_b.addsharecollines <- FALSE
  if (length(sharecolvars) == 0) bln_b.overallsummary <- FALSE else bln_b.overallsummary <- overallsummary

  # df_dsetin <- df_lastdset1
  df_dsetin_1 <- NULL
  list_df_rows_list <- list()

  # print(paste0("ru_report: num_n.pagesz: ", num_n.pagesz))
  
  for (i in 1:base::nrow(df_dsetin)) {
    num_n.catlines <- 0
    num_n.addlines <- 0
    bln_b.overallsummary_line <- FALSE
    if (length(sharecolvars) > 0) {
      str_s.sharecolvarvalue <- NULL
      for (j in 1:(length(sharecolvars) - 1)) {
        num_n.numoflines1 <- stringr::str_count(df_dsetin[i, sharecolvars[j]], str_s.line_break) + 1
        num_n.addlines <- num_n.addlines + num_n.numoflines1
        if ( i == 1 || (df_dsetin[i, sharecolvars[j]] != df_dsetin[i - 1, sharecolvars[j]])) {
          if (bln_b.overallsummary && (i==1 || j < length(sharecolvars) - 1)) {
            num_n.addlines <- num_n.addlines - num_n.numoflines1
            bln_b.overallsummary_line <- TRUE
          }
          bln_b.addsharecollines <- TRUE
          break
        }
      }
    }
    if ( length(nowidowvar) > 0 && (i==1 || df_dsetin[i, nowidowvar] != df_dsetin[i - 1, nowidowvar])) {
      if (length(sharecolvars) > 0 && bln_b.addsharecollines && (ru_index(nowidowvar == str_s.columns_all) >= ru_index(sharecolvars[1] == str_s.columns_all))) {
        num_n.catlines <- num_n.addlines 
      } else if (bln_b.addsharecollines) num_n.catlines <- num_n.addlines 
      
      if ((i+1) <= nrow(df_dsetin)) {
        for (j in seq((i+1), nrow(df_dsetin))) {
          if (df_dsetin[i, nowidowvar] == df_dsetin[j, nowidowvar]) {
            num_n.catlines <- num_n.catlines + df_dsetin[j, "nlines__"] 
            if (! is.null(str_s.skipvars)) for (k in 1:length(str_s.skipvars)) {
              if (str_s.skipvars[k] != nowidowvar) {
                if (j < nrow(df_dsetin) && df_dsetin[j, str_s.skipvars[k]] == df_dsetin[j + 1, str_s.skipvars[k]]) num_n.catlines <- num_n.catlines + 1
              }
            }
          } else break
        }
      }
    }
    if (i == 1) bln_b.newpage <- TRUE else 
      if ( num_n.linenumber > 3 &&  length(nowidowvar) > 0 && num_n.catlines >= num_n.pagesz - num_n.linenumber + 2) bln_b.newpage <- TRUE else 
        if (bln_b.addsharecollines && (num_n.addlines + df_dsetin[i, "nlines__"] >= num_n.pagesz - num_n.linenumber + 2)) bln_b.newpage <- TRUE else 
          if (df_dsetin[i, "nlines__"] >= num_n.pagesz - num_n.linenumber + 2) bln_b.newpage <- TRUE else 
            if (i > 1) bln_b.newpage <- FALSE
    
    # Add extra lines to fill all rows on a page
    if (i > 1 && bln_b.newpage && (num_n.pagesz - num_n.linenumber + 1 > 0)) {
      # df_dsetin_3 <- NULL
      for (k in 1:(num_n.pagesz - num_n.linenumber + 1 )) {
        df_dsetin_2 <- list_df_rows_list[[1]][0, ] 
        df_dsetin_2[1, "nlines__"] <- 1
        df_dsetin_2[1, "linenum__"] <- num_n.linenumber 
        df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber
        df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
        # df_dsetin_3 <- dplyr::bind_rows(df_dsetin_3, df_dsetin_2)
        list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
        num_n.linenumber <- num_n.linenumber + 1
      }
      # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_3)
    }
    
    if (! bln_b.newpage && length(computebeforepagevars) > 0) {
      for (j in 1:length(computebeforepagevars)) {
        if (df_dsetin[i, computebeforepagevars[j]] != df_dsetin[i - 1, computebeforepagevars[j]])  bln_b.newpage <- TRUE
      }
    } 
    
    df_dsetin[i, "newpage__"] <- bln_b.newpage
    df_dsetin[i, "addline__"] <- bln_b.addsharecollines
    df_dsetin[i, "catline__"] <- num_n.catlines
    
    if (bln_b.newpage) {
      num_n.pagenumber <- num_n.pagenumber + 1
      num_n.linenumber <- 1
      num_n.wraplines <- 0
      bln_b.addsharecollines <- TRUE
      if (length(computebeforepagevars) > 0) {
        num_n.linenumber <- num_n.linenumber + length(computebeforepagevars)
      }
    } 
    
    if ((bln_b.addsharecollines || bln_b.newpage) && length(sharecolvars) > 0 && ((! bln_b.overallsummary_line) || (bln_b.overallsummary_line && length(sharecolvars) > 2))) {
      df_dsetin_2 <- df_dsetin[i, ]
      df_dsetin_2[1, "linenum__"] <- num_n.linenumber
      df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber        
      num_n.index <- ru_index(sharecolvars[length(sharecolvars)] == str_s.columns_all)
      for (j in 1:length(str_s.columns_all)) {
        if (j > num_n.index) df_dsetin_2[1, str_s.columns_all[j]] <- " "
        if (str_s.columns_all[j] == sharecolvars[length(sharecolvars)]) {
          str_s.sharecolvarvalue <- NULL
          if (bln_b.overallsummary_line) num_n.loop_levels_adjust <- 2 else num_n.loop_levels_adjust <- 1
          for (k in 1:(length(sharecolvars) - num_n.loop_levels_adjust)) {
            if (is.null(str_s.sharecolvarvalue)) {
              str_s.sharecolvarvalue <- df_dsetin[i, sharecolvars[k]]
            } else {
              str_s.sharecolvarvalue <- paste0(str_s.sharecolvarvalue, str_s.line_break, df_dsetin[i, sharecolvars[k]])
            }
          }
          df_dsetin_2[1, sharecolvars[length(sharecolvars)]]  <- str_s.sharecolvarvalue
        }
      }
      num_n.linenumber <- num_n.linenumber + num_n.addlines
      num_n.wraplines <- num_n.wraplines + num_n.addlines - 1
      df_dsetin_2[1, "nlines__"] <- num_n.addlines
      # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_2)
      list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
    } 
    
    df_dsetin_2 <- df_dsetin[i, ]
    if (bln_b.overallsummary_line) df_dsetin_2[1, sharecolvars[length(sharecolvars)]] <- base::substring(df_dsetin_2[1, sharecolvars[length(sharecolvars)]], sharecolvarsindent + 1)
    df_dsetin_2[["linenum__"]] <- num_n.linenumber
    df_dsetin_2[["pagenum__"]] <- num_n.pagenumber   
    # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_2)  
    list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
    
    
    num_n.linenumber <- num_n.linenumber + unlist(df_dsetin[i, "nlines__"])
    num_n.wraplines <- num_n.wraplines + df_dsetin[i, "nlines__"] - 1
    
    # Add skip lines
    if (length(str_s.skipvars) > 0 & num_n.linenumber < num_n.pagesz + 1) for (k in 1:length(str_s.skipvars)) {
      if ( i < nrow(df_dsetin) && df_dsetin[i, str_s.skipvars[k]] != df_dsetin[i + 1, str_s.skipvars[k]]) {
        df_dsetin_2 <- list_df_rows_list[[1]][0, ] 
        df_dsetin_2[1, "nlines__"] <- 1
        df_dsetin_2[1, "linenum__"] <- num_n.linenumber 
        df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber
        df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
        # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_2)
        list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
        
        num_n.linenumber <- num_n.linenumber + 1
      }
    } 
    
    bln_b.newpage <- FALSE
    bln_b.addsharecollines <- FALSE
  }
  
  # Add extra lines to fill all rows on a page
  if (num_n.pagesz - num_n.linenumber + 1 > 0) {
    # df_dsetin_3 <- NULL
    for (k in 1:(num_n.pagesz - num_n.linenumber + 1)) {
      df_dsetin_2 <- list_df_rows_list[[1]][0, ] 
      df_dsetin_2[1, "nlines__"] <- 1
      df_dsetin_2[1, "linenum__"] <- num_n.linenumber 
      df_dsetin_2[1, "pagenum__"] <- num_n.pagenumber
      df_dsetin_2 <- df_dsetin_2 |> dplyr::mutate(dplyr::across(where(is.character), ~ " "))
      # df_dsetin_3 <- dplyr::bind_rows(df_dsetin_3, df_dsetin_2)
      list_df_rows_list[[length(list_df_rows_list) + 1]] <- df_dsetin_2
      num_n.linenumber <- num_n.linenumber + 1
    }
    # df_dsetin_1 <- dplyr::bind_rows(df_dsetin_1, df_dsetin_3)
  }
  
  df_dsetin_1 <- dplyr::bind_rows(list_df_rows_list)
  df_dsetin_1 |> dplyr::filter(pagenum__==1)
  
  df_dsetin <- ru_labels(df_dsetin_1, str_s.var_labels) |> dplyr::select(-catline__, -addline__, -newpage__, -nlines__)

  str_s.columns_all <- c("pagenum__", str_s.columns_all)
  str_s.ordervars <- c(str_s.ordervars, "pagenum__")
  
  num_n.numofpages <- num_n.pagenumber
  noprintvars <- c(noprintvars, "pagenum__")

  # df_lastdset3 <- df_dsetin
  
  # print(paste0("ru_report: Start to output file"))

  ############################################################################################################################
  ############################################################################################################################
  
  # print(paste0("ru_report: Start to creating GT tables page by page"))  
  
  # df_dsetin <- df_lastdset3
  
  # Remove trading spaces
  df_dsetin <- df_dsetin %>% dplyr::mutate(dplyr::across(all_of(intersect(str_s.columns, names(select(df_dsetin, where(is.character))))), ~ stringr::str_replace_all(.x, "\\s+$", " ")))
  df_dsetin <- df_dsetin %>% dplyr::mutate(dplyr::across(all_of(intersect(str_s.columns, names(select(df_dsetin, where(is.character))))), ~ stringr::str_replace_all(.x, paste0("\\s+", str_s.line_break), str_s.line_break)))

  this_gt_tbl <- list()
  for (i in 1:num_n.numofpages) {
    str_s.computebeforepagelines <- NULL
    if (length(computebeforepagevars) > 0) {
      for (j in 1:length(computebeforepagevars)) {
        if (j==1) {
          str_s.computebeforepagelines <- paste0(computebeforepagelines[(j-1) * 2 + 1], unlist(df_dsetin %>% 
            dplyr::filter(pagenum__ == !! i) %>% dplyr::select(!!! rlang::syms(computebeforepagelines[(j-1) * 2 + 2]))
            %>% dplyr::distinct())[1])
        } else {
          str_s.computebeforepagelines <- paste(str_s.computebeforepagelines, paste0(computebeforepagelines[(j-1) * 2 + 1], unlist(df_dsetin %>% 
            dplyr::filter(pagenum__ == !! i) %>% dplyr::select(!!! rlang::syms(computebeforepagelines[(j-1) * 2 + 2])) 
            %>% dplyr::distinct())[1]), sep=str_s.line_break2)
        }
      }
    }

    for (j in 1:num_n.panels) {
      num_n.page <- (i - 1) * num_n.panels + j
      str_s.headers <- NULL
      if (num_n.headers > 0) for (k in 1:num_n.headers) {
        str_s.temp_left <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.leftheaders[k], fixed = TRUE)
        str_s.temp_right <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.rightheaders[k], fixed = TRUE)
        str_s.temp_center <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.centerheaders[k], fixed = TRUE)
        
        str_s.tmp <- paste0(str_s.temp_left, base::strrep(" ", num_n.linesize))
        if (! str_s.temp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.linesize - base::nchar(str_s.temp_center))/2)), str_s.temp_center)
        if (! str_s.temp_right %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.linesize - base::nchar(str_s.temp_right)),  str_s.temp_right)
        str_s.headers <- c(str_s.headers, base::gsub(" ", str_s.hold_space, base::substring(str_s.tmp, 1, num_n.linesize), fixed=TRUE))
      } 
      
      str_s.titles_1 <- NULL
      if (length(str_s.titles) > 0) for (k in 1:length(str_s.titles)) {
        str_s.tmp <- paste0(base::strrep(str_s.hold_space, base::floor((num_n.linesize - base::nchar(str_s.titles[k]))/2)), str_s.titles[k],  base::strrep(str_s.hold_space, num_n.linesize))  
        str_s.titles_1 <- c(str_s.titles_1, base::substring(str_s.tmp, 1, num_n.linesize))
      }
      str_s.headers <- c(str_s.headers, str_s.titles_1)
      str_s.thistitle <- paste(c(str_s.headers, str_s.hold_space), collapse =str_s.line_break2) # added a line below the title.

      str_s.footers <- NULL
      if (num_n.footers > 0) for (k in 1:num_n.footers) {
        str_s.temp_left <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.leftfooters[k], fixed = TRUE)
        str_s.temp_right <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.rightfooters[k], fixed = TRUE)
        str_s.temp_center <- base::gsub("pageof", paste0("Page ", num_n.page, " of ", num_n.numofpages * num_n.panels), str_s.centerfooters[k], fixed = TRUE)
        
        str_s.tmp <- paste0(str_s.temp_left, base::strrep(" ", num_n.linesize))
        if (! str_s.temp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.linesize - base::nchar(str_s.temp_center))/2)), str_s.temp_center)
        if (! str_s.temp_right %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.linesize - base::nchar(str_s.temp_right)),  str_s.temp_right)
        str_s.footers <- c(str_s.footers, base::gsub(" ", str_s.hold_space, base::substring(str_s.tmp, 1, num_n.linesize), fixed=TRUE))
      }   
      str_s.thisfooter <- if (!is.null(str_s.footers)) paste(str_s.footers, collapse =str_s.line_break2) else NULL

      df_this_dset <- df_dsetin %>% dplyr::filter(pagenum__ == !! i) 
      str_s.this_cols <- base::unique(str_s.panel_cols[[j]])
      if (num_n.panel_left_col[j] > 0) {
        str_s.this_cols <- c("this_left_col__", str_s.this_cols)
        df_this_dset <- df_this_dset %>% dplyr::mutate(this_left_col__="") 
      }
      if (num_n.panel_right_col[j] > 0) {
        str_s.this_cols <- c(str_s.this_cols, "this_right_col__")
        df_this_dset <- df_this_dset %>% dplyr::mutate(this_right_col__="") 
      }
      
      # print(str_s.this_cols)
     
      this_gt_tbl[[num_n.page]] <- df_this_dset %>%
        dplyr::select(all_of(str_s.this_cols)) %>%
        gt::gt() %>%
        (\(x) rlang::exec(gt::tab_options, data = x, !!!list_tab_options))() %>%
        gt::sub_missing(
          columns = everything(),   # apply to all columns
          missing_text = ""         # show blank instead of "NA"
        ) %>%
        gt::tab_style(
          style = gt::cell_text(whitespace = "pre"),  # preserve literal spaces
          locations = gt::cells_body(columns = where(is.character))
        ) %>%
        gt::tab_style(
          style = gt::cell_text(v_align="top"), 
          locations = gt::cells_body(columns = everything())
        ) 
      
      if (! is.null(str_s.thistitle)) {
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          gt::tab_header(
            title = str_s.thistitle,
            subtitle=str_s.computebeforepagelines
          ) 
      }
      
      if (! is.null(str_s.thisfooter)) { 
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          # gt::tab_source_note(
          #   source_note =  str_s.thisfooter
          # ) 
          gt::tab_footnote(
            footnote = str_s.thisfooter
          )
      }
      
      
      # 
      # %>%
      #   gt::tab_style(
      #     style = gt::cell_text(align = "center", v_align="middle"),
      #     locations = gt::cells_title(groups = "title")
      #   ) %>%
      #   # Compuate before page lines
      #   gt::tab_style(
      #     style = gt::cell_text(align = "left", v_align="middle"),
      #     locations = gt::cells_title(groups = "subtitle")
      #   ) %>% 
      #   gt::tab_style(
      #     style = list(
      #       gt::cell_borders(
      #         sides = c("top", "bottom"),
      #         color = "black",
      #         weight = "2px", # or your desired thickness
      #         style = "solid"
      #       )
      #     ),
      #     locations = gt::cells_column_labels(columns = everything())
      #   )          
      # %>%
      #   gt::tab_style(
      #     style = gt::cell_borders(
      #       sides = c("bottom", "top"),
      #       weight = "2px" # Or use points/twips for RTF
      #     ),
      #     locations = gt::cells_column_labels(
      #       columns = base::setdiff(str_s.this_cols, c("this_left_col__", "this_right_col__"))
      #     )
      #   ) %>%
      #   gt::tab_style(
      #     style = gt::cell_borders(
      #       sides = "bottom",
      #       color = "black",
      #       weight = "1px" # This translates to \cline in LaTeX
      #     ),
      #     locations = gt::cells_body(
      #       columns = base::setdiff(str_s.this_cols, c("this_left_col__", "this_right_col__")), # Specify which columns get the line
      #       rows = base::nrow(df_this_dset)               # Targets only the last row of data
      #     )
      #   )
      
      if (is.null(dsetin) || base::nrow(dsetin) == 0) {
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          gt::cols_align(align = "center", columns = where(is.character)) 
      } else {
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          gt::cols_align(align = "left", columns = where(is.character)) %>%
          gt::cols_align(align = "right", columns = where(is.numeric))         
      }
      
      # Spanning columns
      if (length(num_n.spanning_headers) > 0) {
        for (k in 1:num_n.spanning_headers) {
          this_spanning_headers <- extract_by_level(spanningheaders, k)
          for (l in 1:length(this_spanning_headers[["headers"]])) {
            this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
              gt::tab_spanner(
                label = this_spanning_headers[["headers"]][l],
                columns = intersect(str_s.panel_cols[[j]], unlist(this_spanning_headers[["values"]][l]))
              )
          }
        }
      }
      
      # column label
      for (k in 1:length(str_s.panel_cols[[j]])) {
        # print(str_s.panel_cols[[j]])
        if (is.null(str_l.wrap_labels[str_s.panel_cols[[j]][k]])) str_s.this_label <- Hmisc::label(df_dsetin[[str_s.panel_cols[[j]][k]]]) else
          str_s.this_label <- str_l.wrap_labels[str_s.panel_cols[[j]][k]]
        
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          # labels
          gt::cols_label(
            !! rlang::sym(str_s.panel_cols[[j]][k]) := str_s.this_label
          ) %>%
          gt::cols_width(
            rlang::new_formula(as.name(str_s.panel_cols[[j]][k]), paste0(num_n.col_widths[str_s.panel_cols[[j]][k]] * 4/3, "px"))
          )
      }
      # column width
      if (num_n.panel_right_col[j] > 0) {
        str_s.this_col <- "this_right_col__"
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          gt::cols_label(
            this_right_col__=""
          ) %>%
          gt::cols_width(
            rlang::new_formula(as.name(str_s.this_col), paste0(num_n.panel_right_col[j] * 4/3, "px"))
            
          )
      }
      # width of left and right blank column
      if (num_n.panel_left_col[j] > 0) {
        str_s.this_col <- "this_left_col__"
        this_gt_tbl[[num_n.page]] <- this_gt_tbl[[num_n.page]] %>%
          gt::cols_label(
            this_left_col__=""
          ) %>%
          gt::cols_width(
            # this_left_col__ ~ paste0(num_n.panel_left_col[j], "pt")
            # rlang::new_formula(as.name(str_s.this_col), paste0(num_n.panel_left_col[j], "pt"))
            rlang::new_formula(as.name(str_s.this_col), paste0(num_n.panel_left_col[j] * 4/3, "px"))
          )
      }
    }
  }
  
  return(this_gt_tbl)
}


