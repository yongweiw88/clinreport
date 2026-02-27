#' R function to create RTF/PDF output for clinical reporting.
#'
#' Pass in a dataframe and reporting settings to RTF/PDF output generated.
#'
#' @param dsetin Incoming data frame or list of data frames.
#' 
#' @return A PDF output file
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

ru_write2pdf <- function(
  inobj,
  outfile=rfenv$G_OUTFILE,
  fontsize=c(rfenv$G_FONTSIZE, "L10", "L09", "L08", "L07", "L11", "L12", "P10", "P09", "P08", "P07", "P11", "P12"),
  headers=list("left"=c(paste0("Protocol: ", rfenv$G_STUDY_DESC), paste0("Population: ", rfenv$G_POPLBL)),
          "right"=c("pageof", dplyr::if_else(is.null(rfenv$G_DATADATE) || rfenv$G_DATADATE[1] == "", "",  paste0("Data Date: ", rfenv$G_DATADATE)))),
  footers=list("left"=paste0(rfenv$G_USERID, ": ", rfenv$G_PGMPTH, " ", format(Sys.time(), "%Y-%m-%d %T"))),
  titlelinebreakchar="\a",
  titleblanku8char="\u0A00",
  marginsininch=c("top"=1.25, "bottom"=1, "left"=1, "right"=1),
  fontname=c("DejaVuSansMono", "Courier New")
  ) {
  
  # Define a function to replace LaTex special characters.
  this_latex_escape <- function(text) {
    # 1. The Backslash MUST come first (otherwise you'll escape the escapes!)
    # Replace \ with \textbackslash{}
    text <- gsub("\\", "\\textbackslash{}", text, fixed = TRUE)
    
    # 2. These 7 can be escaped with a simple backslash prefix
    # # $ % & _ { }
    # In R gsub, we need \\ to get a single \ in the output
    specials <- c("#", "$", "%", "&", "_", "{", "}")
    for (s in specials) {
      text <- gsub(s, paste0("\\", s), text, fixed = TRUE)
    }
    
    # 3. These 2 need special commands to look right
    # ^ and ~
    text <- gsub("^", "\\textasciicircum{}", text, fixed = TRUE)
    text <- gsub("~", "\\textasciitilde{}", text, fixed = TRUE)
    
    return(text)
  }
  
  str_s.outType <- NULL
  if (is.character(inobj)) str_s.outType <- "TEXT" else
    if (base::inherits(inobj, c("patch", "patchwork", "ggplot", "gg"))) {
      str_s.outType <- "GG" 
      gt_tbl <- list(inobj)
    } else if (base::inherits(inobj, c("gt", "gt_tbl"))) {
      gt_tbl <- list(inobj)
      str_s.outType <- "GT"
    } else if (is.list(inobj)) {
        gt_tbl <- inobj 
        if (base::inherits(inobj[[1]], c("patch", "patchwork", "ggplot", "gg"))) str_s.outType <- "GG"
        if (base::inherits(inobj[[1]], c("gt", "gt_tbl"))) str_s.outType <- "GT"
    }

  # Define a line break and a blank space string which will be used in gt table and will be replaced in LaTex.  
  str_s.hold_space <- titleblanku8char
  str_s.line_break <- titlelinebreakchar
  str_s.line_break2 <- titlelinebreakchar
  # str_s.line_break2 <- paste0(titlelinebreakchar, "\n")
  
  num_n.tmargin <- base::ifelse("top" %in% names(marginsininch), marginsininch["top"], 1.25)
  num_n.bmargin <- base::ifelse("bottom" %in% names(marginsininch), marginsininch["bottom"], 1)
  num_n.lmargin <- base::ifelse("left" %in% names(marginsininch), marginsininch["left"], 1)
  num_n.rmargin <- base::ifelse("right" %in% names(marginsininch), marginsininch["right"], 1)

  str_s.ext <- unlist(stringr::str_split(outfile, "[.]"))
  str_s.ext <- str_s.ext[base::length(str_s.ext)]
  str_s.outdir <- base::dirname(outfile)
  str_s.outfilename <- base::basename(outfile)
  str_s.outfile <- base::substring(str_s.outfilename, 1, base::nchar(str_s.outfilename) - base::nchar(str_s.ext) - 1)
  base::unlink(outfile)
  base::unlink(paste0(str_s.outdir, "/", str_s.outfile, ".tex"))
  system(paste("touch", outfile))
  
  # print(paste0("ru_write2pdf: Get Line size and page size"))
  if (toupper(str_s.ext) %in% c("L07", "L08", "L09", "L10", "L11", "L12", "P07", "P08", "P09", "P10", "P11", "P12")) {
    str_s.ext_t <- str_s.ext
  } else if (is.na(base::suppressWarnings(as.numeric(fontsize[1])))) {
    str_s.ext_t <- fontsize[1]
  } else if (fontsize[1] < 5) {
    str_s.ext_t <- "L10"
  } else { 
    if (fontsize[1] >= 10) {
      str_s.ext_t <- paste0("L", fontsize[1]) 
    } else { 
      str_s.ext_t <- paste0("L0", fontsize[1])
    }
  }
  
  # print(paste0("ru_write2pdf: Orientation"))
  if (base::substr(str_s.ext_t, 1, 1) == "L") {
    str_s.orientation="landscape"
    num_n.page_height <- 8.5
    num_n.page_width <- 11    
    str_s.dtype <- "TXT"
  } else if (base::substr(str_s.ext_t, 1, 1) == "P") {
    str_s.orientation="portrait"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
    str_s.dtype <- "TXT"
  } else {
    str_s.orientation="landscape"
    num_n.page_height <- 11
    num_n.page_width <- 8.5
    str_s.dtype <- str_s.ext
  }
  
  list_l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  list_l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  num_n.linesize <- unlist(list_l.lsmvar[str_s.ext_t])
  num_n.pagesize <- unlist(list_l.psmvar[str_s.ext_t])
  
  num_n.numofpages <- if (str_s.outType == "TEXT") base::sum(stringr::str_count(inobj, "\f")) + 1 else length(gt_tbl)
  
  # headers
  str_s.leftheaders <- if ("left" %in% names(headers)) unlist(headers[["left"]]) else ""
  str_s.rightheaders <- if ("right" %in% names(headers)) unlist(headers[["right"]]) else ""
  str_s.centerheaders <- if ("center" %in% names(headers)) unlist(headers[["center"]]) else ""
  
  str_s.leftheaders <- base::unlist(stringr::str_split(this_latex_escape(str_s.leftheaders), "\n"))
  str_s.rightheaders <- base::unlist(stringr::str_split(this_latex_escape(str_s.rightheaders), "\n"))
  str_s.centerheaders <- base::unlist(stringr::str_split(this_latex_escape(str_s.centerheaders), "\n"))
    
  num_n.headers <- base::max(length(str_s.rightheaders), length(str_s.leftheaders), length(str_s.centerheaders))
  if (num_n.headers == 1 & base::gsub(" *", "", paste(str_s.rightheaders, str_s.leftheaders, str_s.centerheaders, sep="", collapse="")) == "") num_n.headers <- 0
  
  if (num_n.headers > length(str_s.rightheaders) && str_s.rightheaders[1] != "") str_s.rightheaders <- c(str_s.rightheaders, base::rep(" ", num_n.headers - length(str_s.rightheaders)))
  if (num_n.headers > length(str_s.leftheaders) && str_s.leftheaders[1] != "") str_s.leftheaders <- c(str_s.leftheaders, base::rep(" ", num_n.headers - length(str_s.leftheaders)))
  if (num_n.headers > length(str_s.centerheaders) && str_s.centerheaders[1] != "") str_s.centerheaders <- c(str_s.centerheaders, base::rep(" ", num_n.headers - length(str_s.centerheaders)))
  
  if (num_n.headers > 0) {
    str_s.rightheaders <- base::gsub("^\\s*$", "~", str_s.rightheaders, perl=TRUE)
    str_s.leftheaders <- base::gsub("^\\s*$", "~", str_s.leftheaders, perl=TRUE)
    str_s.centerheaders <- base::gsub("^\\s*$", "~", str_s.centerheaders, perl=TRUE)
  }
  
  if (length(str_s.leftheaders) > 1) str_s.leftheaders[-length(str_s.leftheaders)] <- paste0(str_s.leftheaders[-length(str_s.leftheaders)], " \\\\")
  if (length(str_s.rightheaders) > 1) str_s.rightheaders[-length(str_s.rightheaders)] <- paste0(str_s.rightheaders[-length(str_s.rightheaders)], " \\\\")
  if (length(str_s.centerheaders) > 1) str_s.centerheaders[-length(str_s.centerheaders)] <- paste0(str_s.centerheaders[-length(str_s.centerheaders)], " \\\\")
  
  str_s.headers <- NULL
  if (length(str_s.leftheaders) > 1 || str_s.leftheaders[1] != "~") str_s.headers <- c(str_s.headers, "\\lhead{", str_s.leftheaders, "}")
  if (length(str_s.rightheaders) > 1 || str_s.rightheaders[1] != "~") str_s.headers <- c(str_s.headers, "\\rhead{", str_s.rightheaders, "}")
  if (length(str_s.centerheaders) > 1 || str_s.centerheaders[1] != "~") str_s.headers <- c(str_s.headers, "\\chead{", str_s.centerheaders, "}")
  
  if (!is.null(str_s.headers)) str_s.headers <- base::gsub("pageof", paste0("Page \\arabic{page} of ", num_n.numofpages), str_s.headers, fixed = TRUE)
  
  # footers
  str_s.leftfooters <- if ("left" %in% names(footers)) unlist(footers[["left"]]) else ""
  str_s.rightfooters <- if ("right" %in% names(footers)) unlist(footers[["right"]]) else ""
  str_s.centerfooters <- if ("center" %in% names(footers)) unlist(footers[["right"]]) else ""
  
  str_s.leftfooters <- base::unlist(stringr::str_split(this_latex_escape(str_s.leftfooters), "\n"))
  str_s.rightfooters <- base::unlist(stringr::str_split(this_latex_escape(str_s.rightfooters), "\n"))
  str_s.centerfooters <- base::unlist(stringr::str_split(this_latex_escape(str_s.centerfooters), "\n"))

  num_n.footers <- base::max(length(str_s.rightfooters), length(str_s.leftfooters), length(str_s.centerfooters))
  if (num_n.footers == 1 && base::gsub(" *", "", paste0(str_s.rightfooters, str_s.leftfooters, str_s.centerfooters)) == "") num_n.footers <- 0
  
  if (num_n.footers > length(str_s.rightfooters) && str_s.rightfooters[1] != "") str_s.rightfooters <- c(str_s.rightfooters, base::rep(" ", num_n.footers - length(str_s.rightfooters)))
  if (num_n.footers > length(str_s.leftfooters) && str_s.leftfooters[1] != "") str_s.leftfooters <- c(str_s.leftfooters, base::rep(" ", num_n.footers - length(str_s.leftfooters)))
  if (num_n.footers > length(str_s.centerfooters) && str_s.centerfooters[1] != "") str_s.centerfooters <- c(str_s.centerfooters, base::rep(" ", num_n.footers - length(str_s.centerfooters)))
  
  if (num_n.footers > 0) {
    str_s.rightfooters <- base::gsub("^\\s*$", "~", str_s.rightfooters, perl=TRUE)
    str_s.leftfooters <- base::gsub("^\\s*$", "~", str_s.leftfooters, perl=TRUE)
    str_s.centerfooters <- base::gsub("^\\s*$", "~", str_s.centerfooters, perl=TRUE)
  }
  
  if (length(str_s.leftfooters) > 1) str_s.leftfooters[-length(str_s.leftfooters)] <- paste0(str_s.leftfooters[-length(str_s.leftfooters)], " \\\\")
  if (length(str_s.rightfooters)  > 1) str_s.rightfooters[-length(str_s.rightfooters)] <- paste0(str_s.rightfooters[-length(str_s.rightfooters)], " \\\\")
  if (length(str_s.centerfooters)  > 1) str_s.centerfooters[-length(str_s.centerfooters)] <- paste0(str_s.centerfooters[-length(str_s.centerfooters)], " \\\\")
  
  str_s.footers <- NULL
  if (length(str_s.leftfooters) > 1 || str_s.leftfooters[1] != "~") str_s.footers <- c(str_s.footers, "\\lfoot{", str_s.leftfooters, "}")
  if (length(str_s.rightfooters) > 1 || str_s.rightfooters[1] != "~") str_s.footers <- c(str_s.footers, "\\rfoot{", str_s.rightfooters, "}")
  if (length(str_s.centerfooters) > 1 || str_s.centerfooters[1] != "~") str_s.footers <- c(str_s.footers, "\\cfoot{", str_s.centerfooters, "}")
  
  if (!is.null(str_s.footers)) str_s.footers <- base::gsub("pageof", paste0("Page \\arabic{page} of ", num_n.numofpages), str_s.footers, fixed = TRUE)
  
  # margins with header and footer counted
  num_n.font_size <- as.numeric(sub(".*?(\\d+).*", "\\1", str_s.ext_t))
  
  if (str_s.outType == "TEXT") num_n.font_size_1  <- num_n.font_size * 1.045 else
    num_n.font_size_1 <- num_n.font_size 
  
  # --- Build LaTeX wrapper content ---
  
  num_n.headerheight <- round(num_n.font_size_1 * num_n.headers * 1.2, 0)
  num_n.footerheight <- round(num_n.font_size_1 * (num_n.footers) * 1.2, 0)

  if (! is.null(str_s.headers) && ! is.null(str_s.footers)) str_s.noheadfoot <- "includeheadfoot" else
    if (! is.null(str_s.headers)) str_s.noheadfoot <- "includehead" else
      if (! is.null(str_s.footers)) str_s.noheadfoot <- "includefoot" else
        str_s.noheadfoot <- "noheadfoot"
  
  if (str_s.outType == "GG") { 
    # the formulat is not acurate and no reason to explain.
    num_n.bmargin_1 <- base::ifelse(num_n.footers == 0, num_n.bmargin, num_n.bmargin + (num_n.footers + num_n.headers) * num_n.font_size_1/ 72)    
    str_s.this_margin <- sprintf("\\usepackage[letterpaper, %s, top=%sin, bottom=%sin, left=%sin, right=%sin, headsep=%sin, footskip=%spt, %s, nomarginpar, heightrounded]{geometry}", 
                               tolower(str_s.orientation), num_n.tmargin, num_n.bmargin_1, num_n.lmargin, num_n.rmargin, 0, 0, str_s.noheadfoot)    
  } else {
    num_n.bmargin_1 <- base::ifelse(num_n.footers == 0, num_n.bmargin, num_n.bmargin + (num_n.footers + 1) * num_n.font_size_1 / 72)
    str_s.this_margin <- sprintf("\\usepackage[letterpaper, %s, top=%sin, bottom=%sin, left=%sin, right=%sin, headsep=%sin, footskip=%sin, %s, nomarginpar, heightrounded]{geometry}", 
                               tolower(str_s.orientation), num_n.tmargin, num_n.bmargin_1, num_n.lmargin, num_n.rmargin, 0, 0, str_s.noheadfoot) 
  }
  
  # print(c(num_n.footers, num_n.tmargin, num_n.bmargin_1, num_n.lmargin, num_n.rmargin, 0, 0, str_s.noheadfoot))
  str_s.tex_wrapper_file <- paste0(str_s.outdir, "/", str_s.outfile, ".tex")

  # --- Fancy header and footer LaTeX  ---
  if (! is.null(str_s.headers) || ! is.null(str_s.footers)) {
    str_s.fancy_header <- c(
      # "\\usepackage{lipsum} % for dummy text",
      "\\usepackage{fancyhdr}",
      # "\\usepackage{showframe}",
      # "\\usepackage{lastpage}",
      # "\\usepackage{xcolor}",
      # "\\usepackage{refcount}",
      paste0("\\setlength{\\headheight}{", num_n.headerheight, "pt}"), 
      "\\pagestyle{fancy}",
      "\\pagenumbering{arabic}",
      "\\fancyhf{} % Clear default settings",
      str_s.headers,
      str_s.footers,
      "\\renewcommand{\\footrulewidth}{0pt}",
      "\\renewcommand{\\headrulewidth}{0pt} % Add a decorative line"
    )
  } else {
    str_s.fancy_header <- NULL
  }

  str_s.wrapper_header <- c(
    sprintf("\\documentclass[%s]{article}", paste0(num_n.font_size, "pt")),
    "",
    str_s.this_margin,
    "\\usepackage{fontspec}",
    "\\usepackage{booktabs}",
    "\\usepackage{array}",
    "\\usepackage{caption}",
    "\\usepackage{graphicx}",
    str_s.fancy_header, 
    " ",
    sprintf("\\setmonofont{%s}", fontname[1]),
    sprintf("\\setmainfont{%s}", fontname[1]),
    "\\captionsetup{justification=centering, skip=0pt, singlelinecheck=off}",
    " ",
    "% RESET ALL TYPOGRAPHIC SPACING",
    "\\setlength{\\topskip}{0pt}      % First line starts exactly at margin",
    "\\setlength{\\abovecaptionskip}{0pt}",
    "\\setlength{\\textfloatsep}{0pt}",
    "\\setlength{\\parindent}{0pt}    % No indent",
    "\\setlength{\\parskip}{0pt}      % No gap between paragraphs",
    "\\setlength{\\lineskip}{0pt}     % No extra space for tall characters",
    "\\setlength{\\tabcolsep}{0pt}    % Remove the gap between the text and the cell border",
    "\\setlength{\\extrarowheight}{0pt} % Set the distance between row baselines to exactly 10pt (or 10.46pt)", 
    # sprintf("\\setlength{\\baselineskip}{%spt}", num_n.font_size),
    "\\raggedbottom                  % Disable vertical 'stretching'",
    "\\newcommand{\\indentbox}[2]{\\makebox[#1][l]{}#2}",
    "\\renewcommand{\\arraystretch}{1.0} % Remove the extra vertical space LaTeX adds to table rows",
    " ",
    "\\begin{document}",
    "\\pagenumbering{gobble}",
    "% APPLY RIGID FONT/LINE HEIGHT GLOBALLY",
    "\\ttfamily",
    sprintf("\\fontsize{%spt}{%spt}\\selectfont", num_n.font_size, num_n.font_size_1)
  )
  
  # Write to LaTex file
  if (str_s.outType == "GT") { 
    # Insert each gt LaTeX fragment with a page break
    for (i in seq_along(gt_tbl)) {
      # for (i in 1:2) {
      gt_tbl[[i]] <- gt_tbl[[i]] %>%  
        gt::text_transform(
          fn = function(x) gsub("\n", str_s.line_break, gsub(" ", str_s.hold_space, x))
        ) %>%
        gt::text_transform(
          locations = gt::cells_column_labels(),
          fn = function(x) gsub("\n", str_s.line_break, gsub(" ", str_s.hold_space, x))
        ) 
      
      str_s.body_lines <- utils::capture.output(base::cat(as.character(gt::as_latex(gt_tbl[[i]]))))
      # str_s.body_lines <- gsub(str_s.line_break2, "\\\\\\\\", str_s.body_lines)
      str_s.body_lines <- gsub(str_s.line_break2, " \\\\newline \\\\mbox{}", str_s.body_lines)
      str_s.body_lines <- gsub(str_s.line_break, " \\\\newline \\\\mbox{}", str_s.body_lines)
      str_s.body_lines <- stringr::str_replace_all(str_s.body_lines, paste0("\\{(", str_s.hold_space, ")+\\}"), "{}")
      str_s.body_lines <- stringr::str_replace_all(str_s.body_lines, paste0("\\{(", str_s.hold_space, ")+"),
                                          function(m) paste0("{", base::strrep("~", (nchar(m)-1)/nchar(str_s.hold_space))))
      str_s.body_lines <- stringr::str_replace_all(str_s.body_lines, paste0("(", str_s.hold_space, ")+"),
                                          function(m) base::strrep("~", nchar(m)/nchar(str_s.hold_space)))
      str_s.body_lines <- stringr::str_replace_all(str_s.body_lines, "\\\\raggedright", " \\\\relax")
      str_s.body_lines <- gsub("\\\\large\\s*", "", str_s.body_lines)
      # str_s.body_lines <- gsub("\\\\begin\\{minipage\\}\\{", "\\\\begin{minipage}[t]\\{", str_s.body_lines)
      
      # This regex finds ANY fontsize command and replaces it with your flexible variables
      str_s.body_lines <- gsub(
        "\\\\fontsize\\{[^\\}]+\\}\\{[^\\}]+\\}", 
        sprintf("\\\\fontsize{%.1fpt}{%.2fpt}", num_n.font_size, num_n.font_size), 
        str_s.body_lines
      )
      
      # Remove the extra space added by booktabs
      str_s.body_lines <- gsub("\\\\addlinespace\\[.*\\]", "", str_s.body_lines)
      
      if (i == 1) str_s.wrapper <- str_s.wrapper_header else str_s.wrapper <- c(str_s.wrapper, "\\newpage")
      str_s.wrapper <- c(str_s.wrapper, str_s.body_lines)
    }
    
    str_s.wrapper <- c(str_s.wrapper, "\\normalfont", "\\clearpage", "\\label{TotalPages}", "\\end{document}")
    # --- Write wrapper ---
    writeLines(str_s.wrapper, str_s.tex_wrapper_file)
  } else if (str_s.outType == "TEXT") {   
    str_s.wrapper_body_start <- "\\begin{verbatim}"
    str_s.wrapper_body_end <- "\\end{verbatim}"
    str_s.wrapper_end <- "\\end{document}"
    
    str_s.wrapper_body <- unlist(base::mapply(function(x) {
        if (stringr::str_detect(x, "\f")) {
          c(str_s.wrapper_body_end, "\\newpage", str_s.wrapper_body_start, base::gsub("\f", "", x))
        } else x
      }, inobj, SIMPLIFY = TRUE), use.names = FALSE)
    names(str_s.wrapper_body) <- NULL
    
    # writeLines(c(str_s.wrapper_header, str_s.wrapper_body_start, str_s.wrapper_body, str_s.wrapper_end), str_s.tex_wrapper_file)
    writeLines(c(str_s.wrapper_header, str_s.wrapper_body_start, str_s.wrapper_body, str_s.wrapper_body_end, str_s.wrapper_end), str_s.tex_wrapper_file)
  } else if (str_s.outType == "GG") {
    num_n.g_width <- round((num_n.page_width - num_n.lmargin - num_n.rmargin) * 72, 0)
    num_n.g_height <- round((num_n.page_height - num_n.tmargin - num_n.bmargin) * 72 , 0) - num_n.headerheight - num_n.footerheight
    num_n.panel_left_col <- NULL
    num_n.panel_right_col <- NULL
    for (i in 1:seq_along(gt_tbl)) {
      num_n.panel_left_col[i] <- 0
      num_n.panel_right_col[i] <- 0
    }
    # str_s.wrapper_body_start <- c("\\begin{figure}[b]", "\\centering", "\\vspace{-10pt}")
    # str_s.wrapper_body_end <- c("\\vspace{-10pt}", "\\end{figure}")
    str_s.wrapper_body_start <- c("\\begin{figure}", "\\centering")
    str_s.wrapper_body_end <- "\\end{figure}"  
    str_s.wrapper_end <- "\\end{document}"
    for (i in seq_along(gt_tbl)) {
      this_img_file <- paste0(str_s.outdir, "/", str_s.outfile, "___", i, ".png")
      # str_s.wrapper_body_end <- c(paste0("\\label{fig:plot", "", i, "}"), "\\end{figure}")
      # print(c(num_n.g_height/72, num_n.g_width/72))
      ggplot2::ggsave(this_img_file, gt_tbl[[i]], height = base::ceiling(num_n.g_height * 300/72), dpi=300,
                      width = base::ceiling((num_n.g_width - num_n.panel_right_col[i] - num_n.panel_left_col[i]) * 300/72),
                      # height = base::ceiling(num_n.g_height * 300/72),
                      units = "px", scale=base::round((num_n.page_height/num_n.page_width)/(num_n.g_height/num_n.g_width), 2))
      
      str_s.wrapper_body <- paste0("\\includegraphics[width=1\\textwidth]{", this_img_file, "}")
      if (i > 1) str_s.wrapper_body <- c("\\newpage", str_s.wrapper_body)
      writeLines(c(str_s.wrapper_header, str_s.wrapper_body_start, str_s.wrapper_body, str_s.wrapper_body_end, str_s.wrapper_end), str_s.tex_wrapper_file)
    }
  }
  
  # --- Compile PDF ---
  if (tolower(Sys.info()["sysname"]) == "windows") {
    system2(paste0(base::dirname(Sys.getenv("LOCALAPPDATA")), "/Local/Programs/MiKTeX/miktex/bin/x64/xelatex.exe"),  
            args = c(shQuote(paste0("-output-directory=", str_s.outdir)),  shQuote(str_s.tex_wrapper_file)))      
  } else {
    cmd <- sprintf("xelatex -output-directory=%s -interaction=nonstopmode %s", str_s.outdir, str_s.tex_wrapper_file)
    system(cmd)
    system(cmd)   # twice for header/footer consistency
  }
  
  # Remove output .tex, .log and .aux file
  base::unlink(base::paste0(str_s.outdir, "/", str_s.outfile, ".tex"))
  base::unlink(base::paste0(str_s.outdir, "/", str_s.outfile, ".aux"))
  base::unlink(base::paste0(str_s.outdir, "/", str_s.outfile, ".log"))
  if (str_s.outType == "GG") for (i in seq_along(gt_tbl)) {
    base::unlink(paste0(str_s.outdir, "/", str_s.outfile, "___", i, ".png"))
  }
  return(invisible())
}

