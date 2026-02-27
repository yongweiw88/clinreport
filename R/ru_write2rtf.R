#' R function to create RTF output for clinical reporting.
#'
#' Pass in a dataframe and reporting settings to RTF output generated.
#'
#' @param dsetin Incoming data frame or list of data frames.
#' 
#' @return A RTF output file
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

ru_write2rtf <- function(
    inobj,
    outfile=rfenv$G_OUTFILE,
    fontsize=c(rfenv$G_FONTSIZE, "L10", "L09", "L08", "L07", "L11", "L12", "P10", "P09", "P08", "P07", "P11", "P12"),
    headers=list("left"=c(paste0("Protocol: ", rfenv$G_STUDY_DESC), paste0("Population: ", rfenv$G_POPLBL)),
                 "right"=c("pageof", dplyr::if_else(is.null(rfenv$G_DATADATE) || rfenv$G_DATADATE[1] == "", "",  paste0("Data Date: ", rfenv$G_DATADATE)))),
    footers=list("left"=paste0(rfenv$G_USERID, ": ", rfenv$G_PGMPTH, " ", format(Sys.time(), "%Y-%m-%d %T"))),
    titlelinebreakchar="\a",
    titleblanku8char="\u0A00",
    linepaces=0,
    marginsininch=c("top"=1.25, "bottom"=1, "left"=1, "right"=1)
) {

  this_rtf_escape <- function(text) {
    this_text <- gsub("\\", "\\\\", text, fixed = TRUE)
    this_text <- gsub("{", " \\{", this_text, fixed = TRUE)
    this_text <- gsub("}", " \\}", this_text, fixed = TRUE)  
    return(this_text)
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
  str_s.hold_space_rtf <- paste0("\\\\u", base::utf8ToInt(titleblanku8char))
  str_s.line_break <- titlelinebreakchar
  
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
  system(paste("touch", outfile))
  
  # print(paste0("ru_write2rtf: Get Line size and page size"))
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
  
  # print(paste0("ru_write2rtf: Orientation"))
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
  num_n.n.linesize <- unlist(list_l.lsmvar[str_s.ext_t])
  num_n.n.pagesize <- unlist(list_l.psmvar[str_s.ext_t])
  
  num_n.font_size <- as.numeric(sub(".*?(\\d+).*", "\\1", str_s.ext_t))
  num_n.font_size_1  <- num_n.font_size * 1.045 # adjusted for text output
  
  num_n.n.numofpages <- if (str_s.outType == "TEXT") base::sum(stringr::str_count(inobj, "\f")) + 1 else length(gt_tbl)
  
  # headers
  str_s.leftheaders <- if ("left" %in% names(headers)) unlist(headers[["left"]]) else ""
  str_s.rightheaders <- if ("right" %in% names(headers)) unlist(headers[["right"]]) else ""
  str_s.centerheaders <- if ("center" %in% names(headers)) unlist(headers[["center"]]) else ""
  
  str_s.leftheaders <- base::unlist(stringr::str_split(this_rtf_escape(str_s.leftheaders), "\n"))
  str_s.rightheaders <- base::unlist(stringr::str_split(this_rtf_escape(str_s.rightheaders), "\n"))
  str_s.centerheaders <- base::unlist(stringr::str_split(this_rtf_escape(str_s.centerheaders), "\n"))
  
  num_n.n.headers <- base::max(length(str_s.rightheaders), length(str_s.leftheaders), length(str_s.centerheaders))
  if (num_n.n.headers == 1 & base::gsub(" *", "", paste(str_s.rightheaders, str_s.leftheaders, str_s.centerheaders, sep="", collapse="")) == "") num_n.n.headers <- 0
  
  if (num_n.n.headers > length(str_s.rightheaders)) str_s.rightheaders <- c(str_s.rightheaders, base::rep(" ", num_n.n.headers - length(str_s.rightheaders)))
  if (num_n.n.headers > length(str_s.leftheaders)) str_s.leftheaders <- c(str_s.leftheaders, base::rep(" ", num_n.n.headers - length(str_s.leftheaders)))
  if (num_n.n.headers > length(str_s.centerheaders)) str_s.centerheaders <- c(str_s.centerheaders, base::rep(" ", num_n.n.headers - length(str_s.centerheaders)))
  
  str_s.headers <- NULL
  str_s.page_text <- paste0("pageof", base::strrep("x", nchar(num_n.n.numofpages) * 2 + 3))
  if (num_n.n.headers > 0) {
    for (k in 1:num_n.n.headers) {
      str_s.temp_left <- base::gsub("pageof", str_s.page_text, str_s.leftheaders[k], fixed = TRUE)
      str_s.temp_right <- base::gsub("pageof", str_s.page_text, str_s.rightheaders[k], fixed = TRUE)
      str_s.temp_center <- base::gsub("pageof", str_s.page_text, str_s.centerheaders[k], fixed = TRUE)
      
      str_s.tmp <- paste0(str_s.temp_left, base::strrep(" ", num_n.n.linesize))
      if (! str_s.temp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.n.linesize - base::nchar(str_s.temp_center))/2)), str_s.temp_center)
      if (str_s.temp_right %in% c("", " ")) str_s.temp_right <- ""
      str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.n.linesize - base::nchar(str_s.temp_right)),  str_s.temp_right)
      str_s.headers <- c(str_s.headers, paste0(paste0("\\pard\\ql ", "\\f2\\fs", num_n.font_size * 2, " ", str_s.tmp), " \\par"))
    }  
    str_s.headers[1] <- paste0("{\\header ", str_s.headers[1])
    str_s.headers[length(str_s.headers)] <- paste0(str_s.headers[length(str_s.headers)], "}")
    str_s.headers <- base::gsub(str_s.page_text, paste0("Page {\\chpgn} of ", num_n.n.numofpages), str_s.headers, fixed = TRUE)
  } 
  
  # footers
  str_s.leftfooters <- if ("left" %in% names(footers)) unlist(footers[["left"]]) else ""
  str_s.rightfooters <- if ("right" %in% names(footers)) unlist(footers[["right"]]) else ""
  str_s.centerfooters <- if ("center" %in% names(footers)) unlist(footers[["right"]]) else ""
  
  str_s.leftfooters <- base::unlist(stringr::str_split(this_rtf_escape(str_s.leftfooters), "\n"))
  str_s.rightfooters <- base::unlist(stringr::str_split(this_rtf_escape(str_s.rightfooters), "\n"))
  str_s.centerfooters <- base::unlist(stringr::str_split(this_rtf_escape(str_s.centerfooters), "\n"))
  
  num_n.n.footers <- base::max(length(str_s.rightfooters), length(str_s.leftfooters), length(str_s.centerfooters))
  if (num_n.n.footers == 1 && base::gsub(" *", "", paste0(str_s.rightfooters, str_s.leftfooters, str_s.centerfooters)) == "") num_n.n.footers <- 0
  
  if (num_n.n.footers > length(str_s.rightfooters)) str_s.rightfooters <- c(str_s.rightfooters, base::rep(" ", num_n.n.footers - length(str_s.rightfooters)))
  if (num_n.n.footers > length(str_s.leftfooters)) str_s.leftfooters <- c(str_s.leftfooters, base::rep(" ", num_n.n.footers - length(str_s.leftfooters)))
  if (num_n.n.footers > length(str_s.centerfooters)) str_s.centerfooters <- c(str_s.centerfooters, base::rep(" ", num_n.n.footers - length(str_s.centerfooters)))
  
  str_s.footers <- NULL
  if (num_n.n.footers > 0) {
    for (k in 1:num_n.n.footers) {
      str_s.temp_left <- base::gsub("pageof", str_s.page_text, str_s.leftfooters[k], fixed = TRUE)
      str_s.temp_right <- base::gsub("pageof", str_s.page_text, str_s.rightfooters[k], fixed = TRUE)
      str_s.temp_center <- base::gsub("pageof", str_s.page_text, str_s.centerfooters[k], fixed = TRUE)
      
      str_s.tmp <- paste0(str_s.temp_left, base::strrep(" ", num_n.n.linesize))
      if (! str_s.temp_center %in% c("", " ")) str_s.tmp <- paste0(base::substring(str_s.tmp, 1, base::floor((num_n.n.linesize - base::nchar(str_s.temp_center))/2)), str_s.temp_center)
      if (str_s.temp_right %in% c("", " ")) str_s.temp_right <- ""
      str_s.tmp <- paste0(base::substring(str_s.tmp, 1, num_n.n.linesize - base::nchar(str_s.temp_right)),  str_s.temp_right)
      str_s.footers <- c(str_s.footers, paste0(paste0("\\pard\\ql ", "\\f2\\fs", num_n.font_size * 2, " ", str_s.tmp), " \\par"))
    }  
    str_s.footers[1] <- paste0("{\\footer ", str_s.footers[1])
    str_s.footers[length(str_s.footers)] <- paste0(str_s.footers[length(str_s.footers)], "}")
    str_s.footers <- base::gsub(str_s.page_text, paste0("Page {\\chpgn} of ", num_n.n.numofpages), str_s.footers, fixed = TRUE)
  } 

  # --- Build LaTeX wrapper content ---
  
  num_n.n.headerheight <- num_n.font_size * num_n.n.headers * 1.2
  num_n.n.footerheight <- num_n.font_size * (num_n.n.footers) * 1.2
  
  # num_n.n.footerheight <- base::ceiling(num_n.n.footers * (num_n.page_height - num_n.tmargin - num_n.bmargin)/(num_n.n.pagesize)  * 72)

  if (str_s.outType == "GT") num_n.font_size_1 <- num_n.font_size else
    num_n.font_size_1 <- num_n.font_size * 1.045
    
  # num_n.bmargin_1 <- base::ifelse(num_n.n.footers == 0, num_n.bmargin, num_n.bmargin + (num_n.n.footers + 0) * num_n.font_size / 72)
  # num_n.tmargin_1 <- base::ifelse(num_n.n.headers == 0, num_n.tmargin, num_n.tmargin + (num_n.n.headers + 0) * num_n.font_size / 72)
  num_n.bmargin_1 <- num_n.bmargin
  num_n.tmargin_1 <- num_n.tmargin

  # --- Build RTF wrapper content ---

  num_n.space    <- num_n.font_size * 10 * 2 + linepaces
  str_s.condent  <- ""
  
  # Initialize the Character Vector
  # We start by building the header lines
  str_s.rtf_content_header <- c(
    '{\\rtf1\\ansi\\ansicpg1252\\uc1 \\deff0\\deflang1033\\deflangfe1033',
    '{\\fonttbl',
    '{\\f0\\froman\\fcharset0\\fprq2{\\*\\panose 02020603050405020304}Times New Roman;}',
    '{\\f2\\fmodern\\fcharset0\\fprq1{\\*\\panose 02070309020205020404}Courier New;}',
    '{\\f44\\fmodern\\fcharset0\\fprq1{\\*\\panose 020b0609020202020204}SAS Monospace;} ',
    '{\\f45\\froman\\fcharset238\\fprq2 Times New Roman CE;}',
    '{\\f46\\froman\\fcharset204\\fprq2 Times New Roman Cyr;}',
    '{\\f48\\froman\\fcharset161\\fprq2 Times New Roman Greek;}',
    '{\\f49\\froman\\fcharset162\\fprq2 Times New Roman Tur;}',
    '{\\f50\\froman\\fcharset186\\fprq2 Times New Roman Baltic;}',
    '{\\f57\\fmodern\\fcharset238\\fprq1 Courier New CE;}',
    '{\\f58\\fmodern\\fcharset204\\fprq1 Courier New Cyr;}',
    '{\\f60\\fmodern\\fcharset161\\fprq1 Courier New Greek;}',
    '{\\f61\\fmodern\\fcharset162\\fprq1 Courier New Tur;}',
    '{\\f62\\fmodern\\fcharset186\\fprq1 Courier New Baltic;}}',
    '{\\stylesheet{\\widctlpar\\adjustright \\fs20\\cgrid \\snext0 Normal;}',
    '{\\*\\cs10 \\additive Default Paragraph Font;}}',
    str_s.headers,
    str_s.footers
  )
  
  # Add Page Setup Strings
  str_s.rtf_content_header <- c(str_s.rtf_content_header, 
                              sprintf("\\paperw%s\\paperh%s\\margl%s\\margr%s\\margt%s\\margb%s", 
                                      round(num_n.page_width * 1440, 0), round(num_n.page_height * 1440, 0), round(num_n.lmargin * 1440, 0), 
                                      round(num_n.rmargin * 1440, 0), round(num_n.tmargin * 1440, 0), round(num_n.bmargin * 1440, 0)),
                              "\\widowctrl\\ftnbj\\aenddoc\\hyphcaps0\\formshade\\viewkind1\\viewscale100\\pgbrdrhead\\pgbrdrfoot"
  )
  
  if (str_s.orientation == "landscape") {
    str_s.rtf_content_header <- c(str_s.rtf_content_header, 
                                sprintf("\\fet0\\sectd \\lndscpsxn\\psz1\\linex0\\headery%s\\footery%s\\endnhere\\sectdefaultcl", 
                                        round(num_n.tmargin_1 * 1440, 0), round(num_n.bmargin_1 * 1440)))
  } else {
    str_s.rtf_content_header <- c(str_s.rtf_content_header, 
                                sprintf("\\fet0\\sectd \\psz1\\linex0\\headery%s\\footery%s\\endnhere\\sectdefaultcl", 
                                        round(num_n.tmargin_1 * 1440, 0), round(num_n.bmargin_1 * 1440)))      
  }

  if (str_s.outType == "GT") {
    str_s.body_lines <- NULL
    for (i in seq_along(gt_tbl)) {
    # for (i in 1:1) {
      gt_tbl[[i]] <- gt_tbl[[i]] %>%
        gt::text_transform(
          fn = function(x) gsub("\n", str_s.line_break, x)
        ) %>%
        gt::text_transform(
          locations = gt::cells_column_labels(),
          fn = function(x) gsub("\n", str_s.line_break, x)
        )
      # gt_tbl[[i]] <- gt_tbl[[i]] %>%  
      #   gt::text_transform(
      #     fn = function(x) gsub("\n", str_s.line_break, gsub(" ", str_s.hold_space, x))
      #   ) %>%
      #   gt::text_transform(
      #     locations = gt::cells_column_labels(),
      #     fn = function(x) gsub("\n", str_s.line_break, gsub(" ", str_s.hold_space, x))
      #   ) 
      
      str_s.this_body_lines <- utils::capture.output(base::cat(as.character(gt::as_rtf(gt_tbl[[i]]))))
      
      k <- base::grep("\\\\trowd", str_s.this_body_lines)[1]
      if (!is.na(k)) {
        str_s.this_body_lines <- c("{", str_s.this_body_lines[k:length(str_s.this_body_lines)])
      }
      
      if (is.null(str_s.body_lines)) str_s.body_lines <- str_s.this_body_lines else
        str_s.body_lines <- c(str_s.body_lines, "\\page", str_s.this_body_lines)
    }      

    str_s.body_lines <- gsub(str_s.hold_space_rtf, " ", str_s.body_lines)
    str_s.body_lines <- gsub("\\\\sectd", "\\\\landscape\\\\sectd", str_s.body_lines) 
    str_s.body_lines <- gsub("\\\\clpadt\\d+", "\\\\clpadt0", str_s.body_lines)
    str_s.body_lines <- gsub("\\\\clpadb\\d+", "\\\\clpadb0", str_s.body_lines)
    str_s.body_lines <- gsub("\\\\clpadl\\d+", "\\\\clpadl0", str_s.body_lines) 
    str_s.body_lines <- gsub("\\\\clpadr\\d+", "\\\\clpadr0", str_s.body_lines) 
    str_s.body_lines <- gsub("\\\\clpadft\\d+", "\\\\clpadtf0", str_s.body_lines)
    str_s.body_lines <- gsub("\\\\clpadfb\\d+", "\\\\clpadbf0", str_s.body_lines)   
    str_s.body_lines <- gsub("\\\\clpadfl\\d+", "\\\\clpadlf0", str_s.body_lines) 
    str_s.body_lines <- gsub("\\\\clpadfr\\d+", "\\\\clpadrf0", str_s.body_lines)  
    str_s.body_lines <- gsub("\\\\qc", "\\\\ql", str_s.body_lines)  
    if (!is.null(titlelinebreakchar)) str_s.body_lines <- gsub(titlelinebreakchar, "\\\\par ", str_s.body_lines)
    if (!is.null(str_s.line_break)) str_s.body_lines <- gsub(str_s.line_break, "\\\\par ", str_s.body_lines)
    str_s.body_lines <- gsub("\\\\f\\d+", "\\\\f2", str_s.body_lines)
    str_s.body_lines <- gsub("(\\\\pard.* )", sprintf("\\1\\\\sl-%s\\\\slmult0\\\\widctlpar\\\\adjustright", num_n.space),  str_s.body_lines)

    # str_s.body_lines <- gsub("\\\\trrh0", paste0("\\\\trrh", num_n.font_size * 1.25 * 20), str_s.body_lines)
    # str_s.body_lines <- gsub("\\\\pard", paste0("\\\\pard\\\\sl-", num_n.font_size * 20, "\\\\slmult0"), str_s.body_lines)
    # str_s.body_lines <- gsub("\\\\clvertalc", "\\\\clvertalt", str_s.body_lines)    
    
    str_s.rtf_content <- c(str_s.rtf_content_header, str_s.body_lines)
    str_s.rtf_content <- c(str_s.rtf_content, "}")
    writeLines(str_s.rtf_content, con = outfile) 
  } else if (str_s.outType == "TEXT") {
    str_s.rtf_content_header <- c(str_s.rtf_content_header, 
                                sprintf("\\pard\\plain \\sl-%s\\slmult0\\widctlpar\\adjustright \\fs%s\\cgrid {\\f2\\fs%s%s", 
                                        num_n.space, num_n.font_size * 2, num_n.font_size * 2, str_s.condent)
    )
    
    # Process Input File (Replacing Sed/Gawk logic)
    str_s.body_lines <- inobj
    
    # Special characters and page break
    str_s.body_lines <- this_rtf_escape(str_s.body_lines)
    str_s.body_lines <- gsub("\f", "\\page ", str_s.body_lines, fixed = TRUE)
    str_s.body_lines <- paste0(str_s.body_lines, " \\par")
    str_s.body_lines <- gsub("\\page  \\par", "\\page", str_s.body_lines, fixed = TRUE)
    str_s.body_lines <- gsub(str_s.hold_space, " ", str_s.body_lines, fixed = TRUE)
    
    # increase the line space for lines with "_".
    str_s.body_lines <- sapply(str_s.body_lines, function(x) {
      if (grepl("_", x, fixed = TRUE)) {
        c("}",
          sprintf("\\pard\\plain \\widctlpar\\adjustright \\fs%s\\cgrid ", num_n.font_size * 2),
          sprintf("{\\f2\\fs%s%s %s}", num_n.font_size * 2, str_s.condent, x),
          sprintf("\\pard\\plain \\sl-%s\\slmult0\\widctlpar\\adjustright \\fs%s\\cgrid ", num_n.space, num_n.font_size * 2),
          sprintf("{\\f2\\fs%s%s ", num_n.font_size * 2, str_s.condent)
        )
      } else {
        x
      }
    })
    names(str_s.body_lines) <- NULL
    str_s.body_lines <- unlist(str_s.body_lines)
    str_s.body_lines <- c(str_s.body_lines, "}")
    
    str_s.rtf_content <- c(str_s.rtf_content_header, str_s.body_lines)
    
    # 5. Close the RTF bracket
    str_s.rtf_content <- c(str_s.rtf_content, "}")
    
    # 6. Save characters into file with writeLines
    writeLines(str_s.rtf_content, con = outfile) 
  } else if (str_s.outType == "GG") {
    str_s.body_lines <- NULL
    for (i in seq_along(gt_tbl)) {
      if (i > 1) str_s.body_lines <- c(str_s.body_lines, "\\page")
      num_n.n.g_width <- round((num_n.page_width - num_n.lmargin - num_n.rmargin) * 72, 0)
      num_n.n.g_height <- round((num_n.page_height - num_n.tmargin - num_n.bmargin) * 72 , 0) - num_n.n.headerheight - num_n.n.footerheight
      num_n.panel_left_col <- NULL
      num_n.panel_right_col <- NULL
      for (i in 1:seq_along(gt_tbl)) {
        num_n.panel_left_col[i] <- 0
        num_n.panel_right_col[i] <- 0
      }
      
      this_img_file <- paste0(str_s.outdir, "/", str_s.outfile, "___", i, ".png")
      # str_s.wrapper_body_end <- c(paste0("\\label{fig:plot", "", i, "}"), "\\end{figure}")
      ggplot2::ggsave(this_img_file, gt_tbl[[i]], height = base::ceiling(num_n.n.g_height * 300/72), dpi=300,
                      width = base::ceiling((num_n.n.g_width - num_n.panel_right_col[i] - num_n.panel_left_col[i]) * 300/72),
                      # height = base::ceiling(num_n.n.g_height * 300/72),
                      units = "px", scale=base::round((num_n.page_height/num_n.page_width)/(num_n.n.g_height/num_n.n.g_width), 2))
      
      str_s.raw_data <- base::readBin(this_img_file, what = "raw", n = base::file.info(this_img_file)$size)
      # Convert bytes to a single long hexadecimal string
      str_s.hex_string <- paste(str_s.raw_data, collapse = "")
      
      str_s.body_lines <- c(str_s.body_lines,
        sprintf("{\\pard\\qc {\\pict\\pngblip\\picwgoal%d\\pichgoal%d",
                (num_n.n.g_width - num_n.panel_right_col[i] - num_n.panel_left_col[i]) * 20, 
                num_n.n.g_height * 20), 
        str_s.hex_string,
        "} \\par}"
      )
    }
    str_s.rtf_content <- c(str_s.rtf_content_header, str_s.body_lines)
    
    # 5. Close the RTF bracket
    str_s.rtf_content <- c(str_s.rtf_content, "}")
    
    # 6. Save characters into file with writeLines
    writeLines(str_s.rtf_content, con = outfile) 
  }
  
  if (str_s.outType == "GG") for (i in seq_along(gt_tbl)) {
    base::unlink(paste0(str_s.outdir, "/", str_s.outfile, "___", i, ".png"))
  }
  
  return(invisible())
}


