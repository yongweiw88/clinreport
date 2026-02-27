#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_noreport.R
# Purpose: Create "No Data to Report" display
# Date:    08/23/2024
#========================================================================= 


# Create plot object

ru_noreport <- function(
  message="No Data to Report"
  ) {
  
  print(paste0("RU_NOREPORT: ", "Start or RU_NOREPORT"))
  
  sty <- create_style(
    font_name = "Courier",
    font_size = G_FONTSIZE,
    text_color = NULL,
    background_color = NULL,
    title_font_size = 10,
    title_font_bold = NULL,
    title_font_color = NULL,
    title_background = NULL,
    footnote_font_bold = NULL,
    footnote_font_color = NULL,
    footnote_background = NULL,
    border_color = NULL,
    table_header_background = NULL,
    table_header_font_bold = NULL,
    table_header_font_color = NULL,
    table_body_background = NULL,
    table_body_stripe = NULL,
    table_body_font_color = NULL,
    table_stub_background = NULL,
    table_stub_font_color = NULL,
    table_stub_font_bold = NULL,
    table_label_row_bold = NULL
    )
  
  # Variables for graphics
  s.titles <- c(G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7)
  s.footers <- c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9)

  G_NTITLES <- 3
  G_NFOOTERS <- 1  
  for (i in 1:length(s.titles)) {
    if (! is.null(s.titles[i])) G_NTITLES <- G_NTITLES + 1
  }
  
  for (i in 1:length(s.footers)) {
    if (! is.null(s.footers[i])) G_NFOOTERS <- G_NFOOTERS + 1
  }  
  
  G_LINESAPCE <- 11.25
  G_TMARGIN <- 1.25
  G_BMARGIN <- 1
  G_LMARGIN <- 1
  G_RMARGIN <- 1
  
  # 2 extra title lines and 3 extra footer lines
  G_HEIGHT <- 8.5 - G_TMARGIN - G_BMARGIN - (G_NFOOTERS + G_NTITLES + 5) * (11.25)/72
  G_WIDTH <- 11 - G_LMARGIN - G_RMARGIN
  
  
  l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  
  ru_index <- function(...) {
    rtn <- base::which(...)
    if (length(rtn) < 1) rtn <- 0
    else rtn <- rtn[1]
    rtn
  }
  
  s.ext <- base::toupper(unlist(base::strsplit(G_OUTFILE, "[.]")))
  s.ext <- s.ext[base::length(s.ext)]
  

  print(paste0("RU_NOREPORT: Get Line size and page size"))
  if (s.ext  %in% names(l.lsmvar)) {
    n.linesize <- unlist(l.lsmvar[s.ext])
    n.pagesize <- unlist(l.psmvar[s.ext])
  } else { 
    if (G_FONTSIZE > 10) s.ext_t < paste0("L", G_FONTSIZE)
    else s.ext_t < paste0("L0", G_FONTSIZE)
    n.linesize <- unlist(l.lsmvar[s.ext_t])
    n.pagesize <- unlist(l.psmvar[s.ext_t])
  }
  
  # footer: blank_row="above" +1, title: blank_row="below" + 1, column label: bank_row-"above" + 1
  n.outsidebodylines <- G_NTITLES + G_NFOOTERS + 1 + 1  + 1
  n.pagesz <- n.pagesize - n.outsidebodylines
  n.pagesz.2 <- floor(n.pagesz/2) - 1
  
  
  print(paste0("RU_NOREPORT: Modify G_DATADATE:", G_DATADATE))
  if (is.null(G_DATADATE) || G_DATADATE[1] =="") {s.righttitleline <- c("Page [pg] of [tpg]", " ")}
  else {s.righttitleline <- c("Page [pg] of [tpg]", paste0("Data Date: ", G_DATADATE))}

  print(paste0("RU_NOREPORT: Orientation"))
  if (base::substr(s.ext, 1, 1) == "L") {
    s.orientation="landscape"
    s.dtype <- "TXT"
  } else if (base::substr(s.ext, 1, 1) == "L") {
    s.orientation="portrait"
    s.dtype <- "TXT"
  } else {
    s.orientation="landscape"
    s.dtype <- s.ext
  }

  if (! base::exists("G_RTFYN")) G_RTFYN <- TRUE 
  if (s.ext == "RTF") s.rtfyn <- FALSE
  else s.rtfyn <- G_RTFYN
  
  s.outdir <- base::dirname(G_OUTFILE)
  s.outfilename <- base::basename(G_OUTFILE)
  s.outfile <- base::substring(s.outfilename, 1, base::nchar(s.outfilename) - base::nchar(s.ext) - 1)
  s.outfile_rtf <- paste0(s.outdir, "/", s.outfile, ".RTF")
  s.outfiles <- base::list.files(s.outdir, pattern=paste0("^", s.outfile, "\\.(RTF|rtf|L[0-9]+|P[0-9]+|PDF|pdf)$"), full.name=TRUE)

  print(paste0("RU_NOREPORT: Delete output before re-create "))

  if (length(s.outfiles) > 0) for (j in 1:length(s.outfiles)) {
    if (base::file.exists(s.outfiles[j])) {
      print(s.outfiles[j])
      base::file.remove(s.outfiles[j])
    }
  }

  print(paste0("RU_NOREPORT: Create output "))
  if (s.rtfyn) {n.loops <- 2} else {n.loops <- 1}
  
  vl.rpt <- list()
  for (j in 1:n.loops) {
    if ( s.dtype == "TXT") {
      vl.rpt.2 <- reporter::create_report(G_OUTFILE, orientation=s.orientation, output_type = s.dtype, units = "inches", paper_size = "letter",
                                        missing = "", font = "fixed", font_size = G_FONTSIZE) 
      vl.rpt.2 <- vl.rpt.2 %>% reporter::options_fixed(cpuom = NULL, lpuom = NULL, min_margin = NULL, blank_margins = FALSE, font_size = G_FONTSIZE, 
                              line_size = n.linesize, line_count = n.pagesize) 
      vl.rpt.2 <- vl.rpt.2 %>%    
        reporter::page_header(c(paste0("Protocol: ", G_STUDY_DESC), paste0("Population: ", G_POPLBL)), right = s.righttitleline) %>%
        reporter::titles(c(G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7), font_size=G_FONTSIZE) %>%
        reporter::footnotes(c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9, 
                              paste0(G_USERID, ': ', G_PGMPTH,' ', format(Sys.time(), "%Y-%m-%d %T"))), blank_row="above", columns=1, align="left") %>%
        reporter::set_margins(top = G_TMARGIN, bottom = G_BMARGIN, right=G_RMARGIN, left=G_LMARGIN) 
      
      vl.tbl <- create_text(paste0(paste0(base::rep("\n", n.pagesz.2), collapse = ""), message), align = "center")
      
      vl.rpt.2 <- reporter::add_content(vl.rpt.2, vl.tbl)
      reporter::write_report(vl.rpt.2)
      # print(vl.rpt.2)
    } else {
      vl.rpt.1 <- reporter::create_report(s.outfile_rtf, orientation=s.orientation, output_type = s.dtype, units = "inches", paper_size = "letter",
                                          missing = "", font = "fixed", font_size = G_FONTSIZE) 
      vl.rpt.1 <- vl.rpt.1 %>% reporter::options_fixed(cpuom = NULL, lpuom = 6, min_margin = NULL, blank_margins = FALSE, font_size = G_FONTSIZE, 
                                                       line_size = NULL, line_count = NULL, uchar=" ")  
      vl.rpt.1 <- vl.rpt.1 %>%    
        reporter::page_header(c(paste0("Protocol: ", G_STUDY_DESC), paste0("Population: ", G_POPLBL)), right = s.righttitleline) %>%
        reporter::titles(c(G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7), font_size=G_FONTSIZE) %>%
        reporter::footnotes(c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9, 
                              paste0(G_USERID, ': ', G_PGMPTH,' ', format(Sys.time(), "%Y-%m-%d %T"))), blank_row="above", columns=1, align="left") %>%
        reporter::set_margins(top = G_TMARGIN, bottom = G_BMARGIN, right=G_RMARGIN, left=G_LMARGIN) 

      vl.tbl <- create_text(paste0(paste0(base::rep("\n", n.pagesz.2), collapse = ""), message), align = "center")
      vl.rpt.1 <- reporter::add_content(vl.rpt.1, vl.tbl)
      vl.rpt.1 <- reporter::add_style(vl.rpt.1, style = sty)
      reporter::write_report(vl.rpt.1)
    }
    s.dtype <- "RTF"
  }
}
