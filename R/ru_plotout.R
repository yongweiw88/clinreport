#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_plotout.R
# Purpose: Create graphic outputs
# Date:    08/23/2024
#========================================================================= 


# Create plot object

ru_plotout <- function(
  plot=NULL,
  linesizeadjust=0.9,
  pagesizeadjust=0.9
  ) {
  
  print(paste0("RU_PLOTOUT: ", "Start or RU_PLOTOUT"))
  if (! exists("G_FONTSIZE")) G_FONTSIZE <- 10
  
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
  str_titles <- c(G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7)
  str_footers <- c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9)
  
  l_title1 <- case_when(
    G_DSPLYTYP == "F" ~ "Figure",
    G_DSPLYTYP == "T" ~ "Table",
    G_DSPLYTYP == "L" ~ "Listing",
    TRUE ~ NA
  )
  
  l_title1 <- paste(l_title1, G_DSPLYNUM, sep=" ")

  G_NTITLES <- 3
  G_NFOOTERS <- 1  
  for (i in 1:length(str_titles)) {
    if (! is.null(str_titles[i])) G_NTITLES <- G_NTITLES + 1
  }
  
  for (i in 1:length(str_footers)) {
    if (! is.null(str_footers[i])) G_NFOOTERS <- G_NFOOTERS + 1
  }  
  
  if (! exists("G_LINESAPCE")) G_LINESAPCE <- 1.3
  if (! exists("G_TMARGIN")) G_TMARGIN <- 1.25
  if (! exists("G_BMARGIN")) G_BMARGIN <- 1
  if (! exists("G_LMARGIN")) G_LMARGIN <- 1
  if (! exists("G_RMARGIN")) G_RMARGIN <- 1
  
  # 2 extra title lines and 3 extra footer lines
  if (! exists("G_HEIGHT")) G_HEIGHT <- (8.5 - G_TMARGIN - G_BMARGIN - (G_NFOOTERS + G_NTITLES + 2) * G_FONTSIZE * G_LINESAPCE/72) 
  if (! exists("G_WIDTH")) G_WIDTH <- (11 - G_LMARGIN - G_RMARGIN) 
  
  print(c(G_WIDTH, G_HEIGHT))
  
  l_width <- G_WIDTH * linesizeadjust
  l_height <- G_HEIGHT * pagesizeadjust
  
  l_tmargin <- G_TMARGIN + 2 * (G_FONTSIZE + G_LINESAPCE)/72
  
  # l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  # l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  
  ru_index <- function(...) {
    rtn <- base::which(...)
    if (length(rtn) < 1) rtn <- 0
    else rtn <- rtn[1]
    rtn
  }
  
  str_ext <- base::toupper(unlist(base::strsplit(G_OUTFILE, "[.]")))
  str_ext <- str_ext[base::length(str_ext)]
  
  # footer: blank_row="above" +1, title: blank_row="below" + 1, column label: bank_row-"above" + 1
  # n_outsidebodylines <- G_NTITLES + G_NFOOTERS + 1 + 1  + 1
  
  
  print(paste0("RU_PLOTOUT: Modify G_DATADATE:", G_DATADATE))
  if (is.null(G_DATADATE) || G_DATADATE[1] =="") {str_righttitleline <- c("Page [pg] of [tpg]", " ")} else 
    {str_righttitleline <- c("Page [pg] of [tpg]", paste0("Data Date: ", G_DATADATE))}

  print(paste0("RU_PLOTOUT: Orientation"))
  if (base::substr(str_ext, 1, 1) == "L") {
    str_orientation="landscape"
    str_dtype <- "TXT"
  } else if (base::substr(str_ext, 1, 1) == "L") {
    str_orientation="portrait"
    str_dtype <- "TXT"
  } else {
    str_orientation="landscape"
    str_dtype <- str_ext
  }

  if (! base::exists("G_RTFYN")) G_RTFYN <- TRUE 
  if (str_ext == "RTF") str_rtfyn <- FALSE else
    str_rtfyn <- G_RTFYN
  
  str_outdir <- base::dirname(G_OUTFILE)
  str_outfilename <- base::basename(G_OUTFILE)
  str_outfile <- base::substring(str_outfilename, 1, base::nchar(str_outfilename) - base::nchar(str_ext) - 1)
  str_outfile_rtf <- paste0(str_outdir, "/", str_outfile, ".RTF")
  str_outfiles <- base::list.files(str_outdir, pattern=paste0("^", str_outfile, "\\.(RTF|rtf|L[0-9]+|P[0-9]+|PDF|pdf)$"), full.name=TRUE)

  print(paste0("RU_PLOTOUT: Delete output before re-create "))

  if (length(str_outfiles) > 0) for (j in 1:length(str_outfiles)) {
    if (base::file.exists(str_outfiles[j])) {
      base::file.remove(str_outfiles[j])
    }
  }

  print(paste0("RU_PLOTOUT: Create output "))
  if (str_rtfyn & toupper(str_ext) != "RTF") {n_loops <- 2} else {n_loops <- 1}
  
  plot_rpt <- list()
  for (j in 1:n_loops) {
    if ( str_dtype == "PDF") {
      plot_rpt_1 <- reporter::create_report(G_OUTFILE, orientation=str_orientation, output_type = str_dtype, units = "inches", paper_size = "letter",
                                          missing = "", font = "fixed", font_size = G_FONTSIZE)
      n.adjust <- 1
    } else {
      plot_rpt_1 <- reporter::create_report(str_outfile_rtf, orientation=str_orientation, output_type = str_dtype, units = "inches", paper_size = "letter",
                                          missing = "", font = "fixed", font_size = G_FONTSIZE) 
      n.adjust <- 0.87
    }
    plot_rpt_1 <- plot_rpt_1 %>% reporter::options_fixed(cpuom = NULL, lpuom = 6, min_margin = NULL, blank_margins = FALSE, font_size = G_FONTSIZE, 
                                                     line_size = NULL, line_count = NULL, uchar=" ")  
    plot_rpt_1 <- plot_rpt_1 %>%    
      reporter::page_header(c(paste0("Protocol: ", G_STUDY_DESC), paste0("Population: ", G_POPLBL)), right = str_righttitleline) %>%
      reporter::titles(c(l_title1, G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7), font_size=G_FONTSIZE) %>%
      reporter::footnotes(c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9, 
                            paste0(G_USERID, ': ', G_PGMPTH,' ', format(Sys.time(), "%Y-%m-%d %T"))), blank_row="above", columns=1, align="left") %>%
      reporter::set_margins(top = l_tmargin, bottom = G_BMARGIN, right=G_RMARGIN, left=G_LMARGIN) 

    plot_plot_obj <- list()

    if (length(class(plot)) == 1 & class(plot[1]) == "list" && is.object(plot[[1]])){
      for (i in 1:length(plot)) {
        plot_plot_obj[[i]] <- create_plot(plot[[i]], height = l_height * n.adjust, width = l_width)
        plot_rpt_1 <- add_content(plot_rpt_1, plot_plot_obj[[i]])
      }
    } else {
        plot_plot_obj <- create_plot(plot, height = l_height * n.adjust, width = l_width)
        plot_rpt_1 <- add_content(plot_rpt_1,  plot_plot_obj)
    }
    plot_rpt_1 <- reporter::add_style(plot_rpt_1, style = sty)
    reporter::write_report(plot_rpt_1)
    str_dtype <- "RTF"
  }
}
