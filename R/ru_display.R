#========================================================================= 
# Author:  Yongwei Wang
# Program: ru_display.R
# Purpose: Create dispaly
# Date:    08/23/2024
#========================================================================= 


# Create plot object

ru_display <- function(
    dsetin=NULL,
    columns=NULL,
    spanningheaders=NULL,                           # i.e. c("'min.x', 'max.c', label = 'cross column header'") - add 'cross column header' above column min.c/max.c column header.
    noprintvars=NULL,
    ordervars=NULL,
    orders=c(1),
    nowidowvar=NULL,
    leftvars=NULL,
    rightvars=NULL,
    centervars=NULL,
    colspacing=1,
    widths=NULL,
    formats=NULL,
    varlabels=NULL,
    skipvars=NULL,
    idvars=NULL,
    sharecolvars=NULL,              
    sharecolvarsindent=2,
    computebeforepagevars=NULL,
    computebeforepagelines=NULL,
    proptions="first_row_blank=TRUE, borders = c('top', 'bottom')",
    linesizeadjust=0.9,
    pagesizeadjust=0.9
  ) {
  
  print(paste0("RU_DISPLAY: ", "Start or RU_DISPLAY"))
  
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

  G_NTITLES <- 4
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
  G_HEIGHT <- 8.5 - G_TMARGIN - G_BMARGIN - (G_NFOOTERS + G_NTITLES + 5) #* (11.25)/72
  G_WIDTH <- 11 - G_LMARGIN - G_RMARGIN
  
  
  G_TITLE0 <- "Table"
  if (G_DSPLYTYP == "T" ) G_TITLE0 <- "Table"
  if (G_DSPLYTYP == "L" ) G_TITLE0 <- "Listing"
  if (G_DSPLYTYP == "F" ) G_TITLE0 <- "Figure"
  
  G_TITLE0 <- paste0(G_TITLE0, " ", G_DSPLYNUM)
    
  l.lsmvar <- list("P08"=90, "P09"=80, "P10"=72, "P11"=65, "P12"=64 , "L08"=135, "L09"=120, "L10"=108, "L11"=98, "L12"=90)
  l.psmvar <- list("P08"=83, "P09"=74, "P10"=67, "P11"=61, "P12"=56 , "L08"=54,  "L09"=48,  "L10"=43,  "L11"=39, "L12"=36)
  
  ru_index <- function(...) {
    rtn <- base::which(...)
    if (length(rtn) < 1) rtn <- 0
    else rtn <- rtn[1]
    rtn
  }
  
  columns <- ru_expvarlist(dsetin, columns)
  
  if (! is.null(computebeforepagevars)) {
    noprintvars <- c(computebeforepagevars, noprintvars)
    ordervars <- c(computebeforepagevars, ordervars)
    columns <- c(computebeforepagevars, columns)
  }

  s.ext <- base::toupper(unlist(base::strsplit(G_OUTFILE, "[.]")))
  s.ext <- s.ext[base::length(s.ext)]
  
  d.dsetin <- dsetin
  if (! is.null(varlabels) && ! is.na(varlabels[1]) ) {
    d.dsetin <- ru_labels(d.dsetin, varlabels=varlabels)
  }
  
  print(paste0("RU_DISPLAY: ORDERVARS: ", ordervars))
  
  if (! (is.null(ordervars) || is.na(ordervars[1]) || ordervars[1] =="")) {
    d.dsetin <- data.table::setorderv(d.dsetin, ordervars, orders)
  }     

  print(paste0("RU_DISPLAY: Get Line size and page size"))
  if (s.ext  %in% names(l.lsmvar)) {
    n.linesize <- unlist(l.lsmvar[s.ext])
    n.pagesize <- unlist(l.psmvar[s.ext])
  } else { 
    if (G_FONTSIZE >= 10) s.ext_t <- paste0("L", G_FONTSIZE)
    else s.ext_t <- paste0("L0", G_FONTSIZE)
    n.linesize <- l.lsmvar[[s.ext_t]]
    n.pagesize <- l.psmvar[[s.ext_t]]
  }
  
  print("RU_DISPLAY: Modify COLUMNS")
  s.columns <- NULL
  n.sharecolvars <- 0
  s.sharecols <- NULL
  for (i in 1:length(columns)) {
    s.name <-columns[i]
    if (s.name %in% sharecolvars && n.sharecolvars < length(sharecolvars) - 1) {
      n.sharecolvars <- n.sharecolvars + 1
      if (is.null(s.sharecols)) s.sharecols <- s.name
      else s.sharecols <- c(s.sharecols, s.name)
    } else if (! (s.name %in% noprintvars)) {
      if ( ! (s.name %in% sharecolvars && n.sharecolvars >= length(sharecolvars))) {
        if (is.null(s.columns)) s.columns <- s.name
        else s.columns <- c(s.columns, s.name)
      }
    }
  }

  s.rptcolumns <- NULL
  for (i in 1:length(columns)) {
    s.name <- columns[i]
    if (! (s.name %in% noprintvars)) {
      if (is.null(s.rptcolumns)) s.rptcolumns <- s.name
      else s.rptcolumns <- c(s.rptcolumns, s.name)
    }
  }

  print(paste0("RU_DISPLAY: Get Widths"))
  # print(s.columns)
  cwidths <- ru_width_rtf(d.dsetin, c(s.columns), widths, type="NCHAR")
  
  if (! base::exists("G_RTFYN")) G_RTFYN <- TRUE 
  if (s.ext == "RTF") s.rtfyn <- FALSE
  else s.rtfyn <- G_RTFYN
  
  # print(n.linesize)
  if (is.null(n.linesize)) {
    widths <- ((cwidths + colspacing)/100) * linesizeadjust * G_WIDTH * (G_WIDTH * 12 /100)
  } else {
    widths <- ((cwidths + colspacing))/n.linesize * linesizeadjust * G_WIDTH * (n.linesize/sum(cwidths + colspacing)) * (G_WIDTH * 12 /n.linesize)
  }  
  
print(paste("widths:", paste(widths, collapse = ",", sep="-"), " total widths:", sum(widths), " G_WIDTH: ", G_WIDTH, "n.linesize:", n.linesize, sep=" "))

  # widths <- ru_width_rtf(dsetin, c(s.columns), widths) * linesizeadjust * G_WIDTH/100
  # if (sum(widths) > G_WIDTH) {widths[length(widths)] <- widths[length(widths)] - sum(widths) + G_WIDTH}  
  
  if (! is.null(s.sharecols)) {
    if (! (sharecolvars[length(sharecolvars)] %in% names(widths))) {
      cwidths[sharecolvars[length(sharecolvars)]] <- cwidths[sharecolvars[length(sharecolvars)]] + (length(sharecolvars) - 1) * sharecolvarsindent
    }
    for (i in 1:length(s.sharecols)) {
      cwidths[s.sharecols[i]] <- cwidths[sharecolvars[length(sharecolvars)]]
      noprintvars <- c(noprintvars, s.sharecols)
    }
  }
  
  print(cwidths)
  print(paste0("RU_DISPLAY: Sharecolvars Label"))

  s.sharecolvarlabel <- NULL
  n.outsidebodylines <- 0
  l.label <-list()
  if (! is.null(sharecolvars)) {
    for (i in 1:length(sharecolvars)) {
      s.thislabel <- base::lapply(d.dsetin, base::attr, "label")[sharecolvars[i]]
      s.thislabel <- stringr::str_wrap(s.thislabel, width = cwidths[sharecolvars[i]], 
                     indent = sharecolvarsindent * (i-1), exdent = (i - 1) * sharecolvarsindent, whitespace_only = TRUE)
      if (! (is.null(s.thislabel) || s.thislabel == "")) {
        if (is.null(s.sharecolvarlabel)) s.sharecolvarlabel <- s.thislabel
        else s.sharecolvarlabel <- paste0(s.sharecolvarlabel, "\n", s.thislabel)
      }
    }
    l.label[[sharecolvars[length(sharecolvars)]]] <- s.sharecolvarlabel
    n.outsidebodylines <- max(n.outsidebodylines, stringr::str_count(s.sharecolvarlabel, "[\n]") + 1)
  }  
  
  print(paste0("RU_DISPLAY: Wrap Labels"))
  for (i in 1:length(s.columns)) {
    if (is.null(sharecolvars) || ! (s.columns[i] %in% sharecolvars)) { 
      s.thislabel <- base::lapply(d.dsetin, base::attr, "label")[s.columns[i]]
      s.thislabel <- stringr::str_wrap(s.thislabel, width = cwidths[s.columns[i]], indent = 0, exdent = 0, whitespace_only = TRUE)
      l.label[[s.columns[i]]] <- s.thislabel
      n.outsidebodylines <- max(n.outsidebodylines, stringr::str_count(s.thislabel, "[\n]") + 1)
    }
  }   
  ru_labels(dsetin, l.label)
  
  if ( is.null(proptions) || (any(grep("first_row_blank\\w*=\\w*TRUE", proptions) > 0 ))) n.outsidebodylines <- n.outsidebodylines + 1
  if ( ! is.null(spanningheaders)) n.outsidebodylines <- n.outsidebodylines + 1
  
  # footer: blank_row="above" +1, title: blank_row="below" + 1, column label: bank_row-"above" + 1
  n.outsidebodylines <- n.outsidebodylines + G_NTITLES + G_NFOOTERS + 1 + 1 + 1
  print(paste0("n title/footer :", G_NTITLES, "-", G_NFOOTERS))

  # print(n.outsidebodylines)
  
  print(paste0("RU_DISPLAY: Wrap Column Values"))
  
  for (i in 1:base::nrow(d.dsetin)) {
    n.linesinarow <- 0
    for (j in 1:length(s.rptcolumns)) {
      s.thisvalue <- d.dsetin[i, s.rptcolumns[j]]
      b.visiable <- TRUE
      if (! is.null(sharecolvars) &&  s.rptcolumns[j] %in% sharecolvars) {
        n.index <- ru_index(s.rptcolumns[j] == sharecolvars)
        if (n.index != length(sharecolvars)) b.visiable <- FALSE 
        s.thisvalue <- stringr::str_wrap(s.thisvalue, width = cwidths[s.rptcolumns[j]], indent = (n.index - 1) * sharecolvarsindent, 
                       exdent = (n.index - 1) * sharecolvarsindent, whitespace_only = TRUE)
      } else {
        s.thisvalue.1 <- base::substring(s.thisvalue, 1, 1)
        s.thisvalue.2 <- paste0("a", base::substring(s.thisvalue, 2))        
        s.thisvalue.2 <- stringr::str_wrap(s.thisvalue.2, width = cwidths[s.rptcolumns[j]], indent = 0, exdent = 0, whitespace_only = TRUE)
        s.thisvalue <- paste0(s.thisvalue.1, base::substring(s.thisvalue.2, 2))
      }
      d.dsetin[i, s.rptcolumns[j]] <- s.thisvalue
      if (b.visiable) n.linesinarow <- max(n.linesinarow,  stringr::str_count(s.thisvalue, "[\n]") + 1)
    }  
    d.dsetin[i, "nlines__"] <- n.linesinarow
  }
  
  s.skipvars <- skipvars
  if (! (is.null(skipvars) || is.null(nowidowvar))) {
    s.skipvars <- NULL
    for (i in 1:length(skipvars)) {
      n.ind <- ru_index(nowidowvar == columns)
      if ( n.ind <= ru_index(skipvars[i] == columns)) {
        if (is.null(s.skipvars)) s.skipvars <- skipvars[i]
        else s.skipvars <- c(s.skipvars, skipvars[i])
      } 
    }
  }
  
  print(paste0("RU_DISPLAY: Add Page Break if needed"))  
  n.pagesz <- n.pagesize * pagesizeadjust - n.outsidebodylines
  n.pagenumber <- 0
  n.linenumber <- 0
  n.row <- 0
  n.addlines <- 0
  n.numofpages <- 1
  s.sharecolvarvalue <- NULL
  n.newrownum <- 0
  d.dsetin.1 <- NULL
  
  print(paste0("RU_DISPLAY: n.pagesz: ", n.pagesz))
  
  if (! (is.null(sharecolvars) && is.null(nowidowvar)) ) {
    for (i in 1:base::nrow(d.dsetin)) {
      n.newrownum <- n.newrownum + 1
      n.row <- n.row + 1
      
      if (i == 1) b.newpage <- TRUE
      else b.newpage <- FALSE
      b.addlines <- FALSE
      
      if (! is.null(sharecolvars)) {
        n.addlines <- 0
        s.sharecolvarvalue <- NULL
        for (j in 1:(length(sharecolvars) - 1)) {
          n.numoflines1 <- stringr::str_count(d.dsetin[i, sharecolvars[j]], "[\n]") + 1
          n.addlines <- n.addlines + n.numoflines1
          # if (i < 20) print(paste0(i, "---", b.addlines, " ---", d.dsetin[i, sharecolvars[j]], "---", d.dsetin[i - 1, sharecolvars[j]]))
          if ( i == 1 || (d.dsetin[i, sharecolvars[j]] != d.dsetin[i - 1, sharecolvars[j]])) {
            b.addlines <- TRUE
            break
          }
        }
      }
      # if (i < 20) print(paste0("i: ", i, "---", "b.addlines: " ,b.addlines))
      n.catlines <- 0
      if ( ! is.null(nowidowvar) && (i==1 || d.dsetin[i, nowidowvar] != d.dsetin[i - 1, nowidowvar])) {
        if (!is.null(sharecolvars) && b.addlines && (ru_index(nowidowvar == columns) >= ru_index(sharecolvars[1] == columns))) {
          n.catlines <- n.addlines 
        } else if (b.addlines) n.catlines <- n.addlines 
        
        for (j in (i+1):nrow(d.dsetin)) {
          if (d.dsetin[i, nowidowvar] == d.dsetin[j, nowidowvar]) {
            n.catlines <- n.catlines + d.dsetin[j, "nlines__"] 
            if (! is.null(s.skipvars)) for (k in 1:length(s.skipvars)) {
              if (s.skipvars[k] != nowidowvar) {
                if ( j < nrow(d.dsetin) && d.dsetin[j, s.skipvars[k]] == d.dsetin[j + 1, s.skipvars[k]]) n.catlines <- n.catlines + 1
              }
            }
          } else break
        }
      }
      
      if (i == 1) b.newpage <- TRUE
      else if ( n.row > 3 &&  ! is.null(nowidowvar) && n.catlines >= n.pagesz - n.linenumber) b.newpage <- TRUE
      else if (b.addlines && (n.addlines + d.dsetin[i, "nlines__"] >= n.pagesz - n.linenumber)) b.newpage <- TRUE
      else if (d.dsetin[i, "nlines__"] >= n.pagesz - n.linenumber) b.newpage <- TRUE
      else if (i > 1) b.newpage <- FALSE
      
      if (! b.newpage && ! is.null(computebeforepagevars )) {
        for (j in 1:length(computebeforepagevars)) {
          if (d.dsetin[i, computebeforepagevars[j]] != d.dsetin[i - 1, computebeforepagevars[j]])  b.newpage <- TRUE
        }
      } 
      
      d.dsetin[i, "newpage__"] <- b.newpage
      d.dsetin[i, "addline__"] <- b.addlines
      d.dsetin[i, "catline__"] <- n.catlines
      
      if (b.newpage) {
        n.pagenumber <- n.pagenumber + 1
        n.linenumber <- 1
        b.addlines <- TRUE
        n.row <- 0
      }  
      
      d.dsetin[i, "linenum__"] <- n.linenumber
      d.dsetin[i, "pagenum__"] <- n.pagenumber
      
      if ((b.addlines || b.newpage) && ! is.null(sharecolvars)) {
        d.dsetin.2 <- d.dsetin[i, ]
        n.index <- ru_index(sharecolvars[length(sharecolvars)] == columns)
        for (j in 1:length(columns)) {
          if (j > n.index) d.dsetin.2[1, columns[j]] <- " "
          if (columns[j] == sharecolvars[length(sharecolvars)]) {
            s.sharecolvarvalue <- NULL
            for (k in 1:(length(sharecolvars) - 1)) {
              if (is.null(s.sharecolvarvalue)) {
                s.sharecolvarvalue <- d.dsetin[i, sharecolvars[k]]
              } else {
                s.sharecolvarvalue <- paste0(s.sharecolvarvalue, "\n", d.dsetin[i, sharecolvars[k]])
              }
            }
            d.dsetin.2[1, sharecolvars[length(sharecolvars)]]  <- s.sharecolvarvalue
          }
        }
        d.dsetin.1 <- base::rbind(d.dsetin.1, d.dsetin.2)
      }
      
      d.dsetin.2 <- d.dsetin[i, ]
      if (is.null(d.dsetin.1)) {
        d.dsetin.1 <- d.dsetin.2
      } else {
        d.dsetin.1 <- base::rbind(d.dsetin.1, d.dsetin.2)
      }
      
      # if ((b.addlines || b.newpage) && ! is.null(sharecolvars)) {
      #   for (j in 1:length(s.columns)) {
      #     if (! s.columns[j] %in% sharecolvars ) {
      #       d.dsetin[i, s.columns[j]] <- paste0(paste0(rep("\n", n.addlines), collapse=""), d.dsetin[i, s.columns[j]])
      #     } else if (s.columns[j] == sharecolvars[length(sharecolvars)]) {
      #       s.sharecolvarvalue <- NULL
      #       for (k in 1:length(sharecolvars)) {
      #         if (is.null(s.sharecolvarvalue)) {
      #           s.sharecolvarvalue <- d.dsetin[i, sharecolvars[k]]
      #         } else {
      #           s.sharecolvarvalue <- paste0(s.sharecolvarvalue, "\n", d.dsetin[i, sharecolvars[k]])
      #         }
      #       }
      #       d.dsetin[i,sharecolvars[j]]  <- s.sharecolvarvalue
      #     }
      #   }
      # }
      
      if (b.addlines) n.linenumber <- n.linenumber + n.addlines + d.dsetin[i, "nlines__"]
      else n.linenumber <- n.linenumber + d.dsetin[i, "nlines__"]
      
      if (! is.null(computebeforepagevars) && b.newpage) n.linenumber <- n.linenumber + length(computebeforepagevars)
      
      if (! is.null(s.skipvars)) for (k in 1:length(s.skipvars)) {
        if ( i < nrow(d.dsetin) && d.dsetin[i, s.skipvars[k]] != d.dsetin[i + 1, s.skipvars[k]]) n.linenumber <- n.linenumber + 1
      } 
      b.newpage <- FALSE
      b.addlines <- FALSE
    }
 
    d.dsetin <- d.dsetin.1
    d.dsetin.1 <- NULL
    # print(s.rptcolumns)
    # print(d.dsetin[c(sharecolvars, "pagenum__", "linenum__", "newpage__", "nlines__", "addline__", "catline__")])

    sharecolvars <- NULL
    s.rptcolumns <- c("pagenum__", s.rptcolumns)
    columns <- c("pagenum__", columns)
    ordervars <- c("pagenum__")
    
    n.numofpages <- n.pagenumber
    noprintvars <- c(noprintvars, "pagenum__")
  }

  print(paste0("RU_DISPLAY: Modify G_DATADATE:", G_DATADATE))
  if (is.null(G_DATADATE) || as.character(G_DATADATE) == "") {s.righttitleline <- c("Page [pg] of [tpg]", " ")}
  else {s.righttitleline <- c("Page [pg] of [tpg]", paste0("Data Date: ", G_DATADATE))}

  print(paste0("RU_DISPLAY: Orientation"))
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
  
  s.outdir <- base::dirname(G_OUTFILE)
  s.outfilename <- base::basename(G_OUTFILE)
  s.outfile <- base::substring(s.outfilename, 1, base::nchar(s.outfilename) - base::nchar(s.ext) - 1)
  s.outfile_rtf <- paste0(s.outdir, "/", s.outfile, ".RTF")
  s.outfiles <- base::list.files(s.outdir, pattern=paste0("^", s.outfile, "\\.(RTF|rtf|L[0-9]+|P[0-9]+|PDF|pdf)$"), full.name=TRUE)

  print(paste0("RU_DISPLAY: Check parameter PROPTIONS=", proptions))
  
  if (is.null(proptions) || is.na(proptions[1]) || (length(proptions) ==1 && proptions[1] == "")) {
    proptions <- "first_row_blank=TRUE, borders = c('top', 'bottom'))"
  } 
  
  s.statements.1 <- NULL

  for (k in 1:n.numofpages) {
    if (is.null(s.statements.1)) s.statements.1 <- paste0("reporter::create_table(d.dsetin.subs.", k, ", show_cols=columns,", proptions, ")")
    else s.statements.1 <- c(s.statements.1, paste0("reporter::create_table(d.dsetin.subs.", k, ", show_cols=columns,", proptions, ")"))
  }
  s.statements.1.1  <- "reporter::create_table(d.dsetin.subs.1, show_cols=columns, borders='all')"
  # print(s.statements.1)
  
  s.statements <- NULL
  
  if (! is.null(computebeforepagevars )) for (j in 1:length(computebeforepagevars)) {
    s.statement <- paste0(" %>% ", "reporter::page_by(", computebeforepagevars[j], ", '", computebeforepagelines[j], "', blank_row = 'none')")
    if (is.null(s.statements)) s.statements <- s.statement
    else s.statements <- paste(s.statements, s.statement)    
  }
  # print(s.statements)
  s.statement <- " %>% reporter::column_defaults(width = .6, style = cell_style(bold = FALSE, indicator = NULL))"
  if (is.null(s.statements)) s.statements <- s.statement
  else s.statements <- paste(s.statements, s.statement)

  print(paste0("RU_DISPLAY: Check parameter SHARECOLVARS=", sharecolvars))
  if ((! is.null(sharecolvars) && ! is.na(sharecolvars[1]) && sharecolvars[1] != "")) {
    for (j in 1:length(sharecolvars)) {
      if (j==1) s.sharecolvars <- paste0("'", sharecolvars[j], "'")
      else s.sharecolvars <- paste0(s.sharecolvars, ",", paste0("'", sharecolvars[j], "'")) 
    }
    s.statements <- paste(s.statements, " %>% reporter::stub(c(", s.sharecolvars, "))")
  }
  
  print(paste0("RU_DISPLAY: Check parameter SPANNINGHEADERS=", spanningheaders))
  
  if  (! (is.null(spanningheaders) || is.na(spanningheaders[1]) || (length(spanningheaders) ==1 && spanningheaders[1] == ""))) {
    for (j in 1:length(spanningheaders)) {
      s.statement <- paste0("%>% reporter::spanning_header(", spanningheaders[j], ")")
      s.statements <- paste(s.statements, s.statement)
    }  
  }

  print(paste0("RU_DISPLAY: n.numofpages", n.numofpages))
  
  n.sharecolvars <- 0
  n.indent <- 0.25
  
  print(paste0("RU_DISPLAY: COLUMNS: ", columns))
  for (i in 1:length(columns)) {
    s.align <- ''
    s.name <-columns[i]
    if (s.name %in% leftvars) s.align <- ", align='left'"
    else if (s.name %in% rightvars) s.align <- ", align='right'"
    else if (s.name %in% centervars) s.align <- ", align='center'"
    
    s.dedup <- ""
    if ((s.name %in% ordervars) && ! (s.name %in% noprintvars)) s.dedup <- ", dedupe = TRUE"
    
    s.skip <- ""
    if (s.name %in% skipvars) s.skip <- ", blank_after = TRUE"
    
    s.page <- ""
    # if (s.name %in% c("pagenum__")) s.page <- ", page_break = TRUE"
    
    s.visible<-"TRUE"
    if (s.name %in% noprintvars) s.visible <- "FALSE"
    
    s.idvar<-"FALSE"
    if (s.name %in% idvars) s.idvar <- "TRUE"
    
    s.share <- ""
    if (s.name %in% sharecolvars) {
      if (n.sharecolvars == 0 ) {
        n.sharecolvars <- n.sharecolvars + 1
        s.share <- ", blank_after = TRUE, label_row = TRUE"
      } else s.share <- paste0(", indent=", n.indent)
    }
    
    s.width <- ""
    if (! (is.null(widths[s.name]) ||  is.na(widths[s.name]))) s.width <- paste0(", width=", widths[s.name])
    
    s.format <- ""
    if (! (is.null(formats[s.name]) || is.na(formats[s.name]))) s.width <- paste0(", format='", formats[s.name], "'")
    
    s.label <- base::lapply(d.dsetin, base::attr, "label")[s.name]

    s.statement <-paste0(" %>% reporter::define(", s.name, s.width, s.format, s.align, s.skip, s.page, s.dedup, s.share, ", label='", s.label, "', visible=", s.visible, ", id_var=", s.idvar, ")")
    s.statements <- paste(s.statements, s.statement)
  }
  
  print(s.statements)
  # vl.tbl <- eval(parse(text=s.statements))
  
  print(paste0("RU_DISPLAY: Delete output before re-create "))

  if (length(s.outfiles) > 0) for (j in 1:length(s.outfiles)) {
    if (base::file.exists(s.outfiles[j])) {
      print(s.outfiles[j])
      base::file.remove(s.outfiles[j])
    }
  }

    print(paste0("RU_DISPLAY: Create output "))
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
        reporter::titles(c(G_TITLE0, G_TITLE1, G_TITLE2, G_TITLE3, G_TITLE4, G_TITLE5, G_TITLE6, G_TITLE7), font_size=G_FONTSIZE) %>%
        reporter::footnotes(c(G_FOOT1, G_FOOT2, G_FOOT3, G_FOOT4, G_FOOT5, G_FOOT6, G_FOOT7, G_FOOT8, G_FOOT9, 
                              paste0(G_USERID, ': ', G_PGMPTH,' ', format(Sys.time(), "%Y-%m-%d %T"))), blank_row="above", columns=1, align="left") %>%
        reporter::set_margins(top = G_TMARGIN, bottom = G_BMARGIN, right=G_RMARGIN, left=G_LMARGIN) 
      # vl.rpt.2 <- reporter::add_content(vl.rpt.2, vl.tbl)
      
      for (k in 1:n.numofpages) {
        if (n.numofpages == 1) {
          s.statements.3 <- paste0("d.dsetin.subs.", k," <- d.dsetin")
        } else {
          s.statements.3 <- paste0("d.dsetin.subs.", k, " <- subset(d.dsetin, pagenum__ == ", k, ")")
        }
        eval(parse(text=s.statements.3))
        s.statements.2 <- paste0(s.statements.1[k], s.statements) 
        vl.tbl <- eval(parse(text=s.statements.2))
        vl.rpt.2 <- reporter::add_content(vl.rpt.2, vl.tbl)
      }
      # Write out report
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
      # for (k in 1:n.numofpages) {
      #   if (n.numofpages == 1) {
      #     s.statements.3 <- paste0("d.dsetin.subs.", k, " <- d.dsetin")
      #   } else {
      #     s.statements.3 <- paste0("d.dsetin.subs.", k, " <- subset(d.dsetin, pagenum__ == ", k, ")")
      #   }
      #   eval(parse(text=s.statements.3))
      #   s.statements.2 <- paste0(s.statements.1[k], s.statements) 
      #   vl.tbl <- eval(parse(text=s.statements.2))
      #   vl.rpt.1 <- reporter::add_content(vl.rpt.1, vl.tbl)
      # }
 
      d.dsetin.subs.1 <- d.dsetin
      s.statements.2 <- paste0(s.statements.1.1, s.statements)
      vl.tbl <- eval(parse(text=s.statements.2))
      vl.rpt.1 <- reporter::add_content(vl.rpt.1, vl.tbl)
      vl.rpt.1 <- reporter::add_style(vl.rpt.1, style = sty)
      #vl.rpt.1 <- reporter::page_footer(vl.rpt.1, paste0(G_USERID, ': ', G_PGMPTH,' ', format(Sys.time(), "%Y-%m-%d %T")), center=NA, right=NA)
      reporter::write_report(vl.rpt.1)
    }
    s.dtype <- "RTF"
  }
}
