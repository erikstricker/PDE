## PDE: Extract Sentences and Tables from PDF Files.
## Copyright (C) 2020-2023  Erik Stricker
## 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' PDE: Extract Tables and Sentences from PDF Files.
#' 
#' The package includes two main components: 1) The PDE analyzer performs the
#' sentence and table extraction while 2) the PDE reader allows the
#' user-friendly visualization and quick-processing of the obtained results.
#'
#' @section PDE functions: \code{\link{PDE_analyzer}}, \code{\link{PDE_analyzer_i}},
#'   \code{\link{PDE_extr_data_from_pdfs}}, \code{\link{PDE_pdfs2table}}, 
#'   \code{\link{PDE_pdfs2table_searchandfilter}},\code{\link{PDE_pdfs2txt_searchandfilter}}, 
#'   \code{\link{PDE_reader_i}}, \code{\link{PDE_install_Xpdftools4.02}},
#'   \code{\link{PDE_check_Xpdf_install}}
#'
#' @docType package
#' @name PDE
NULL
#> NULL

## 1.4.3 github

## declare global variables
PDE.globals <- new.env()
PDE.globals$jumpto.list <- list()
PDE.globals$le.progress.textbox <- list()
PDE.globals$mark.list <- list()
PDE.globals$tables.masterlist <- list()
PDE.globals$ttanalyzer <- list()

#' Deprecated functions in package \sQuote{PDE}
#' 
#' @description  These functions are provided for compatibility with older versions
#' of \sQuote{PDE} only, and will be defunct at the next release.
#'
#' @details  The following functions are deprecated and will be made defunct; use
#' the replacement indicated below:
#'   \itemize{
#'     
#'     \item{PDE_path: \code{system.file(package = "PDE")}}
#'     
#'   }
#'
#' @name PDE-deprecated
NULL
#> NULL

#'Export the installation path the PDE (PDF Data Extractor) package
#'
#'\code{PDE_path} is deprecated. Please run system.file(package = "PDE") instead.
#'
#'@return The function returns a potential path for the PDE package. If the PDE
#'tool was not correctly installed it returns "".
#'
#'@export
PDE_path <- function(){
  .Deprecated("system.file(package = \"PDE\")", package= "PDE",old = "PDE_path")
  ## set PDE library location
  out_path <- ""
  for (dir in .libPaths()){
    if (dir.exists(paste0(dir,"/PDE/R"))){
      out_path <- paste0(dir,"/PDE/")
      break
    }
  }
  return(out_path)
}

#'Check if the Xpdftools are installed an in the system path
#'
#'\code{PDE_check_Xpdf_install} runs a version test for pdftotext, pdftohtml and pdftopng.
#'
#'@param sysname String. In case the function returns "Unknown OS" the sysname can be set manually. 
#' Allowed options are "Windows", "Linux",  "SunOS" for Solaris, and "Darwin" for Mac. Default: \code{NULL}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'
#'@return The function returns a Boolean for the installation status and a message in case 
#' the commands are not detected.
#'
#'@examples
#'
#' PDE_check_Xpdf_install()
#'
#'@export
PDE_check_Xpdf_install <- function(sysname=NULL, verbose=TRUE){
  
  ## receive pdftotext, pdftohtml and pdftopng information from config file if it exists
  
  xpdf_config_location <- paste0(system.file(package = "PDE"),"/bin/XPDF_DIR.config")
  dir.create(dirname(xpdf_config_location), recursive = TRUE, showWarnings = FALSE)
  
  pdftotext_location <- NULL
  pdftohtml_location <- NULL
  pdftopng_location <- NULL
  
  if (file.exists(xpdf_config_location)){
    pdftotext_location <- grep("pdftotext",readLines(xpdf_config_location), value = TRUE)
    if (!length(pdftotext_location) > 0){
      if (file.exists(pdftotext_location) == FALSE) pdftotext_location <- NULL
    } else {
      pdftotext_location <- NULL
    }
    
    pdftohtml_location <- grep("pdftohtml",readLines(xpdf_config_location), value = TRUE)
    if (!length(pdftohtml_location) > 0){
      if (file.exists(pdftohtml_location) == FALSE) pdftohtml_location <- NULL
    } else {
      pdftohtml_location <- NULL
    }
    
    pdftopng_location <- grep("pdftopng",readLines(xpdf_config_location), value = TRUE)
    if (!length(pdftotext_location) > 0){
      if (file.exists(pdftopng_location) == FALSE) pdftopng_location <- NULL
    } else {
      pdftotext_location <- NULL
    }
  } 
  
  # if either the config file does not exist or the xpdf tool files do not exist
  if (!file.exists(xpdf_config_location) ||
      is.null(pdftotext_location) || 
      is.null(pdftohtml_location) || 
      is.null(pdftopng_location)) {
    
    if (is.null(sysname)) {
      sysname <- Sys.info()["sysname"]
    }
    
    show_file_path_linux <- function(filename){
      whereis_output <- system(paste0("whereis -b ",filename), intern = TRUE)
      only_dirs <- sub("^ ","",sub(paste0("^",filename,":"),"",whereis_output))
      if (only_dirs == ""){
        return(NULL)
      } else {
        return(strsplit(gsub(" /",";/",only_dirs),split = ";")[[1]])
      }
    }
    
    show_file_path_solaris <- function(filename){
      whereis_test <- suppressWarnings(tryCatch(system(paste0("/usr/ucb/whereis ",filename),
                                                       , intern = TRUE)[1], 
                                                error=function(err) NULL))
      if (length(whereis_test) != 0) {
        whereis_output <- system(paste0("/usr/ucb/whereis ",filename), intern = TRUE)
        only_dirs <- sub("^ ","",sub(paste0("^",filename,":"),"",whereis_output))
        if (only_dirs == ""){
          return(NULL)
        } else {
          return(strsplit(gsub(" /",";/",only_dirs),split = ";")[[1]])
        }
      } else {
        return(NULL)
      }
    }
    
    if (sysname == "Windows") {
      pdftotext_location <- suppressWarnings(system("C:\\WINDOWS\\system32\\cmd.exe /c where pdftotext", intern = TRUE))
      if (length(attributes(pdftotext_location)$status) > 0) pdftotext_location <- NULL
      
      pdftohtml_location <- suppressWarnings(system("C:\\WINDOWS\\system32\\cmd.exe /c where pdftohtml", intern = TRUE))
      if (length(attributes(pdftohtml_location)$status) > 0) pdftohtml_location <- NULL
      
      pdftopng_location <- suppressWarnings(system("C:\\WINDOWS\\system32\\cmd.exe /c where pdftopng", intern = TRUE))
      if length(attributes(pdftopng_location)$status) > 0) pdftopng_location <- NULL
    } else if (sysname == "Linux") {
      pdftotext_location <- show_file_path_linux("pdftotext")
      
      pdftohtml_location <- show_file_path_linux("pdftohtml")
      
      pdftopng_location <- show_file_path_linux("pdftopng")
      
    } else if (sysname == "SunOS") {
      pdftotext_location <- show_file_path_solaris("pdftotext")
      
      pdftohtml_location <- show_file_path_solaris("pdftohtml")
      
      pdftopng_location <- show_file_path_solaris("pdftopng")
      
    } else if (sysname == "Darwin") {
      
      pdftotext_location <- suppressWarnings(system("which -a pdftotext", intern = TRUE))
      
      pdftohtml_location <- suppressWarnings(system("which -a pdftohtml", intern = TRUE))
      
      pdftopng_location <- suppressWarnings(system("which -a pdftopng", intern = TRUE))
    } else{
      stop("Unknown OS. Please set sysname option.")
    }
  }
  
  out <- TRUE
  files <- NULL
  
  if (length(pdftotext_location) == 0) {
    files <- c(files,"pdftotext")
    out=FALSE 
  }
  if (length(pdftohtml_location) == 0) {
    files <- c(files,"pdftohtml")
    out=FALSE 
  }
  if (length(pdftopng_location) == 0) {
    files <- c(files,"pdftopng")
    out=FALSE 
  }
  
  ## if the command line tools where all detected
  if (out == TRUE) {
    ## test pdftotext version
    pdftotext_path <- NULL
    pdfpath <- paste0(system.file(package = "PDE"),"/examples/Methotrexate/29973177_!.pdf")
    keeplayouttxtpath <- paste0(dirname(pdfpath),"/test_txt/test_keeplayout.txt")
    for (i in 1:length(pdftotext_location)){
      dir.create(dirname(keeplayouttxtpath))
      status <- suppressWarnings(system(paste0("\"", pdftotext_location[i], "\" \"", "-layout",
                                               "\" \"", pdfpath, "\" \"", keeplayouttxtpath,
                                               "\""), 
                                        wait = TRUE, ignore.stderr = TRUE, intern = TRUE))
      if (file.exists(keeplayouttxtpath)) {
        unlink(dirname(keeplayouttxtpath), recursive = TRUE)
        pdftotext_path <- pdftotext_location[i]
        break
      } 
      
      unlink(dirname(keeplayouttxtpath), recursive = TRUE)
    }
    
    ## test pdftohtml version
    pdftohtml_path <- NULL
    pdfpath <- paste0(system.file(package = "PDE"),"/examples/Methotrexate/29973177_!.pdf")
    htmlpath <- paste0(dirname(pdfpath),"/test_html/test.html")
    for (i in 1:length(pdftohtml_location)){
      dir.create(dirname(htmlpath))
      status <- system(paste0("\"",pdftohtml_location[i],"\" \"", pdfpath,
                              "\" \"", htmlpath, "\""), wait = TRUE,
                       ignore.stderr = TRUE, intern = TRUE)
      if (dir.exists(htmlpath) && file.exists(paste0(htmlpath, "/index.html"))) {
        unlink(dirname(htmlpath), recursive = TRUE)
        pdftohtml_path <- pdftohtml_location[i]
        break
      }
      unlink(dirname(htmlpath), recursive = TRUE)
    }
    
    ## test pdftopng
    pdftopng_path <- NULL
    pdfpath <- paste0(system.file(package = "PDE"),"/examples/Methotrexate/29973177_!.pdf")
    pngpath <- paste0(dirname(pdfpath),"/test_png/test.png")
    for (i in 1:length(pdftopng_location)){
      dir.create(dirname(pngpath))
      status <- suppressWarnings(system(paste0("\"",pdftopng_location[i],"\" \"",
                                               "-f", "\" \"", 1, "\" \"", "-l",
                                               "\" \"", 1, "\" \"", pdfpath, "\" \"",
                                               pngpath,"\""),
                                        wait = TRUE, ignore.stderr = TRUE, intern = TRUE))
      if (file.exists(sub("test.png$","test.png-000001.png",pngpath))) {
        unlink(dirname(pngpath), recursive = TRUE)
        pdftopng_path <- pdftopng_location[i]
        break
      }
      unlink(dirname(pngpath), recursive = TRUE)
    }
    
    if (length(pdftotext_path) > 0 && 
        length(pdftohtml_path) > 0 && 
        length(pdftopng_path) > 0) {
      write(paste(pdftotext_path,pdftohtml_path,pdftopng_path, sep = "\n"),
            file = xpdf_config_location)
      attributes(out) <- list(msg = "Correct version of Xpdf command line tools is installed.")
      if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
    } else {
      if (length(pdftotext_path) == 0) {
        files <- c(files,"pdftotext")
        out=FALSE 
      }
      if (length(pdftohtml_path) == 0) {
        files <- c(files,"pdftohtml")
        out=FALSE 
      }
      if (length(pdftopng_path) == 0) {
        files <- c(files,"pdftopng")
        out=FALSE 
      }
      
      msg1 <- paste(" installed. Please install the Xpdf command line tools",
                    "using PDE_install_Xpdftools4.02()")
      
      if (length(files) == 1) {
        out.file <- files
        attributes(out) <- list(msg = paste0("The wrong version of the ",
                                             out.file, " file is",msg1))
        if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
      } else if (length(files) == 2) {
        out.file <- paste0(files[1], " and ", files[2])
        attributes(out) <- list(msg = paste0("The wrong version of the ", 
                                             out.file, " files are",msg1))
        if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
      } else if (length(files) == 3) {
        out.file <- paste0(files[1], ", ", files[2], " and " , files[3])
        attributes(out) <- list(msg = paste0("The wrong version of the ",
                                             out.file, " files are",msg1))
        if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
      } 
      
    }
    
    ## if one or more command line tools where not detected    
  } else {
    msg1 <- paste(" not detected. Please install the Xpdf command line tools again",
                  "using PDE_install_Xpdftools4.02()")
    
    if (length(files) == 1) {
      out.file <- files
      attributes(out) <- list(msg = paste0(out.file, " file",msg1))
      if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
    } else if (length(files) == 2) {
      out.file <- paste0(files[1], " and ", files[2])
      attributes(out) <- list(msg = paste0(out.file, " files",msg1))
      if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
    } else if (length(files) == 3) {
      out.file <- paste0(files[1], ", ", files[2], " and " , files[3])
      attributes(out) <- list(msg = paste0(out.file, " files",msg1))
      if (verbose == TRUE) cat(attributes(out)$msg, sep="\n")
    } 
  } 
  
  return(out)
}

#'Install the Xpdf command line tools 4.02
#'
#'\code{PDE_install_Xpdftools4.02} downloads and installs the XPDF command line tools 4.02.
#'
#'@param sysname String. In case the function returns "Unknown OS" the sysname can be set manually. 
#' Allowed options are "Windows", "Linux", "SunOS" for Solaris, and "Darwin" for Mac. Default: \code{NULL}.
#'@param bin String. In case the function returns "Unknown OS" the bin of the operational system
#' can be set manually. Allowed options are "64", and "32". Default: \code{NULL}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'@param permission Numerical. If set to 0 the user is ask for a permission to
#' download Xpdftools. If set to 1, no user input is required. Default: \code{0}.
#'
#'
#'@return The function returns a Boolean for the installation status and a message in case 
#' the commands are not installed.
#'
#'@examples
#' \dontrun{
#' 
#' PDE_install_Xpdftools4.02()
#' 
#' }
#'
#'
#'@export
PDE_install_Xpdftools4.02 <- function(sysname=NULL, bin=NULL, verbose=TRUE, permission = 0){
  ## check if Xpdftools are installed
  install.test <- PDE_check_Xpdf_install(verbose=FALSE)
  downloadq <- FALSE
  installq <- FALSE
  out_msg <- NULL
  out <- NULL
  
  xpdf_config_location <- paste0(system.file(package = "PDE"),"/bin/XPDF_DIR.config")
  dir.create(dirname(xpdf_config_location), recursive = TRUE, showWarnings = FALSE)
  
  ## set xpdf library location
  xpdf_bin_path <- paste0(system.file(package = "PDE"),"/bin")
  if (is.null(sysname)) sysname <- Sys.info()["sysname"]
  
  download.test <- FALSE
  if (sysname == "Windows") {
    if (dir.exists(paste0(xpdf_bin_path,"/xpdf-tools-win-4.02"))) download.test <- TRUE
  } else if (sysname == "Linux" || sysname == "SunOS") {
    if (dir.exists(paste0(xpdf_bin_path,"/xpdf-tools-linux-4.02"))) download.test <- TRUE
  } else if (sysname == "Darwin") {
    if (dir.exists(paste0(xpdf_bin_path,"/xpdf-tools-mac-4.02"))) download.test <- TRUE
  } else {
    stop("Unknown OS. Please set sysname option.")
  }
  
  if (is.null(bin)){
    if (grepl("32",Sys.info()[["machine"]])) {
      bin <- "32"
    } else if (grepl("64",Sys.info()[["machine"]])) {
      bin <- "64"
    } else {
      stop("Unknown OS. Please set sysname option.")
    }
  }
  
  if (bin != "64" && bin != "32"){
    stop("Unknown OS. Please set bin option.")
  }
  
  ## determine operating system and download correct xpdf
  if (permission == 0){
    if (download.test == FALSE){
      ## determine operating system and download correct Xpdf command line tools
      downloadq <- utils::menu(c("Y", "N"), title="Do you want to download and install xpdf version 4.02? (y/n)") == 1
      installq <- downloadq
    } else {
      downloadq <- utils::menu(c("Y", "N"), title=paste("Xpdf command line tools 4.02 are already downloaded.",
                                                        "Do you want to download the Xpdf command line tools version 4.02 again? (y/n)")) == 1
      if (install.test == TRUE){
        installq <- utils::menu(c("Y", "N"), title=paste("Working versions of Xpdf command line tools are already installed.",
                                                         "Do you want to still (re)install",
                                                         "the Xpdf command line tools version 4.02? (y/n)")) == 1
      } else {
        installq <- utils::menu(c("Y", "N"), title=paste("Do you want to also install",
                                                         "the Xpdf command line tools version 4.02? (y/n)")) == 1
      }
    }
  } else {
    downloadq <- TRUE
    installq <- TRUE
  }
  
  if (downloadq == TRUE){
    
    if (sysname == "Windows") {
      utils::download.file("https://raw.githubusercontent.com/erikstricker/PDE/master/inst/examples/bin/xpdf-tools-win-4.02.zip", 
                           destfile = paste0(xpdf_bin_path,"/xpdf-tools-win-4.02.zip"),
                           mode = "wb")
      utils::unzip(paste0(xpdf_bin_path,"/xpdf-tools-win-4.02.zip"),exdir = xpdf_bin_path)
      remove.status <- suppressWarnings(file.remove(paste0(xpdf_bin_path,"/xpdf-tools-win-4.02.zip")))
      download.test <- TRUE
    } else if (sysname == "Linux" || sysname == "SunOS") {
      utils::download.file("https://raw.githubusercontent.com/erikstricker/PDE/master/inst/examples/bin/xpdf-tools-linux-4.02.tar.gz", 
                           destfile = paste0(xpdf_bin_path,"/xpdf-tools-linux-4.02.tar.gz"),
                           mode = "wb")
      utils::untar(paste0(xpdf_bin_path,"/xpdf-tools-linux-4.02.tar.gz"),exdir = xpdf_bin_path)
      remove.status <- suppressWarnings(file.remove(paste0(xpdf_bin_path,"/xpdf-tools-linux-4.02.tar.gz")))
      download.test <- TRUE
    } else if (sysname == "Darwin") {
      utils::download.file("https://raw.githubusercontent.com/erikstricker/PDE/master/inst/examples/bin/xpdf-tools-mac-4.02.tar.gz", 
                           destfile = paste0(xpdf_bin_path,"/xpdf-tools-mac-4.02.tar.gz"))
      utils::untar(paste0(xpdf_bin_path,"/xpdf-tools-mac-4.02.tar.gz"),exdir = xpdf_bin_path)
      remove.status <- suppressWarnings(file.remove(paste0(xpdf_bin_path,"/xpdf-tools-mac-4.02.tar.gz")))
      download.test <- TRUE
    } else {
      stop("Unknown OS. Please set sysname option.")
    }
  }
  
  if (download.test == TRUE){
    if (sysname == "Windows") {
      filepath <- normalizePath(paste0(xpdf_bin_path,"/xpdf-tools-win-4.02/bin",bin))
      ext <- ".exe"
    } else if (sysname == "Linux" || sysname == "SunOS") {
      filepath <- normalizePath(paste0(xpdf_bin_path,"/xpdf-tools-linux-4.02/bin",bin))
      ext <- ""
    } else if (sysname == "Darwin") {
      filepath <- normalizePath(paste0(xpdf_bin_path,"/xpdf-tools-mac-4.02/bin",bin))
      ext <- ""
    } else {
      stop("Unknown OS. Please set sysname option.")
    }
    out_msg <- c(out_msg,paste0("Location of Xpdf command line tools 4.02: ",filepath))
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
    attributes(out) <- list(msg = out_msg,path=filepath)
    
    ## "Installation"
    if (installq == TRUE){
      
      pdftotext_path <- normalizePath(paste0(filepath,"/pdftotext",ext))
      
      pdftohtml_path <- normalizePath(paste0(filepath,"/pdftohtml",ext))
      
      pdftopng_path <- normalizePath(paste0(filepath,"/pdftopng",ext))
      
      write(paste(pdftotext_path,pdftohtml_path,pdftopng_path, sep = "\n"),
            file = xpdf_config_location)
      
      out_msg <- c(out_msg,"The Xpdf command line tools 4.02 were successfully installed.")
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      attributes(out) <- list(msg = out_msg,path=filepath)
      out <- TRUE
    } else {
      out_msg <- c(out_msg,"The Xpdf command line tools 4.02 were not installed.")
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      attributes(out) <- list(msg = out_msg,path=filepath)
      out <- FALSE
    }
  } else {
    out_msg <- c(out_msg,"The Xpdf command line tools 4.02 were not downloaded.")
    attributes(out) <- list(msg = out_msg,path="")
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
    out <- FALSE
  }
  
  return(out)
}

#'Extracting data from a PDF (Protable Document Format) file
#'
#'\code{PDE_extr_data_from_pdf} extracts sentences or tables from a single PDF
#'file and writes output in the corresponding folder.
#'
#'@param pdf String. Path to the PDF file to be analyzed.
#'@param whattoextr String. Either \emph{txt}, \emph{tab}, or \emph{tabandtxt}
#'  for PDFS2TXT (extract sentences from a PDF file) or PDFS2TABLE (table of a PDF
#'  file to a Microsoft Excel file) extraction. \emph{tab} allows the extraction
#'  of tables with and without search words while \emph{txt} and \emph{tabandtxt}
#'  require search words.
#'@param out String. Directory chosen to save analysis results in. Default:
#'  \code{"."}.
#'@param filter.words List of strings. The list of filter words. If not
#'  \code{NA} or \code{""} a hit will be counted every time a word from the list
#'  is detected in the article.
#'   Default: \code{""}.
#'@param regex.fw Logical. If TRUE filter words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.fw Logical. Are the filter words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param filter.word.times Numeric or string. Can either be expressed as absolute number or percentage 
#'  of the total number of words (by adding the "%" sign). The minimum number of hits described for
#'  \code{filter.words} for a paper to be further analyzed. Default: \code{0.2\%}.
#'@param table.heading.words List of strings. Different than standard (TABLE,
#'  TAB or table plus number) headings to be detected. Regex rules apply (see
#'  also
#'  \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'   Default = \code{""}.
#'@param ignore.case.th Logical. Are the additional table headings (see
#'  \code{table.heading.words}) case-sensitive (does capitalization matter)?
#'  Default = \code{FALSE}.
#'@param search.words List of strings. List of search words. To extract all
#'  tables from the PDF file leave \code{search.words = ""}. 
#'@param search.word.categories List of strings. List of categories with the 
#'  same length as the list of search words. Accordingly, each search word can be 
#'  assigned to a category, of which the word counts will be summarized in the
#'  \code{PDE_analyzer_word_stats.csv} file. If search.word.categories is a
#'  different length than search.words the parameter will be ignored.
#'  Default: \code{NULL}.
#'@param regex.sw Logical. If TRUE search words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.sw Logical. Are the search words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param eval.abbrevs Logical. Should abbreviations for the search words be
#'  automatically detected and then replaced with the search word + "$*"?
#'  Default: \code{TRUE}.
#'@param out.table.format String. Output file format. Either comma separated
#'  file \code{.csv} or tab separated file \code{.tsv}. The encoding indicated
#'  in parantheses should be selected according to the operational system 
#'  exported tables are opened in, i.e., Windows: \code{"(WINDOWS-1252)"}; Mac: 
#'  \code{(macintosh)}; Linux: \code{(UTF-8)}. Default: \code{".csv"} and 
#'  encoding depending on the operational system.
#'@param dev_x Numeric. For a table the size of indention which would be
#'  considered the same column. Default: \code{20}.
#'@param dev_y Numeric. For a table the vertical distance which would be
#'  considered the same row. Can be either a number or set to dynamic detection 
#'  [9999], in which case the font size is used to detect which words are in the 
#'  same row. 
#'  Default: \code{9999}.
#'@param context Numeric. Number of sentences extracted before and after the
#'  sentence with the detected search word. If \code{0} only the sentence with
#'  the search word is extracted. Default: \code{0}.
#'@param write.table.locations Logical. If \code{TRUE}, a separate file with the
#'  headings of all tables, their relative location in the generated html and
#'  txt files, as well as information if search words were found will be
#'  generated. Default: \code{FALSE}.
#'@param exp.nondetc.tabs Logical. If \code{TRUE}, if a table was detected in a
#'  PDF file but is an image or cannot be read, the page with the table with be
#'  exported as a png. Default: \code{TRUE}.
#'@param write.tab.doc.file Logical. If \code{TRUE}, if search words are used
#'  for table detection and no search words were found in the tables of a PDF 
#'  file, a \strong{no.table.w.search.words}. Default: \code{TRUE}.
#'@param write.txt.doc.file Logical. If \code{TRUE}, if no search words were
#'  found in the sentences of a PDF file, a file will be created with the PDF
#'  filename followed by \strong{no.txt.w.search.words}. If the PDF file is
#'  empty, a file will be created with the PDF filename followed by
#'  \strong{no.content.detected}. If the filter word threshold is not met, 
#'  a file will be created with the PDF filename followed by 
#'  \strong{no.txt.w.filter.words}. Default: \code{TRUE}.
#'@param delete Logical. If \code{TRUE}, the intermediate \strong{txt},
#'  \strong{keeplayouttxt} and \strong{html} copies of the PDF file will be 
#'  deleted. Default: \code{TRUE}.
#'@param cpy_mv String. Either "nocpymv", "cpy", or "mv". If filter words are used in the
#'  analyses, the processed PDF files will either be copied ("cpy") or moved ("mv") into the
#'  /pdf/ subfolder of the output folder. Default: \code{"nocpymv"}.
#'@param verbose Logical. Indicates whether messages will be printed in the 
#'  console. Default: \code{TRUE}.
#'
#'@return If tables were extracted from the PDF file the function returns a list of
#'  following tables/items: 1) \strong{htmltablelines}, 2)
#'  \strong{txttablelines}, 3) \strong{keeplayouttxttablelines}, 4) \strong{id},
#'  5) \strong{out_msg}.
#'  The \strong{tablelines} are tables that provide the heading and position of
#'  the detected tables. The \strong{id} provide the name of the PDF file. The
#'  \strong{out_msg} includes all messages printed to the console or the suppressed
#'  messages if \code{verbose=FALSE}.
#'
#'@examples
#'
#'## Running a simple analysis with filter and search words to extract sentences and tables
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- .PDE_extr_data_from_pdf(pdf = "/examples/Methotrexate/29973177_!.pdf",
#'  whattoextr = "tabandtxt",
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_output+-0_test/"),
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  ignore.case.fw = TRUE,
#'  regex.fw = FALSE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  ignore.case.sw = FALSE,
#'  regex.sw = TRUE)
#'}
#'
#'## Running an advanced analysis with filter and search words to
#'## extract sentences and tables and obtain documentation files
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- .PDE_extr_data_from_pdf(pdf = paste0(system.file(package = "PDE"),
#'                        "/examples/Methotrexate/29973177_!.pdf"),
#'  whattoextr = "tabandtxt",
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_output+-1_test/"),
#'  context = 1,
#'  dev_x = 20,
#'  dev_y = 9999,
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  ignore.case.fw = TRUE,
#'  regex.fw = FALSE,
#'  filter.word.times = "0.2%",
#'  table.heading.words = "",
#'  ignore.case.th = FALSE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  ignore.case.sw = FALSE,
#'  regex.sw = TRUE,
#'  eval.abbrevs = TRUE,
#'  out.table.format = ".csv (WINDOWS-1252)",
#'  write.table.locations = TRUE,
#'  write.tab.doc.file = TRUE,
#'  write.txt.doc.file = TRUE,
#'  exp.nondetc.tabs = TRUE,
#'  cpy_mv = "nocpymv",
#'  delete = TRUE)
#'}
#'
#'@seealso
#'\code{\link{PDE_pdfs2table}},\code{\link{PDE_pdfs2table_searchandfilter}},
#'\code{\link{PDE_pdfs2txt_searchandfilter}}
#'
#'@export
.PDE_extr_data_from_pdf <- function(pdf, whattoextr,
                                    out = ".", filter.words = "", regex.fw = TRUE, ignore.case.fw = FALSE, filter.word.times = "0.2%",
                                    table.heading.words = "", ignore.case.th = FALSE, search.words, search.word.categories = NULL, regex.sw = TRUE,
                                    ignore.case.sw = FALSE, eval.abbrevs = TRUE, out.table.format = ".csv (WINDOWS-1252)", 
                                    dev_x = 20, dev_y = 9999, context = 0,write.table.locations = FALSE, exp.nondetc.tabs = TRUE, 
                                    write.tab.doc.file = TRUE,write.txt.doc.file = TRUE, delete = TRUE, cpy_mv = "nocpymv",
                                    verbose = TRUE){
  
  ## General functions -------------------------------------------
  ## Added re.escape function in v1.4.0 to convert regex to non-regex
  re.escape <- function(string){
    vals <- c("\\\\", "\\[", "\\]", "\\(", "\\)", 
              "\\{", "\\}", "\\^", "\\$","\\*", 
              "\\+", "\\?", "\\.", "\\|")
    replace.vals <- paste0("\\\\", vals)
    for(i in seq_along(vals)){
      string <- gsub(vals[i], replace.vals[i], string)
    }
    return(string)
  }
  
  create_id_from_stringlist <- function(list_of_strings, char_len = 8){
    list_of_ints <- NULL
    for (string in list_of_strings){
      final <- 20
      numbers <- utf8ToInt(string) - utf8ToInt("a") + 1L
      for (i in 2:length(numbers)){
        final <- abs(round(mean(c(final, (numbers[[i-1]] %% numbers[[i]] + (numbers[[i]] * 10000))))))
      } 
      list_of_ints <- c(list_of_ints, final)
    }
    set.seed(round(mean(list_of_ints)))
    n <- 1
    pool <- c(letters, LETTERS, 0:9)
    
    res <- character(n) # pre-allocating vector is much faster than growing it
    for(i in seq(n)){
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      while(this_res %in% res){ # if there was a duplicate, redo
        this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      }
      res[i] <- this_res
    }
    return(res)
    
  }
  
  readin_txt <- function(txtpath) {
    
    ## read in the txt file
    txtcontent_from_txtpath_lat1 <- readLines(txtpath, warn = FALSE, encoding = "latin1")
    txtcontent_from_txtpath_utf8 <- readLines(txtpath, warn = FALSE, encoding = "UTF-8")
    res_lat1 <- try(sum(nchar(txtcontent_from_txtpath_lat1)),silent = TRUE)
    res_utf8 <- try(sum(nchar(txtcontent_from_txtpath_utf8)),silent = TRUE)    
    if (inherits(res_utf8,"try-error")){
      ## if utf8 throws an error
      txtcontent_from_txtpath <- txtcontent_from_txtpath_lat1
    } else if (inherits(res_lat1,"try-error")){
      ## if latin1 throws an error
      txtcontent_from_txtpath <- txtcontent_from_txtpath_utf8
    } else {
      ## latin1 preferred for 4.2.0 xpdf version
      txtcontent_from_txtpath <- txtcontent_from_txtpath_lat1
    }
    
    
    for (r in 1:length(txtcontent_from_txtpath)){
      ## replace all fi
      res <- try(gsub(intToUtf8(0xFB01),"fi",txtcontent_from_txtpath[r], fixed = TRUE),silent = TRUE)
      if (inherits(res,"try-error")){
        txtcontent_from_txtpath[r] <- iconv(txtcontent_from_txtpath[r], 'UTF-8', 'latin1', 'bit')
      }
      txtcontent_from_txtpath[r] <- gsub(intToUtf8(0xFB01),"fi",txtcontent_from_txtpath[r], fixed = TRUE)
    }
    return(txtcontent_from_txtpath)
    
  }
  
  
  page_splits <- function(txtcontent_to_split) {
    
    ## split the content according to pages
    pagesplits <- grep("^\\f", txtcontent_to_split)
    
    ## if there is only one page or no txtcontent_to_split
    if (length(pagesplits) > 1) {
      ## page split the line before
      g <- rep.int(1, pagesplits[1] - 1)
      for (p in 2:length(pagesplits)) {
        g <- c(g, rep.int(p, pagesplits[p] - pagesplits[p - 1]))
      }
      g <- c(g, length(pagesplits))
      splittxtcontent <- split(txtcontent_to_split, g)
    } else {
      splittxtcontent <- txtcontent_to_split
    }
    
    return(splittxtcontent)
  }
  
  find_similar_row <- function(originrow, targettablelines,
                               relative.match.col, output.for.originrow.only,
                               output.for.match, output.column.name) {
    
    determine_similarity_1_vs_2 <- function(strings){
      ## determine similarity
      matches_fsr <- NULL
      for (pos in 1:length(strings[[1]])) {
        counter_fsr <- 0
        out_fsr <- FALSE
        while (out_fsr == FALSE && counter_fsr < 4) {
          ## test is characters are the same
          if (is.na(strings[[2]][pos + counter_fsr])) break
          if (strings[[1]][pos] == strings[[2]][pos + counter_fsr]) {
            out_fsr <- TRUE
          } else {
            counter_fsr <- counter_fsr + 1
          }
        }  ## end while
        matches_fsr <- c(matches_fsr, out_fsr)
      }
      ## determine how much it matches_fsr
      percent.match <- sum(matches_fsr, na.rm = TRUE)/length(strings[[1]])
      return(percent.match)
    }
    
    ## set variables
    matchingrow <- NA
    matchpercent <- NULL
    targettablerow <- NULL
    
    ## check every row in targettablelines
    for (targetrow in 1:nrow(targettablelines)) {
      ## skip lines that do not have matching pages
      if (as.numeric(targettablelines[targetrow, "page"]) != as.numeric(originrow[["page"]])) next
      x <- as.character(originrow[[relative.match.col]])
      y <- as.character(targettablelines[targetrow, relative.match.col])
      if (nchar(x) > nchar(y)) x <- substr(x, 1, nchar(y))
      ## Run one way around
      strings1 <- sapply(list(x, y), strsplit, "")
      strings2 <- sapply(list(y, x), strsplit, "")
      percent.match1 <- determine_similarity_1_vs_2(strings1)
      percent.match2 <- determine_similarity_1_vs_2(strings2)
      percent.match <- max(percent.match1,percent.match2)
      
      if (percent.match > 0.8) {
        matchpercent <- rbind(matchpercent, c(originrow = 1,
                                              targetrow = targetrow,
                                              percent.match = percent.match))
      }
    }
    
    ## if multiple tables were on the same page if no
    ## matching table is found
    if (is.null(matchpercent)) {
      originrow[output.column.name] <- output.for.originrow.only
      targettablerow <- NA
      ## if only one row matches_fsr
    } else if (nrow(matchpercent) == 1) {
      originrow[output.column.name] <- output.for.match
      targettablelines[matchpercent[1, "targetrow"],
                       output.column.name] <- output.for.match
      targettablerow <- targettablelines[matchpercent[1, "targetrow"],]
      ## if multiple rows
    } else {
      maxrow <- grep(TRUE, matchpercent[, "percent.match"] %in% max(matchpercent[, "percent.match"]))[1]
      originrow[output.column.name] <- output.for.match
      targettablelines[matchpercent[maxrow, "targetrow"], output.column.name] <- output.for.match
      targettablerow <- targettablelines[matchpercent[maxrow, "targetrow"],]
    }
    
    return(list(originrow = originrow, targettablelines = targettablelines,
                targettablerow = targettablerow))
  }
  
  exp_nondetc_tabs <- function(input_table, pdfpath,
                               outputpath, detectinfile,
                               based.on.column, matches_end) {
    
    ## find all rows that match
    matched.rows <- grep(matches_end, input_table[, based.on.column])
    
    ## find page from input_table
    pages <- input_table[matched.rows, "page"]
    
    exp.pages <- unique(pages)
    
    ## only start function if there are pages to export
    if (length(exp.pages) > 0) {
      dir.create(paste0(outputpath,"/tables"), showWarnings = FALSE)
      for (page in pages) {
        system(paste0("\"",pdftopng_location,"\" \"",
                      "-f", "\" \"", page, "\" \"", "-l",
                      "\" \"", page, "\" \"", pdfpath, "\" \"",
                      outputpath, "/tables/", substr(basename(pdfpath),
                                                     1, regexpr(".pdf", basename(pdfpath)) -
                                                       1), "_page", page, "_w.table",
                      "\""), wait = TRUE, ignore.stderr = TRUE)
      }
    }
  }
  
  test_if_abbrev_in_parantheses <- function(searchword, paragraph,ignore.case.sw) {
    output <- list(res = TRUE,"","","","")
    ## get the position of the search word in the paragraph
    pos_searchword_start <- regexpr(searchword,paragraph,ignore.case = ignore.case.sw)[[1]]
    pos_searchword_end <- pos_searchword_start + attr(regexpr(searchword,paragraph,
                                                              ignore.case = ignore.case.sw),"match.length") - 1
    simple_searchword <- substr(paragraph, pos_searchword_start, pos_searchword_end)
    ## test if parantheses are after it +2 for \\
    pos_open <- regexpr("\\(",substr(paragraph, pos_searchword_end + 1, nchar(paragraph)))[[1]]
    pos_abbrev_start <- pos_open + 1 + pos_searchword_end
    pos_close <- regexpr("\\)",substr(paragraph, pos_abbrev_start + 1, nchar(paragraph)))[[1]]
    pos_abbrev_end <- pos_close + pos_abbrev_start - 1
    sentence.end <-  regexpr("\\. [0-9A-Z]",substr(paragraph, pos_searchword_end + 1, nchar(paragraph)))[[1]] +
      pos_searchword_end - 1
    if (sentence.end == -1) sentence.end <- 999999
    ## if both paranthesis were found and there is no end of a sentence
    if (pos_open > 0 &&
        pos_close > 0 &&
        sentence.end > pos_abbrev_end &&
        pos_searchword_start > 0 &&
        pos_searchword_end > 0) {
      pos_ext_searchword_end <- pos_abbrev_start - 3
      ext_searchword <- substr(paragraph, pos_searchword_start, pos_ext_searchword_end)
      ## get all letters within parantheses
      current_char_pos <- pos_abbrev_start
      current_char <- substr(paragraph, current_char_pos, current_char_pos)
      char_list <- NULL
      while (!grepl(" |\\)", current_char)){
        char_list <- c(char_list, current_char)
        current_char_pos <- current_char_pos + 1
        current_char <- substr(paragraph, current_char_pos, current_char_pos)
      }
      
      char_list <- char_list[!(char_list %in% c("(", ")","[", "]", "/",
                                                "{","}","\\"))]
      
      ext_searchword <- gsub("[^[:alnum:] ]","",ext_searchword)
      
      ## test if letter description was found
      if (length(char_list) > 0){
        trunc_searchword <- ext_searchword
        ## match the letters to letters in the searchword
        for (n in 1:length(char_list)){
          pos_char <- regexpr(char_list[n], trunc_searchword, ignore.case = TRUE)[[1]]
          removed.chars <- substr(trunc_searchword, 1, pos_char)
          ## if character was found in search word and abbreviation and
          ## there were no words without character in searchword removed
          if (pos_char > 0 && !length(gregexpr(" ",removed.chars)[[1]]) > 1) {
            trunc_searchword <- substr(trunc_searchword, pos_char + 1, nchar(trunc_searchword))
          } else {
            output <- list(res = FALSE,"","","","")
            break
          }
        }
        
        ## test if each word in extended searchword has letter in abbreviation
        list.of.words <- gsub("[^A-z]","",
                              strsplit(ext_searchword," ")[[1]])[!(gsub("[^A-z]","",
                                                                        strsplit(ext_searchword," ")[[1]]) %in% "")]
        ## allow one slack
        if (length(list.of.words) - 1 <= length(char_list)){
          result <- TRUE
          trunc_abbrev <- char_list
          ## match the letters to letters in the searchword
          for (n in 1:length(list.of.words)){
            if (length(trunc_abbrev) == 0 && result == TRUE){
              ## one slack
              result <- FALSE
              next
            } else {
              output <- list(res = FALSE,"","","","")
              break
            }
            pos_char <- regexpr(substr(list.of.words[n],1,1), trunc_abbrev, ignore.case = TRUE)[[1]]
            removed.chars <- trunc_abbrev[pos_char]
            ## if character was found in search word and abbreviation and
            ## there were no words without character in searchword removed
            if (pos_char > 0 && !nchar(removed.chars) > 1) {
              trunc_abbrev <- trunc_abbrev[-pos_char]
            } else if (result == TRUE){
              ## one slack
              result <- FALSE
            } else {
              output <- list(res = FALSE,"","","","")
              break
            }
          }
        } else {
          output <- list(res = FALSE,"","","","")
        }
        
        ## test if letters did match search word
        if (output[[1]]) {
          ## for plural search words
          if (char_list[length(char_list)] == "s" &&
              substr(ext_searchword, nchar(ext_searchword),nchar(ext_searchword)) == "s"){
            abbrev_plural <- paste(char_list, collapse = "")
            replacement_plural <- paste0(abbrev_plural," (",ext_searchword,")$*")
            abbrev_singular <- paste(char_list[-length(char_list)], collapse = "")
            simple_searchword <- substr(paragraph, pos_searchword_start, pos_searchword_end)
            replacement_singular <- paste0(abbrev_singular," (",simple_searchword,")$*")
          } else {
            abbrev_singular <- paste(char_list, collapse = "")
            replacement_singular <- paste0(abbrev_singular," (",ext_searchword,")$*")
            abbrev_plural <- paste(c(char_list,"s"), collapse = "")
            replacement_plural <- paste0(abbrev_plural," (",ext_searchword,"s)$*")
          }
          output <- list(res = TRUE,abbrev_singular = abbrev_singular,
                         replacement_singular = replacement_singular,
                         abbrev_plural = abbrev_plural,replacement_plural = replacement_plural)
        }
      } else {
        output <- list(res = FALSE,"","","","")
      }
    } else {
      ## abbrev found, abbrev_singular, replacement_singular, abbrev_plural, replacement_plural
      output <- list(res = FALSE,"","","","")
    }
    return(output)
  }
  
  test_if_abbrev_double_dots_or_equal <- function(searchword, paragraph, ignore.case.sw) {
    output <- list(res = TRUE,"","","","")
    ## get the position of the search word in the paragraph
    pos_searchword_start <- regexpr(searchword,paragraph, ignore.case = ignore.case.sw)[[1]]
    pos_searchword_end <- pos_searchword_start + attr(regexpr(searchword,paragraph, 
                                                              ignore.case = ignore.case.sw),
                                                      "match.length") - 1
    ## test if : or = is before it
    minus_three_chars <- substr(paragraph, (pos_searchword_start - 3),(pos_searchword_start - 1))
    if (grepl("(:|=)", minus_three_chars)) {
      pos_ext_searchword_end <- (regexpr("[^[:alnum:] ]", 
                                         substr(paragraph, pos_searchword_end + 1, 
                                                pos_searchword_end + 3)))[[1]] +
        pos_searchword_end - 1
      ext_searchword <- substr(paragraph, pos_searchword_start, pos_ext_searchword_end)
      ## get all letters before the : or =
      current_char_pos <- pos_searchword_start - 2 - lengths(regmatches(minus_three_chars, 
                                                                        gregexpr(" ", minus_three_chars)))
      current_char <- substr(paragraph, current_char_pos, current_char_pos)
      char_list <- NULL
      while (grepl("[A-z|0-9]", current_char)){
        char_list <- c(current_char, char_list)
        current_char_pos <- current_char_pos - 1
        current_char <- substr(paragraph, current_char_pos, current_char_pos)
      }
      
      char_list <- char_list[!(char_list %in% c("(", ")","[", "]", "/",
                                                "{","}","\\"))]
      
      ## test if letter description was found
      if (length(char_list) > 0){
        trunc_searchword <- gsub("[^[:alnum:] ]","",ext_searchword)
        ## match the letters to letters in the searchword
        for (n in 1:length(char_list)){
          pos_char <- regexpr(char_list[n], trunc_searchword, ignore.case = TRUE)[[1]]
          removed.chars <- substr(trunc_searchword, 1, pos_char)
          ## if character was found in search word and abbreviation and
          ## there were no words without character in searchword removed
          if (pos_char > 0 && !length(gregexpr(" ",removed.chars)[[1]]) > 1) {
            trunc_searchword <- substr(trunc_searchword, pos_char + 1, nchar(trunc_searchword))
          } else {
            output <- list(res = FALSE,"","","","")
            break
          }
        }
        
        ## test if each word in extended searchword has letter in abbreviation
        list.of.words <- gsub("[^A-z]","",
                              strsplit(ext_searchword," ")[[1]])[!(gsub("[^A-z]","",
                                                                        strsplit(ext_searchword,
                                                                                 " ")[[1]]) %in% "")]
        
        ## allow one slack
        if (length(list.of.words) - 1 <= length(char_list)){
          result <- TRUE
          trunc_abbrev <- char_list
          ## match the letters to letters in the searchword
          for (n in 1:length(list.of.words)){
            pos_char <- regexpr(substr(list.of.words[n],1,1), trunc_abbrev, ignore.case = TRUE)[[1]]
            removed.chars <- trunc_abbrev[pos_char]
            ## if character was found in search word and abbreviation and
            ## there were no words without character in searchword removed
            if (pos_char > 0 && !nchar(removed.chars) > 1) {
              trunc_abbrev <- trunc_abbrev[-pos_char]
            } else if (result == TRUE){
              ## one slack
              result <- FALSE
            } else {
              output <- list(res = FALSE,"","","","")
              break
            }
          }
        } else {
          output <- list(res = FALSE,"","","","")
        }
        
        ## test if letters did match search word
        if (output[[1]]) {
          ## for plural search words
          if (char_list[length(char_list)] == "s" &&
              substr(ext_searchword, nchar(ext_searchword),nchar(ext_searchword)) == "s"){
            abbrev_plural <- paste(char_list, collapse = "")
            replacement_plural <- paste0(abbrev_plural," (",ext_searchword,")$*")
            abbrev_singular <- paste(char_list[-length(char_list)], collapse = "")
            simple_searchword <- substr(paragraph, pos_searchword_start, pos_searchword_end)
            replacement_singular <- paste0(abbrev_singular," (",simple_searchword,")$*")
          } else {
            abbrev_singular <- paste(char_list, collapse = "")
            replacement_singular <- paste0(abbrev_singular," (",ext_searchword,")$*")
            abbrev_plural <- paste(c(char_list,"s"), collapse = "")
            replacement_plural <- paste0(abbrev_plural," (",ext_searchword,"s)$*")
          }
          output <- list(res = TRUE,abbrev_singular = abbrev_singular,
                         replacement_singular = replacement_singular,
                         abbrev_plural = abbrev_plural,replacement_plural = replacement_plural)
        }
      } else {
        output <- list(res = FALSE,"","","","")
      }
    } else {
      ## abbrev found, abbrev_singular, replacement_singular, abbrev_plural, replacement_plural
      output <- list(res = FALSE,"","","","")
    }
    return(output)
  }
  
  deletefile <- function(verbose=TRUE) {
    out_msg <- NULL
    
    if (delete == TRUE) {
      ## clean up
      unlink(txtpath, recursive = TRUE)
      if (exists(txtpath)) {
        out_msg <- c(out_msg, paste0("Could not delete:", txtpath))
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      }
      unlink(keeplayouttxtpath, recursive = TRUE)
      if (exists(keeplayouttxtpath)) {
        out_msg <- c(out_msg, paste0("Could not delete:", keeplayouttxtpath))
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      }
      unlink(htmlpath, recursive = TRUE)
      if (exists(htmlpath)){ 
        if (exists(keeplayouttxtpath)) {
          out_msg <- c(out_msg, paste0("Could not delete:", htmlpath))
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        }
      }
    }
    return(out_msg)
  }
  
  replace.html.entity <- function(input.with.html) {
    output.without.html <- input.with.html
    output.without.html <- gsub("&amp;","&",output.without.html)
    output.without.html <- gsub("&lt;","<",output.without.html)
    output.without.html <- gsub("&gt;",">",output.without.html)
    output.without.html <- gsub("&#160;"," ",output.without.html)
    output.without.html <- gsub("&Agrave;",intToUtf8(0x00C0),output.without.html)
    output.without.html <- gsub("&Aacute;",intToUtf8(0x00C1),output.without.html)
    output.without.html <- gsub("&Acirc;",intToUtf8(0x00C2),output.without.html)
    output.without.html <- gsub("&Atilde;",intToUtf8(0x00C3),output.without.html)
    output.without.html <- gsub("&Auml;",intToUtf8(0x00C4),output.without.html)
    output.without.html <- gsub("&Aring;",intToUtf8(0x00C5),output.without.html)
    output.without.html <- gsub("&AElig;",intToUtf8(0x00C6),output.without.html)
    output.without.html <- gsub("&Ccedil;",intToUtf8(0x00C7),output.without.html)
    output.without.html <- gsub("&Egrave;",intToUtf8(0x00C8),output.without.html)
    output.without.html <- gsub("&Eacute;",intToUtf8(0x00C9),output.without.html)
    output.without.html <- gsub("&Ecirc;",intToUtf8(0x00CA),output.without.html)
    output.without.html <- gsub("&Euml;",intToUtf8(0x00CB),output.without.html)
    output.without.html <- gsub("&Igrave;",intToUtf8(0x00CC),output.without.html)
    output.without.html <- gsub("&Iacute;",intToUtf8(0x00CD),output.without.html)
    output.without.html <- gsub("&Icirc;",intToUtf8(0x00CE),output.without.html)
    output.without.html <- gsub("&Iuml;",intToUtf8(0x00CF),output.without.html)
    output.without.html <- gsub("&ETH;",intToUtf8(0x00D0),output.without.html)
    output.without.html <- gsub("&Ntilde;",intToUtf8(0x00D1),output.without.html)
    output.without.html <- gsub("&Ograve;",intToUtf8(0x00D2),output.without.html)
    output.without.html <- gsub("&Oacute;",intToUtf8(0x00D3),output.without.html)
    output.without.html <- gsub("&Ocirc;",intToUtf8(0x00D4),output.without.html)
    output.without.html <- gsub("&Otilde;",intToUtf8(0x00D5),output.without.html)
    output.without.html <- gsub("&Ouml;",intToUtf8(0x00D6),output.without.html)
    output.without.html <- gsub("&Oslash;",intToUtf8(0x00D8),output.without.html)
    output.without.html <- gsub("&Ugrave;",intToUtf8(0x00D9),output.without.html)
    output.without.html <- gsub("&Uacute;",intToUtf8(0x00DA),output.without.html)
    output.without.html <- gsub("&Ucirc;",intToUtf8(0x00DB),output.without.html)
    output.without.html <- gsub("&Uuml;",intToUtf8(0x00DC),output.without.html)
    output.without.html <- gsub("&Yacute;",intToUtf8(0x00DD),output.without.html)
    output.without.html <- gsub("&THORN;",intToUtf8(0x00DE),output.without.html)
    output.without.html <- gsub("&szlig;",intToUtf8(0x00DF),output.without.html)
    output.without.html <- gsub("&agrave;",intToUtf8(0x00E0),output.without.html)
    output.without.html <- gsub("&aacute;",intToUtf8(0x00E1),output.without.html)
    output.without.html <- gsub("&acirc;",intToUtf8(0x00E2),output.without.html)
    output.without.html <- gsub("&atilde;",intToUtf8(0x00E3),output.without.html)
    output.without.html <- gsub("&auml;",intToUtf8(0x00E4),output.without.html)
    output.without.html <- gsub("&aring;",intToUtf8(0x00E5),output.without.html)
    output.without.html <- gsub("&aelig;",intToUtf8(0x00E6),output.without.html)
    output.without.html <- gsub("&ccedil;",intToUtf8(0x00E7),output.without.html)
    output.without.html <- gsub("&egrave;",intToUtf8(0x00E8),output.without.html)
    output.without.html <- gsub("&eacute;",intToUtf8(0x00E9),output.without.html)
    output.without.html <- gsub("&ecirc;",intToUtf8(0x00EA),output.without.html)
    output.without.html <- gsub("&euml;",intToUtf8(0x00EB),output.without.html)
    output.without.html <- gsub("&igrave;",intToUtf8(0x00EC),output.without.html)
    output.without.html <- gsub("&iacute;",intToUtf8(0x00ED),output.without.html)
    output.without.html <- gsub("&icirc;",intToUtf8(0x00EE),output.without.html)
    output.without.html <- gsub("&iuml;",intToUtf8(0x00EF),output.without.html)
    output.without.html <- gsub("&eth;",intToUtf8(0x00F0),output.without.html)
    output.without.html <- gsub("&ntilde;",intToUtf8(0x00F1),output.without.html)
    output.without.html <- gsub("&ograve;",intToUtf8(0x00F2),output.without.html)
    output.without.html <- gsub("&oacute;",intToUtf8(0x00F3),output.without.html)
    output.without.html <- gsub("&ocirc;",intToUtf8(0x00F4),output.without.html)
    output.without.html <- gsub("&otilde;",intToUtf8(0x00F5),output.without.html)
    output.without.html <- gsub("&ouml;",intToUtf8(0x00F6),output.without.html)
    output.without.html <- gsub("&oslash;",intToUtf8(0x00F8),output.without.html)
    output.without.html <- gsub("&ugrave;",intToUtf8(0x00F9),output.without.html)
    output.without.html <- gsub("&uacute;",intToUtf8(0x00FA),output.without.html)
    output.without.html <- gsub("&ucirc;",intToUtf8(0x00FB),output.without.html)
    output.without.html <- gsub("&uuml;",intToUtf8(0x00FC),output.without.html)
    output.without.html <- gsub("&yacute;",intToUtf8(0x00FD),output.without.html)
    output.without.html <- gsub("&thorn;",intToUtf8(0x00FE),output.without.html)
    output.without.html <- gsub("&yuml;",intToUtf8(0x00FF),output.without.html)
    output.without.html <- gsub("&iexcl;",intToUtf8(0x00A1),output.without.html)
    output.without.html <- gsub("&cent;",intToUtf8(0x00A2),output.without.html)
    output.without.html <- gsub("&pound;",intToUtf8(0x00A3),output.without.html)
    output.without.html <- gsub("&curren;",intToUtf8(0x00A4),output.without.html)
    output.without.html <- gsub("&yen;",intToUtf8(0x00A5),output.without.html)
    output.without.html <- gsub("&brvbar;",intToUtf8(0x00A6),output.without.html)
    output.without.html <- gsub("&sect;",intToUtf8(0x00A7),output.without.html)
    output.without.html <- gsub("&uml;",intToUtf8(0x00A8),output.without.html)
    output.without.html <- gsub("&copy;",intToUtf8(0x00A9),output.without.html)
    output.without.html <- gsub("&ordf;",intToUtf8(0x00AA),output.without.html)
    output.without.html <- gsub("&laquo;",intToUtf8(0x00AB),output.without.html)
    output.without.html <- gsub("&not;",intToUtf8(0x00AC),output.without.html)
    output.without.html <- gsub("&reg;",intToUtf8(0x00AE),output.without.html)
    output.without.html <- gsub("&macr;",intToUtf8(0x00AF),output.without.html)
    output.without.html <- gsub("&deg;",intToUtf8(0x00B1),output.without.html)
    output.without.html <- gsub("&plusmn;",intToUtf8(0x00B2),output.without.html)
    output.without.html <- gsub("&sup2;",intToUtf8(0x00B3),output.without.html)
    output.without.html <- gsub("&sup3;",intToUtf8(0x00B4),output.without.html)
    output.without.html <- gsub("&acute;",intToUtf8(0x00B5),output.without.html)
    output.without.html <- gsub("&micro;",intToUtf8(0x00B6),output.without.html)
    output.without.html <- gsub("&para;",intToUtf8(0x00B7),output.without.html)
    output.without.html <- gsub("&cedil;",intToUtf8(0x00B8),output.without.html)
    output.without.html <- gsub("&sup1;",intToUtf8(0x00B9),output.without.html)
    output.without.html <- gsub("&ordm;",intToUtf8(0x00BA),output.without.html)
    output.without.html <- gsub("&raquo;",intToUtf8(0x00BB),output.without.html)
    output.without.html <- gsub("&frac14;",intToUtf8(0x00BC),output.without.html)
    output.without.html <- gsub("&frac12;",intToUtf8(0x00BD),output.without.html)
    output.without.html <- gsub("&frac34;",intToUtf8(0x00BE),output.without.html)
    output.without.html <- gsub("&iquest;",intToUtf8(0x00BF),output.without.html)
    output.without.html <- gsub("&times;",intToUtf8(0x00D7),output.without.html)
    output.without.html <- gsub("&divide;",intToUtf8(0x00F7),output.without.html)
    output.without.html <- gsub("&circ;",intToUtf8(0x00FE),output.without.html)
    output.without.html <- gsub("&tilde;",intToUtf8(0x007E),output.without.html)
    output.without.html <- gsub(intToUtf8(0xFB01),"fi",output.without.html, fixed = TRUE)
    return(output.without.html)
  }
  
  insert.html.entity <- function(input.without.html) {
    output.with.html <- input.without.html
    output.with.html <- gsub(intToUtf8(0x00C0),"&Agrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C1),"&Aacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C2),"&Acirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C3),"&Atilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C4),"&Auml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C5),"&Aring;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C6),"&AElig;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C7),"&Ccedil;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C8),"&Egrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00C9),"&Eacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CA),"&Ecirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CB),"&Euml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CC),"&Igrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CD),"&Iacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CE),"&Icirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00CF),"&Iuml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D0),"&ETH;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D1),"&Ntilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D2),"&Ograve;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D3),"&Oacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D4),"&Ocirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D5),"&Otilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D6),"&Ouml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D8),"&Oslash;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D9),"&Ugrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DA),"&Uacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DB),"&Ucirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DC),"&Uuml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DD),"&Yacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DE),"&THORN;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00DF),"&szlig;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E0),"&agrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E1),"&aacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E2),"&acirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E3),"&atilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E4),"&auml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E5),"&aring;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E6),"&aelig;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E7),"&ccedil;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E8),"&egrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00E9),"&eacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00EA),"&ecirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00EB),"&euml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00EC),"&igrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00ED),"&iacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00EE),"&icirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00EF),"&iuml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F0),"&eth;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F1),"&ntilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F2),"&ograve;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F3),"&oacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F4),"&ocirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F5),"&otilde;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F6),"&ouml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F8),"&oslash;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F9),"&ugrave;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FA),"&uacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FB),"&ucirc;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FC),"&uuml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FD),"&yacute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FE),"&thorn;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FF),"&yuml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A1),"&iexcl;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A2),"&cent;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A3),"&pound;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A4),"&curren;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A5),"&yen;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A6),"&brvbar;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A7),"&sect;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A8),"&uml;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00A9),"&copy;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00AA),"&ordf;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00AB),"&laquo;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00AC),"&not;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00AE),"&reg;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00AF),"&macr;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B1),"&deg;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B2),"&plusmn;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B3),"&sup2;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B4),"&sup3;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B5),"&acute;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B6),"&micro;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B7),"&para;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B8),"&cedil;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00B9),"&sup1;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BA),"&ordm;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BB),"&raquo;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BC),"&frac14;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BD),"&frac12;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BE),"&frac34;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00BF),"&iquest;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00D7),"&times;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00F7),"&divide;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x00FE),"&circ;",output.with.html)
    output.with.html <- gsub(intToUtf8(0x007E),"&tilde;",output.with.html)
    return(output.with.html)
  }
  
  update_progress_info <- function(print_message){
    if (length(PDE.globals$le.progress.textbox) > 0){
      ## add completion  info
      progress_info_length <- length(tcltk2::tk2list.get(PDE.globals$le.progress.textbox))
      if (progress_info_length > 3) {
        new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)[!grepl("^$",
                                                                                tcltk2::tk2list.get(PDE.globals$le.progress.textbox))]
      } else {
        new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)
      }
      tcltk::tkconfigure(PDE.globals$le.progress.textbox,values = c(new_list,print_message))
      tcltk::tkconfigure(PDE.globals$le.progress.textbox,textvariable = tcltk::tclVar(print_message))
      tcltk::tcl("update")
    }
  }
  
  remove_backref <- function(x) {
    for (s in 1:length(x)){
      string_strsplit <- strsplit(x[s],"")[[1]]
      for (l in 1:length(string_strsplit)){
        if (l != length(string_strsplit)){
          if ((string_strsplit[l] == "\\\\") && (string_strsplit[l+1] %in% as.character(0:9))){
            string_strsplit <- gsub("\\\\","",string_strsplit)
          }
        }
      }
      string_strsplit <- gsub("\\\\","",string_strsplit)
      x[s] <- paste0(string_strsplit,collapse ="")
    }
    return(x)
  }
  
  ## set all indicator variables ---------------------------
  integrity.indicator <- TRUE ## indicates if txt, keeplayouttxt and html copy of the PDF file are created correctly
  filterwords.go <- FALSE ## indicator if filter words were found or not set
  searchwords.go <- FALSE ## indicator if search words were found or not set
  nexti <- FALSE ## indicator for html table to be processed
  output_files <- NULL ## this is the output to return at the end
  out_msg <- NULL
  sw_in_tab_counted <- FALSE
  
  ## set statics output -----------------------------------------
  stat_output <- NULL
  pdf_word_count <- 0
  pdf_page_count <- 0
  pdf_filterwords <- NULL
  pdf_filterword_times <- NULL
  pdf_filterword_names <- NULL
  pdf_filterword_total <- NULL
  pdf_searchwords <- NULL
  pdf_searchword_times <- NULL
  pdf_searchword_names <- NULL
  pdf_searchword_total <- NULL
  pdf_sentences_w_searchwords <- NA
  search.word.category_total <- NULL
  
  ## set the paths of the files ---------------------------------
  output <- NULL
  pdfpath <- pdf
  txtpath <- gsub(".pdf[^.pdf]*$", ".txt", pdfpath)
  keeplayouttxtpath <- gsub(".pdf[^.pdf]*$", "_keeplayout.txt",
                            pdfpath)
  
  ##make sure filter and search words do not have duplicates
  stop_ind <- FALSE
  if (any(duplicated(search.words))){
    print_message <- paste0("Following search words are duplicated in list: ",
                            paste(search.words[duplicated(search.words)], collapse = ";"),
                            ". Please remove the duplicates and restart the analysis.")
    out_msg <- c(out_msg, print_message)
    update_progress_info(print_message)
    stop_ind <- TRUE
  }
  if (any(duplicated(filter.words))){
    print_message <- paste0("Following search words are duplicated in list: ",
                            paste(filter.words[duplicated(filter.words)], collapse = ";"),
                            ". Please remove the duplicates and restart the analysis.")
    out_msg <- c(out_msg, print_message)
    update_progress_info(print_message)
    stop_ind <- TRUE
  }
  if (stop_ind == TRUE){
    stop("Words were duplicated in the keyword list. Please remove the duplicates and restart the analysis.")
  }
  
  
  ## create the id and output dir ----------------------------------
  dir.create(out, showWarnings = FALSE)
  id <- sub("^(.*)\\..*$", "\\1", basename(txtpath))
  print_message <- paste0("Following file is processing: \'",id,".pdf\'")
  out_msg <- c(out_msg, print_message)
  if (verbose) cat(utils::tail(out_msg,1), sep="\n")
  
  ## 1) Create txt and html copies of PDF file ---------------------------------------
  ## test of Xpdftools are installed
  xpdf_config_location <- paste0(system.file(package = "PDE"),"/bin/XPDF_DIR.config")
  if (file.exists(xpdf_config_location)){
    pdftotext_location <- grep("pdftotext",readLines(xpdf_config_location), value = TRUE)
    
    pdftohtml_location <- grep("pdftohtml",readLines(xpdf_config_location), value = TRUE)
    
    pdftopng_location <- grep("pdftopng",readLines(xpdf_config_location), value = TRUE)
    
    if (length(file.exists(pdftotext_location)) == 0 ||
        length(file.exists(pdftohtml_location)) == 0 ||
        length(file.exists(pdftopng_location)) == 0){
      install.test <- PDE_check_Xpdf_install(verbose=verbose)
    } else {
      install.test <- TRUE
    }
    
  } else {
    install.test <- PDE_check_Xpdf_install(verbose=verbose)
  }
  if (install.test == FALSE) {
    if (length(PDE.globals$le.progress.textbox) > 0){
      result_install <- tk_messageBox(type = "yesno",
                    paste0(attributes(install.test)$msg, 
                           " Do you want to download and install xpdf now?"), caption = "xpdf not installed")
      if (result_install == "yes"){
        PDE_install_Xpdftools4.02(permission = 1)
        update_progress_info("Please stop and restart the analysis.")
        stop("Please stop and restart the analysis.")
      } else {
        update_progress_info(attributes(install.test)$msg)
        stop(attributes(install.test)$msg)
      }
    } else {
      stop(attributes(install.test)$msg)
    }
  } else {
    pdftotext_location <- grep("pdftotext",readLines(xpdf_config_location), value = TRUE)
    
    pdftohtml_location <- grep("pdftohtml",readLines(xpdf_config_location), value = TRUE)
    
    pdftopng_location <- grep("pdftopng",readLines(xpdf_config_location), value = TRUE)
  }
  
  system(paste0("\"",pdftotext_location,"\" -layout",
                " \"", pdfpath, "\" \"", keeplayouttxtpath,
                "\""), wait = TRUE, ignore.stderr = TRUE)
  system(paste0("\"",pdftotext_location,"\" \"", pdfpath,
                "\" \"", txtpath, "\""), wait = TRUE,
         ignore.stderr = TRUE)
  htmlpath <- gsub(".pdf[^.pdf]*$", ".html", pdfpath)
  ## convert PDF to HTML
  system(paste0("\"",pdftohtml_location,"\" \"", pdfpath,
                "\" \"", htmlpath, "\""), wait = TRUE,
         ignore.stderr = TRUE)
  
  ## test if keeplayouttxt files were saved with funny name
  fixed_basekeeplayouttxtpath1 <- iconv(basename(keeplayouttxtpath), from = "Windows-1252", to = "UTF-8")
  fixed_keeplayouttxtpath1 <- paste0(dirname(keeplayouttxtpath),"/",fixed_basekeeplayouttxtpath1)
  fixed_basekeeplayouttxtpath2 <- fixed_basekeeplayouttxtpath1
  Encoding(fixed_basekeeplayouttxtpath2) <- "Windows-1252"
  fixed_keeplayouttxtpath2 <- paste0(dirname(keeplayouttxtpath),"/",fixed_basekeeplayouttxtpath2)
  if (file.exists(fixed_keeplayouttxtpath1)){
    response <- file.rename(from=fixed_keeplayouttxtpath1, to=keeplayouttxtpath)
  } else if (file.exists(fixed_keeplayouttxtpath2)){
    response <- file.rename(from=fixed_keeplayouttxtpath2, to=keeplayouttxtpath)
  }
  
  ## test if txt files were saved with funny name
  fixed_basetxtpath1 <- iconv(basename(txtpath), from = "Windows-1252", to = "UTF-8")
  fixed_txtpath1 <- paste0(dirname(txtpath),"/",fixed_basetxtpath1)
  fixed_basetxtpath2 <- fixed_basetxtpath1
  Encoding(fixed_basetxtpath2) <- "Windows-1252"
  fixed_txtpath2 <- paste0(dirname(txtpath),"/",fixed_basetxtpath2)
  if (file.exists(fixed_txtpath1)){
    response <- file.rename(from=fixed_txtpath1, to=txtpath)
  } else if (file.exists(fixed_txtpath2)){
    response <- file.rename(from=fixed_txtpath2, to=txtpath)
  }
  
  ## test if files were saved with funny name
  fixed_basehtmlpath1 <- iconv(basename(htmlpath), from = "Windows-1252", to = "UTF-8")
  fixed_htmlpath1 <- paste0(dirname(htmlpath),"/",fixed_basehtmlpath1)
  fixed_basehtmlpath2 <- fixed_basehtmlpath1
  Encoding(fixed_basehtmlpath2) <- "Windows-1252"
  fixed_htmlpath2 <- paste0(dirname(htmlpath),"/",fixed_basehtmlpath2)
  if (dir.exists(fixed_htmlpath1)){
    htmlpath <- fixed_htmlpath1
  } else if (file.exists(fixed_htmlpath2)){
    htmlpath <- fixed_htmlpath2
  }
  
  ## add completion  info
  update_progress_info(print_message)
  ## 2.1) Check txt and html file integrity ----------------------------------
  integrity.indicator <- TRUE
  
  ## check if html was created (if pdf is secured)
  if (!dir.exists(htmlpath) || !file.exists(paste0(htmlpath, "/index.html"))) {
    ## export error and do not remove file change
    print_message <- paste0("\'", id, ".pdf\' is most likely secured and cannot be processed!")
    out_msg <- c(out_msg, print_message)
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
    update_progress_info(print_message)
    dir.create(paste0(out,"/secured"), showWarnings = FALSE)
    write(paste0(pdfpath, " is most likely secured and cannot be processed!"),
          file = paste0(out,"/secured/",id, "_is_secured.txt"))
    integrity.indicator <- FALSE
    ## if cpymv is either cpy or mv and filterwords were used
    if (cpy_mv == "cpy" && filter.words[1] != ""){
      dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
      dir.create(paste0(out,"/pdfs/secured"), showWarnings = FALSE)
      file.copy(pdf, paste0(out,"/pdfs/secured"))
      print_message <- paste0(basename(pdf),
                              " was copied to \'", out,"/pdfs/secured\'.")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
    } else if (cpy_mv == "mv" && filter.words[1] != ""){
      dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
      dir.create(paste0(out,"/pdfs/secured"), showWarnings = FALSE)
      file.copy(pdf, paste0(out,"/pdfs/secured"))
      unlink(pdf)
      print_message <- paste0(basename(pdf),
                              " was moved to \'", out,"/pdfs/secured\'.")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
    }
    ## if html was created
  } else {
    ## read in the txt files
    txtcontent <- readin_txt(txtpath)
    ##split rows with double page splits
    doublepagesplits <- grep("^\\f.*\\f", txtcontent)
    counter <- 0
    for (dbps in doublepagesplits){
      new_rows <- strsplit(txtcontent[dbps + counter],"\\f")[[1]][-1]
      for (r in length(new_rows):2){
        txtcontent[dbps + counter] <- paste0("\f",new_rows[1])
        txtcontent <- unlist(append(txtcontent, list(paste0("\f",new_rows[r])), dbps + counter))
        counter <- counter + 1
      }
    }
    
    splittxtcontent <- page_splits(txtcontent)
    keeplayouttxtcontent <- readin_txt(keeplayouttxtpath)
    indexcontent <- readLines(paste0(htmlpath, "/index.html"))
    ## if the txt or html files have no content
    if (identical(indexcontent, "")) {
      ## export error and do not remove file change
      print_message <- paste0("\'", id, ".pdf\' is most likely secured and cannot be processed!")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      dir.create(paste0(out,"/secured"), showWarnings = FALSE)
      write(paste0(pdfpath, " is most likely secured and cannot be processed!"),
            file = paste0(out,"/secured/",id, "_is_secured.txt"))
      integrity.indicator <- FALSE
      ## if cpymv is either cpy or mv and filterwords were used
      if (cpy_mv == "cpy" && filter.words[1] != ""){
        dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
        dir.create(paste0(out,"/pdfs/secured"), showWarnings = FALSE)
        file.copy(pdf, paste0(out,"/pdfs/secured"))
        print_message <- paste0(basename(pdf),
                                " was copied to \'", out,"/pdfs/secured\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      } else if (cpy_mv == "mv" && filter.words[1] != ""){
        dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
        dir.create(paste0(out,"/pdfs/secured"), showWarnings = FALSE)
        file.copy(pdf, paste0(out,"/pdfs/secured"))
        unlink(pdf)
        print_message <- paste0(basename(pdf),
                                " was moved to \'", out,"/pdfs/secured\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      }
    } else if (identical(txtcontent, "") || identical(keeplayouttxtcontent, "") ||
               identical(gsub("\f","",txtcontent), "") || identical(gsub("\f","",keeplayouttxtcontent), "")  ) {
      print_message <- paste0("\'", id, ".pdf\' most likely contains no text content or is a scanned document!")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      dir.create(paste0(out,"/nr/"), showWarnings = FALSE)
      write(paste0(pdfpath, " most likely contains no text content or is a scanned document!"),
            file = paste0(out, "/nr/", id, "_non-readable.txt"))
      integrity.indicator <- FALSE
      ## if cpymv is either cpy or mv and filterwords were used
      if (cpy_mv == "cpy" && filter.words[1] != ""){
        dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
        dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
        file.copy(pdf, paste0(out,"/pdfs/nr"))
        print_message <- paste0(basename(pdf),
                                " was copied to \'", out,"/pdfs/nr\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      } else if (cpy_mv == "mv" && filter.words[1] != ""){
        dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
        dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
        file.copy(pdf, paste0(out,"/pdfs/nr"))
        unlink(pdf)
        print_message <- paste0(basename(pdf),
                                " was moved to \'", out,"/pdfs/nr\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      }
      ## when txtcontent is there and html index was created
    } else {
      ## extract all the page names ##
      pages <- NULL
      for (line in indexcontent) {
        if (grepl("a href=\"", line)) pages <- c(pages,
                                                 substr(line, regexpr("a href=\"", line) + 8,
                                                        regexpr("html", line) + 3))
      }
      
      ## read in the html file ##
      htmlcontent <- vector("list", length(pages))
      for (i in 1:length(pages)) {
        htmlpagecontent <- readLines(paste0(htmlpath,"/", pages[i]), encoding = "UTF-8", warn = FALSE)
        if (identical(htmlpagecontent, "")) {
          print_message <- paste0("\'", id, ".pdf\' most likely contains no text content or is a scanned in document!")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
          dir.create(paste0(out,"/nr/"), showWarnings = FALSE)
          write(paste0(pdfpath, " most likely contains no text content or is a scanned in document!"),
                file = paste0(out, "/nr/", id, "_non-readable.txt"))
          integrity.indicator <- FALSE
          ## if cpymv is either cpy or mv and filterwords were used
          if (cpy_mv == "cpy" && filter.words[1] != ""){
            dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
            dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
            file.copy(pdf, paste0(out,"/pdfs/nr"))
            print_message <- paste0(basename(pdf),
                                    " was copied to \'", out,"/pdfs/nr\'.")
            out_msg <- c(out_msg, print_message)
            if (verbose) cat(utils::tail(out_msg,1), sep="\n")
            update_progress_info(print_message)
          } else if (cpy_mv == "mv" && filter.words[1] != ""){
            dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
            dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
            file.copy(pdf, paste0(out,"/pdfs/nr"))
            unlink(pdf)
            print_message <- paste0(basename(pdf),
                                    " was moved to \'", out,"/pdfs/nr\'.")
            out_msg <- c(out_msg, print_message)
            if (verbose) cat(utils::tail(out_msg,1), sep="\n")
            update_progress_info(print_message)
          }
          break
        }
        ## replace all fi
        lines_with_fi <- grep(intToUtf8(0xFB01),htmlpagecontent)
        for (line in lines_with_fi){
          res <- try(gsub(intToUtf8(0xFB01),"fi",htmlpagecontent[line], fixed = TRUE),silent = TRUE)
          if (inherits(res,"try-error")){
            htmlpagecontent[line] <- iconv(htmlpagecontent[line], 'UTF-8', 'latin1', 'bit')
          }
          htmlpagecontent[line] <- gsub(intToUtf8(0xFB01),"fi",htmlpagecontent[line], fixed = TRUE)
        }
        ## replace p styles
        ## get the list of p styles
        list_of_fts <- NULL
        lines_with_ft <- grep("\\.ft",htmlpagecontent)
        if (length(lines_with_ft) > 0){
          for (ln in lines_with_ft){
            list_of_fts <- rbind(list_of_fts,
                                 cbind(ft=sub("\\{.*$","",substr(htmlpagecontent[ln],
                                                                 regexpr("\\.ft",htmlpagecontent[ln])[1]+1,
                                                                 nchar(htmlpagecontent[ln]))),
                                       style=sub("\\}.*$","",sub("^.*\\{","",substr(htmlpagecontent[ln],
                                                                                    regexpr("\\.ft",htmlpagecontent[ln])[1]+1,
                                                                                    nchar(htmlpagecontent[ln]))))))
          }
          ## replace each ft style in that page
          for (ft.numb in 1:nrow(list_of_fts)){
            htmlpagecontent <- gsub(paste0("\" class=\"",list_of_fts[ft.numb,1],"\""),
                                    paste0(";",list_of_fts[ft.numb,2],"\""),htmlpagecontent)
          }
        }
        
        htmlcontent[[i]] <- htmlpagecontent
      }
    } ## end if the txt or html files have no content
  } ## end if (!dir.exists(htmlpath))
  
  if (integrity.indicator == TRUE) {
    ## if the file is only images or empty then don't process
    realcontent <- gsub("^\\f", "", paste(txtcontent,
                                          collapse = ""))
    realcontent <- gsub("\t", "", realcontent)
    realcontent <- gsub(" ", "", realcontent)
    if (realcontent == "") {
      integrity.indicator <- FALSE
      print_message <- paste0("\'", id, ".pdf\' has no readable content")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      ## write an empty file
      if (write.txt.doc.file == TRUE) {
        dir.create(paste0(out,"/nr/"), showWarnings = FALSE)
        write(paste0(pdfpath, " has no readable content in PDF file"),
              file = paste0(out, "/nr/", id, "_non-readable.txt"))
        integrity.indicator <- FALSE
        ## if cpymv is either cpy or mv and filterwords were used
        if (cpy_mv == "cpy" && filter.words[1] != ""){
          dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
          dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
          file.copy(pdf, paste0(out,"/pdfs/nr"))
          print_message <- paste0(basename(pdf),
                                  " was copied to \'", out,"/pdfs/nr\'.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        } else if (cpy_mv == "mv" && filter.words[1] != ""){
          dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
          dir.create(paste0(out,"/pdfs/nr"), showWarnings = FALSE)
          file.copy(pdf, paste0(out,"/pdfs/nr"))
          unlink(pdf)
          print_message <- paste0(basename(pdf),
                                  " was moved to \'", out,"/pdfs/nr\'.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        }
      }
    }
  }
  
  ## if there was an issue with creating files
  if (integrity.indicator == FALSE) {
    pdf_word_count <- 0
    pdf_page_count <- 0
    pdf_filterwords <- NULL
    pdf_searchwords <- NULL
    search.word.category_total <- NULL
    if (!filter.words[1] == "") {
      for (i in 1:length(filter.words)) {
        word <- filter.words[i]
        ## add filterwords to output
        if (ignore.case.fw == TRUE){
          ic <- "ic"
        } else {
          ic <- "nic"
        }
        if (regex.fw == TRUE){
          reg <- "regex"
        } else {
          reg <- "nregex"
        }
        pdf_filterwords <- c(pdf_filterwords, paste0("FW_",ic,"_",reg,":",word))
        pdf_filterword_total <- NA
        pdf_filterword_times <- rep(NA,length(pdf_filterwords))
      }
    }
    if (!search.words[1] == "") {
      for (i in 1:length(search.words)) {
        word <- search.words[i]
        ## add filterwords to output
        if (ignore.case.sw == TRUE){
          ic <- "ic"
        } else {
          ic <- "nic"
        }
        if (regex.sw == TRUE){
          reg <- "regex"
        } else {
          reg <- "nregex"
        }
        pdf_searchwords <- c(pdf_searchwords, paste0("SW_",ic,"_",reg,":",word))
        pdf_searchword_total <- NA
        pdf_searchword_times <- rep(NA,length(pdf_searchwords))
        search.word.category_total <- NULL
        if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
          for (swc in 1:length(unique(search.word.categories))){
            search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
          }
        }
      }
    }
    stat_output <- cbind(pdf_word_count = pdf_word_count, pdf_page_count = pdf_page_count,
                         pdf_filterword_total = pdf_filterword_total, 
                         pdf_filterword_percentage = paste0(as.character(as.numeric(pdf_filterword_total)/as.numeric(pdf_word_count)*100),"%"), 
                         pdf_searchword_total = pdf_searchword_total,rbind(search.word.category_total),
                         rbind(pdf_filterword_times), rbind(pdf_searchword_times))
    extracol_num <- (ncol(stat_output) - 
                       length(pdf_filterword_times) - 
                       length(pdf_searchword_times) -
                       length(search.word.category_total) + 1)
    colnames(stat_output)[extracol_num:ncol(stat_output)] <- c(unique(search.word.categories),pdf_filterwords,pdf_searchwords)
    rownames(stat_output) <- id
    output_files$stat_output <- data.frame(cbind(stat_output, 
                                                 pdf_sentences_w_searchwords = NA),
                                           check.names = FALSE)
    ##if everything is ok
  } else {
    ## determine word count of pdf file
    pdf_word_count <- sum(sapply(gregexpr("[[:alpha:]]+", txtcontent), function(x) sum(x > 0)))
    pdf_page_count <- length(htmlcontent)
    
    ## 2.2) Check all the options chosen for PDE analyzer ------------------------------
    ## if general extraction (search words is undefined), then context <- 0
    if (search.words[1] == "" || search.words[1] == "*" ||
        search.words[1] == ".") {
      context <- 0
      search.words <- ""
    }
    if (ignore.case.sw == FALSE) ignore.case.sw <- FALSE
    else if (!ignore.case.sw == TRUE) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "ignore.case.sw: ignore.case.sw has to be either TRUE or FALSE")
      stop("ignore.case.sw: ignore.case.sw has to be either TRUE or FALSE")
    }
    if (is.na(filter.words[1])) filter.words <- ""
    ## adjust filter.words with regex
    if (regex.fw == FALSE){
      for (fw_pos in 1:length(filter.words)){
        filter.words[fw_pos] <- re.escape(filter.words[fw_pos])
      }
    }
    if (is.null(ncol(filter.words))) filter.word.table <- data.frame(words = filter.words,
                                                                ignore.case.fw = ignore.case.fw)
    # if (!is.numeric(filter.word.times)) {
    #   tcltk::tkmessageBox(title = "Warning",
    #                       type = "ok", icon = "warning",
    #                       message = "filter.word.times: has to be a number")
    #   stop("filter.word.times: has to be a number")
    # }
    if (is.null(ncol(table.heading.words))) table.heading.words <- data.frame(words = table.heading.words,
                                                                              ignore.case.th = ignore.case.th)
    ## adjust search.words with regex
    if (regex.sw == FALSE){
      for (sw_pos in 1:length(search.words)){
        search.words[sw_pos] <- re.escape(search.words[sw_pos])
      }
    }
    if (is.null(ncol(search.words))) search.word.table <- data.frame(words = search.words,
                                                                ignore.case.sw = ignore.case.sw)
    if (write.table.locations == FALSE) write.table.locations <- FALSE
    else if (!write.table.locations == TRUE) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "write.table.locations: has to be either TRUE or FALSE")
      stop("write.table.locations: has to be either TRUE or FALSE")
    }
    if (is.na(out.table.format)) {
      out.table.format <- ".csv (WINDOWS-1252)"
    } else if (!(out.table.format %in% c(".csv (WINDOWS-1252)", ".csv (macintosh)", ".csv (UTF-8)", 
                                         ".tsv (WINDOWS-1252)",".tsv (macintosh)",".tsv (UTF-8)"))) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = paste("out.table.format: has to be either .csv (WINDOWS-1252), .csv (macintosh), .csv (UTF-8)",
                                          "or .tsv (WINDOWS-1252), .tsv (macintosh), .tsv (UTF-8)"))
      stop(paste("out.table.format: has to be either .csv (WINDOWS-1252), .csv (macintosh), .csv (UTF-8)",
                 "or .tsv (WINDOWS-1252), .tsv (macintosh), .tsv (UTF-8)"))
    }
    if (grepl("csv", out.table.format)) {
      out.table.separator <- ","
      out.table.ext <- ".csv"
    }
    if (grepl("tsv", out.table.format)) {
      out.table.separator <- "\t"
      out.table.ext <- ".tsv"
    }
    if (grepl("WINDOWS-1252", out.table.format)) {
      out.encoding <- "WINDOWS-1252"
    } else if (grepl("macintosh", out.table.format)) {
      out.encoding <- "macintosh"
    } else if (grepl("UTF-8", out.table.format)) {
      out.encoding <- "UTF-8"
    } else {
      out.encoding <- "WINDOWS-1252"
    }
    
    
    if (!is.numeric(dev_x)) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "dev_x: has to be a number")
      stop("dev_x: has to be a number")  ## deviation between cell positions, might 
      ##have to be increased if words that should be in the same column
    }
    if (!is.numeric(context)) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "context: has to be a number")
      stop("context: has to be a number")  ## +/- context number of sentences 
      ## before and after the search word was found will be put out
    }
    if (write.tab.doc.file == FALSE) write.tab.doc.file <- FALSE
    else if (!write.tab.doc.file == TRUE) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "write.tab.doc.file: has to be either TRUE or FALSE")
      stop("write.tab.doc.file: has to be either TRUE or FALSE")
    }
    if (write.txt.doc.file == FALSE) write.txt.doc.file <- FALSE
    else if (!write.txt.doc.file == TRUE) {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = "write.txt.doc.file: has to be either TRUE or FALSE")
      stop("write.txt.doc.file: has to be either TRUE or FALSE")
    }
  }
  
  ## reset variables
  if (integrity.indicator == TRUE) {
    output <- NULL
    htmltablelines <- NULL
    keeplayouttxttablelines <- NULL
    txttablelines <- NULL
  }
  
  ## 2.3) Make content ----------------------------------------
  if (integrity.indicator == TRUE) {
    content <- list(txtcontent, keeplayouttxtcontent)
    
    ## make a variable only with the txt content
    txthtmlcontent <- htmlcontent
    ##TODO replace superscript
    ## go through pages
    for (j in 1:length(txthtmlcontent)) {
      ## go through each html line
      for (z in 1:length(txthtmlcontent[[j]])) {
        ## Removing the Table wording
        line <- txthtmlcontent[[j]][z]
        line.txthtmlcontent <- ""
        res <- try(utf8ToInt(line),silent = TRUE)
        if (inherits(res,"try-error")) line <- iconv(line, 'UTF-8', 'latin1', 'byte')
        ##replace superscript
        while (grepl("vertical-align:super",line)){
          super_pos <- regexpr("vertical-align:super",line)[1]
          ## replace the end of the superscript with "]"
          endspan_positions <- gregexpr("</span>",line)[[1]]
          s <- 1
          endspan_pos <- endspan_positions[s] - super_pos
          while (endspan_pos<0){
            s <- s + 1
            endspan_pos <- endspan_positions[s] - super_pos
          }
          endspan_pos <- endspan_pos + super_pos
          line_list <- unlist(strsplit(line, split = ""))
          line_list[endspan_pos] <- "]<"
          line <- paste(line_list,collapse = "")
          
          ## replace the beginning of the superscript with "^["
          endofstartspan_positions <- gregexpr(";\">",line)[[1]]
          s <- 1
          endofstartspan_pos <- endofstartspan_positions[s] - super_pos
          while (endofstartspan_pos<0){
            s <- s + 1
            endofstartspan_pos <- endofstartspan_positions[s] - super_pos
          }
          endofstartspan_pos <- endofstartspan_pos + super_pos
          
          line_before_super <- sub("vertical-align:super.*", "", line)
          startspan_pos <- max(gregexpr("<span id=",line_before_super)[[1]])
          line_list <- unlist(strsplit(line, split = ""))
          line_list[endofstartspan_pos+2] <- ">^["
          line <- paste(line_list,collapse = "")
          line <- sub("vertical-align:super", "vertical-align:baseline", line)
          # line <- sub(paste0("^(.{",as.numeric(startspan_pos)-1,"})(.{",as.numeric(endofstartspan_pos-startspan_pos+2),"})."),"\\1^[",
          #             line)
        }
        ##replace subscript
        while (grepl("vertical-align:sub",line)){
          sub_pos <- regexpr("vertical-align:sub",line)[1]
          ## replace the end of the subscript with "]"
          endspan_positions <- gregexpr("</span>",line)[[1]]
          s <- 1
          endspan_pos <- endspan_positions[s] - sub_pos
          while (endspan_pos<0){
            s <- s + 1
            endspan_pos <- endspan_positions[s] - sub_pos
          }
          endspan_pos <- endspan_pos + sub_pos
          line_list <- unlist(strsplit(line, split = ""))
          line_list[endspan_pos] <- "]<"
          line <- paste(line_list,collapse = "")
          
          ## replace the beginning of the subscript with "_["
          endofstartspan_positions <- gregexpr(";\">",line)[[1]]
          s <- 1
          endofstartspan_pos <- endofstartspan_positions[s] - sub_pos
          while (endofstartspan_pos<0){
            s <- s + 1
            endofstartspan_pos <- endofstartspan_positions[s] - sub_pos
          }
          endofstartspan_pos <- endofstartspan_pos + sub_pos
          
          line_before_sub <- sub("vertical-align:sub.*", "", line)
          startspan_pos <- max(gregexpr("<span id=",line_before_sub)[[1]])
          line_list <- unlist(strsplit(line, split = ""))
          line_list[endofstartspan_pos+2] <- ">_["
          line <- paste(line_list,collapse = "")
          
        }
        ##replace different html formating
        line <- gsub("</p>","</span>",line)
        ## replace line break with space (not optimal but better for searching)
        line <- gsub("<br/>"," ",line)
        ## remove residue from hyperlinks
        line <- gsub("<a href=\".*?>","",line)
        line <- gsub("</a>","",line)
        rev.line <- intToUtf8(rev(utf8ToInt(line)))
        for (spanpos in rev(gregexpr(">naps/<",rev.line)[[1]])){
          add.txthtmlcontent <- substr(rev.line,spanpos+7, regexpr(">\"",
                                                                   substr(rev.line,spanpos+7,
                                                                          nchar(rev.line)))+spanpos+7-2)
          line.txthtmlcontent <- paste0(line.txthtmlcontent,intToUtf8(rev(utf8ToInt(add.txthtmlcontent))))
        }
        if (grepl("&",line.txthtmlcontent)){
          txthtmlcontent[[j]][z] <- replace.html.entity(line.txthtmlcontent)
        } else {
          txthtmlcontent[[j]][z] <- line.txthtmlcontent
        }
      }  ## end go through each line z
      content[[(j+2)]] <- txthtmlcontent[[j]]
    }
  }
  
  
  ## 3) Evaluate for filter words ---------------------------------------
  list_of_abbrevs <- NULL
  
  ## 3.1) Filter Search ---------------------------------------
  if (integrity.indicator == TRUE && !filter.word.table[1, "words"] == "") {
    word.txtline.fw <- NULL
    for (i in 1:nrow(filter.word.table)) {
      ## search for lines with filter word in [txtcontent]
      word <- filter.word.table[i, "words"]
      ignore.case.fw <- filter.word.table[i, "ignore.case.fw"]
      detected_line <- grep(word, txtcontent, ignore.case = ignore.case.fw)
      word.txtline.fw <- c(word.txtline.fw,
                           detected_line)
      
      ## 3.2) Replace abbreviations -----------------------------------------------------
      
      if (eval.abbrevs == TRUE && length(word.txtline.fw) > 0){
        ## Check if any occurences (heading + text) of the
        ## filter word are defining and abbreviation
        ## go through each txtcontent line that the searchword was found
        for (nth in 1:length(word.txtline.fw)) {
          paragraph <- txtcontent[word.txtline.fw[[nth]]]
          ## identify definitions of abbrev
          occur_double_dots_or_equal <- test_if_abbrev_double_dots_or_equal(searchword = as.character(word), 
                                                                            paragraph = paragraph,
                                                                            ignore.case = ignore.case.fw)
          occur_in_parantheses <- test_if_abbrev_in_parantheses(searchword = as.character(word), 
                                                                paragraph = paragraph,
                                                                ignore.case = ignore.case.fw)
          ## if abbrev was found in nth occurence
          for (occur in list(occur_double_dots_or_equal, occur_in_parantheses)){
            if (occur$res == TRUE) {
              ## replace if abbrev is not yet defined
              for (abbrev in c("abbrev_singular","abbrev_plural")) {
                if (!(occur[[abbrev]] %in% list_of_abbrevs)){
                  list_of_abbrevs <- c(list_of_abbrevs,occur[[abbrev]])
                  for (c in 1:length(content)){
                    to_find <- paste0("([^A-z|0-9]+|^)",occur[[abbrev]],"([^A-z|0-9|:]$|[^A-z|0-9|:][^=|:]|$)")
                    found_lines <- grep(to_find, content[[c]],
                                        ignore.case = FALSE)
                    for (line in found_lines){
                      found_pos <- gregexpr(to_find, content[[c]][line])[[1]]
                      found_pos_list <- gregexpr(to_find, content[[c]][line])[[1]]
                      add_pos <- 0
                      for (p in 1:length(found_pos)){
                        sub <- substr(content[[c]][line],found_pos[p] + add_pos,nchar(content[[c]][line]))
                        ## prevent double substitution if sub already contains substitution
                        if (grepl(occur[[sub("abbrev","replacement",abbrev)]], sub)) next
                        found_pos_list[p] <- regexpr(occur[[abbrev]],sub) + found_pos[p] + add_pos - 1
                        content[[c]][line] <- paste0(substr(content[[c]][line],1,found_pos_list[p]-1),
                                                     sub(occur[[abbrev]],
                                                         occur[[sub("abbrev","replacement",abbrev)]],
                                                         substr(content[[c]][line],
                                                                found_pos_list[p],nchar(content[[c]][line])),
                                                         ignore.case = FALSE))
                        add_pos <- add_pos + nchar(occur[[sub("abbrev","replacement",
                                                              abbrev)]]) - nchar(occur[[abbrev]])
                        ## correct for sub of abbrev definition e.g. methotrexate (MTX (methotrexate))
                        len_diff <- nchar(content[[c]][line]) - 
                          nchar(gsub(paste0("(",occur[[sub("abbrev","replacement",abbrev)]],")"),
                                     paste0("(",occur[[abbrev]],")"),
                                     content[[c]][line] , fixed = TRUE))
                        add_pos <- add_pos - len_diff
                        content[[c]][line]  <- gsub(paste0("(",occur[[sub("abbrev","replacement",abbrev)]],")"),
                                                    paste0("(",occur[[abbrev]],")"),
                                                    content[[c]][line] , fixed = TRUE)
                      } ## end for (p in length(found_pos)){
                    } ## end for (line in found_lines){
                  } ## for (c in 1:length(content)){
                } ##if (!(occur[[abbrev]] %in% list_of_abbrevs)){
              } ## for (abbrev in c("abbrev_singular","abbrev_plural")) {
            } ## if (occur$res) {
          } ## end for (abbrev in c("abbrev_singular","abbrev_plural")) {
        } ## end for (nth in 1:length(word.txtline.fw)) {
      } ## end replace abbreviations in content
    } ## end for each filter word
  } ## if (integrity.indicator == TRUE)
  
  ## if filter word abbreviations were found replace the abbreviations in content
  if (eval.abbrevs == TRUE && integrity.indicator == TRUE &&
      !filter.word.table[i, "words"] == "" && !is.null(list_of_abbrevs)) {
    txtcontent <- content[[1]]
    keeplayouttxtcontent <- content[[2]]
    
    ## html content per page
    for (pa in 3:length(content)) txthtmlcontent[[(pa-2)]] <- content[[pa]]
    
  } ## end replace abbreviations
  
  ## 3.3) Real filter Search ---------------------------------------
  if (integrity.indicator == TRUE) {
    word.txtline.fw <- NULL
    word.txtpos.fw <- NULL
    pdf_filterwords <- NULL
    ## if there are filter words
    if (!filter.word.table[1, "words"] == "") {
      for (i in 1:nrow(filter.word.table)) {
        ## search for lines with filter word in [txtcontent]
        word <- filter.word.table[i, "words"]
        ignore.case.fw <- filter.word.table[i, "ignore.case.fw"]
        ## add filterwords to output
        if (ignore.case.fw == TRUE){
          ic <- "ic"
        } else {
          ic <- "nic"
        }
        if (regex.fw == TRUE){
          reg <- "regex"
        } else {
          reg <- "nregex"
        }
        pdf_filterwords <- c(pdf_filterwords, paste0("FW_",ic,"_",reg,":",word))
        detected_line <- grep(word, txtcontent, ignore.case = ignore.case.fw)
        word.txtline.fw <- c(word.txtline.fw,
                             detected_line)
        current_word.txtpos.fw <- NULL
        for (li in detected_line){
          current_word.txtpos.fw <- c(current_word.txtpos.fw,
                                      gregexpr(word, txtcontent[li], ignore.case = ignore.case.fw)[[1]])
        }
        pdf_filterword_times <- c(pdf_filterword_times, length(current_word.txtpos.fw))
        pdf_filterword_names <- c(pdf_filterword_names, word)
        pdf_filterword_total <- sum(pdf_filterword_times)
        word.txtpos.fw <- c(word.txtpos.fw,current_word.txtpos.fw)
      }
      ## test if percent or number
      pdf_word_count <- sum(sapply(gregexpr("[[:alpha:]]+", txtcontent), function(x) sum(x > 0)))
      if (grepl("%",filter.word.times)){
        if (length(word.txtpos.fw)/pdf_word_count >= (as.numeric(sub("%","",filter.word.times))/100)) {
          filterwords.go <- TRUE
          print_message <- paste0(round((length(word.txtpos.fw)/pdf_word_count*100),4),
                                  "% of all words were filter word(s) in ", id, ".pdf.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        } else {
          filterwords.go <- FALSE
        }
      } else {
        if (length(word.txtpos.fw) >= as.numeric(filter.word.times)) {
          filterwords.go <- TRUE
          print_message <- paste0(length(word.txtpos.fw),
                                  " filter word(s) were detected in ", id, ".pdf.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
          ##TODO save length(word.txtpos.fw) in progress output
          
        } else {
          filterwords.go <- FALSE
        }
      }
      
      if (filterwords.go == FALSE){
        pdf_word_count <- sum(sapply(gregexpr("[[:alpha:]]+", txtcontent), function(x) sum(x > 0)))
        if (grepl("%",filter.word.times)){
          print_message <- paste0("\'",id,".pdf\' was filtered out due to a lack of the filter words. ",
                                  round((length(word.txtpos.fw)/pdf_word_count*100),4),
                                  "% of all words were filter word(s) in ", id, ".pdf.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        } else {
          print_message <- paste0("\'",id,".pdf\' was filtered out due to a lack of the filter words. ",
                                  length(word.txtpos.fw),
                                  " filter word(s) were detected in ", id, ".pdf.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        }
        ## write 0 search words
        if (!search.word.table[1, "words"] == "") {
          for (i in 1:nrow(search.word.table)) {
            word <- search.word.table[i, "words"]
            ## add filterwords to output
            if (ignore.case.sw == TRUE){
              ic <- "ic"
            } else {
              ic <- "nic"
            }
            if (regex.sw == TRUE){
              reg <- "regex"
            } else {
              reg <- "nregex"
            }
            pdf_searchwords <- c(pdf_searchwords, paste0("SW_",ic,"_",reg,":",word))
            pdf_searchword_total <- 0
            pdf_searchword_times <- rep(0,length(pdf_searchwords))
            search.word.category_total <- NULL
            if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
              for (swc in 1:length(unique(search.word.categories))){
                search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
              }
            }
          }
        }
        if (write.txt.doc.file == TRUE) {
          dir.create(paste0(out,"/excl_by_fw"), showWarnings = FALSE)
          pdf_word_count <- sum(sapply(gregexpr("[[:alpha:]]+", txtcontent), function(x) sum(x > 0)))
          if (grepl("%",filter.word.times)){
            utils::write.table(paste0("Not enough txt lines with filter word found. ",
                                      round((length(word.txtpos.fw)/pdf_word_count*100),4),
                                      "% of words were filter word(s) in ", id, ".pdf."),
                               paste0(out,"/excl_by_fw/",id,"_too_few_fwds",
                                      out.table.ext),
                               sep = out.table.separator, row.names = FALSE,
                               col.names = FALSE, na = "")
          } else {
            utils::write.table(paste0("Not enough txt lines with filter word found. ",
                                      length(word.txtpos.fw),
                                      " filter word(s) were detected in ", id, ".pdf."),
                               paste0(out,"/excl_by_fw/",id,"_too_few_fwds",
                                      out.table.ext),
                               sep = out.table.separator, row.names = FALSE,
                               col.names = FALSE, na = "")
          }
        }
        ## if cpymv is either cpy or mv
        if (cpy_mv == "cpy"){
          dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
          dir.create(paste0(out,"/pdfs/excl_by_fw"), showWarnings = FALSE)
          file.copy(pdf, paste0(out,"/pdfs/excl_by_fw"))
          print_message <- paste0(basename(pdf),
                                  " was copied to \'", out,"/pdfs/excl_by_fw\'.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        } else if (cpy_mv == "mv"){
          dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
          dir.create(paste0(out,"/pdfs/excl_by_fw"), showWarnings = FALSE)
          file.copy(pdf, paste0(out,"/pdfs/excl_by_fw"))
          unlink(pdf)
          print_message <- paste0(basename(pdf),
                                  " was moved to \'", out,"/pdfs/excl_by_fw\'.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(print_message)
        }
      }  ## end if filter words were present
    } else {
      out_msg <- c(out_msg, "No filter words chosen for analysis.")
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info("No filter words chosen for analysis.")
      filterwords.go <- TRUE
      pdf_filterword_total <- NA
      pdf_filterword_times <- NA
      pdf_filterwords <- c("filter_word_list")
    }  ## end if filter words were set
  } ## end 3.3) Filter Search
  
  ## 4) Search of search words -----------------------------------------------
  ## only if filter words were found or no filter was set continue and
  ## search words were set
  if (filterwords.go == TRUE && integrity.indicator == TRUE) {
    searchwords.go <- FALSE
    ## search for lines with search word
    word.txtline <- NULL
    for (i in 1:nrow(search.word.table)) {
      ## 4.1) Search for lines with search word -------------------------
      word <- search.word.table[i, "words"]
      ignore.case.sw <- search.word.table[i, "ignore.case.sw"]
      word.txtline <- NULL
      word.keeplayoutline <- NULL
      ## if search words were not chosen write all lines in txtline
      if (search.word.table[i, "words"] == ""){
        word.txtline <- 1:length(txtcontent)
        word.keeplayoutline <- 1:length(word.keeplayoutline)
      } else {
        ## search for lines with search word in [txtcontent]
        word.txtline <- grep(word, txtcontent, ignore.case = ignore.case.sw)
        ## search for lines with search word in [keeplayouttxtcontent]
        word.keeplayoutline <- grep(word, keeplayouttxtcontent,
                                    ignore.case = ignore.case.sw)
      }
      
      ## 4.2) Continue analysis when search words were found ----------------------------
      if (length(word.txtline) > 0) searchwords.go <- TRUE
      
      ## 4.3) Replace abbreviations -----------------------------------------------------
      if (eval.abbrevs == TRUE && length(word.txtline) > 0 &&
          !search.word.table[i, "words"] == ""){
        ## Check if any occurences (heading + text) of the
        ## search word are defining and abbreviation
        ## go through each txtcontent line that the searchword was found
        for (nth in 1:length(word.txtline)) {
          paragraph <- txtcontent[word.txtline[[nth]]]
          ## identify definitions of abbrev
          occur_double_dots_or_equal <- test_if_abbrev_double_dots_or_equal(searchword = as.character(word), 
                                                                            paragraph = paragraph,
                                                                            ignore.case.sw = ignore.case.sw)
          occur_in_parantheses <- test_if_abbrev_in_parantheses(searchword = as.character(word), 
                                                                paragraph = paragraph,
                                                                ignore.case.sw = ignore.case.sw)
          ## if abbrev was found in nth occurence
          for (occur in list(occur_double_dots_or_equal, occur_in_parantheses)){
            if (occur$res == TRUE) {
              ## replace if abbrev is not yet defined
              for (abbrev in c("abbrev_singular","abbrev_plural")) {
                if (!(occur[[abbrev]] %in% list_of_abbrevs)){
                  list_of_abbrevs <- c(list_of_abbrevs,occur[[abbrev]])
                  for (c in 1:length(content)){
                    to_find <- paste0("([^A-z|0-9]+|^)",occur[[abbrev]],"([^A-z|0-9|:]$|[^A-z|0-9|:][^=|:]|$)")
                    found_lines <- grep(to_find, content[[c]],
                                        ignore.case = FALSE)
                    for (line in found_lines){
                      found_pos <- gregexpr(to_find, content[[c]][line])[[1]]
                      found_pos_list <- gregexpr(to_find, content[[c]][line])[[1]]
                      add_pos <- 0
                      for (p in 1:length(found_pos)){
                        sub <- substr(content[[c]][line],found_pos[p] + add_pos,nchar(content[[c]][line]))
                        ## prevent double substitution if sub already contains substitution
                        if (grepl(occur[[sub("abbrev","replacement",abbrev)]], sub)) next
                        found_pos_list[p] <- regexpr(occur[[abbrev]],sub) + found_pos[p] + add_pos - 1
                        content[[c]][line] <- paste0(substr(content[[c]][line],1,found_pos_list[p]-1),
                                                     sub(occur[[abbrev]],
                                                         occur[[sub("abbrev","replacement",abbrev)]],
                                                         substr(content[[c]][line],found_pos_list[p],
                                                                nchar(content[[c]][line])),
                                                         ignore.case = FALSE))
                        add_pos <- add_pos + nchar(occur[[sub("abbrev","replacement",abbrev)]]) - nchar(occur[[abbrev]])
                        ## correct for sub of abbrev definition e.g. methotrexate (MTX (methotrexate))
                        len_diff <- nchar(content[[c]][line]) - 
                          nchar(gsub(paste0("(",occur[[sub("abbrev","replacement",abbrev)]],")"),
                                     paste0("(",occur[[abbrev]],")"),
                                     content[[c]][line] , fixed = TRUE))
                        add_pos <- add_pos - len_diff
                        content[[c]][line]  <- gsub(paste0("(",occur[[sub("abbrev","replacement",abbrev)]],")"),
                                                    paste0("(",occur[[abbrev]],")"),
                                                    content[[c]][line] , fixed = TRUE)
                      } ## end for (p in length(found_pos)){
                    } ## end for (line in found_lines){
                  } ## for (c in 1:length(content)){
                } ##if (!(occur[[abbrev]] %in% list_of_abbrevs)){
              } ## for (abbrev in c("abbrev_singular","abbrev_plural")) {
            } ## if (occur$res) {
          } ## end for (abbrev in c("abbrev_singular","abbrev_plural")) {
        } ## end for (nth in 1:length(word.txtline)) {
      } ## end replace abbreviations in content
    } ## end for each search word
  } ## if (filterwords.go == TRUE && integrity.indicator == TRUE)
  
  ## if search words were found replace the abbreviations in content
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      eval.abbrevs == TRUE && integrity.indicator == TRUE &&
      !search.word.table[i, "words"] == "") {
    txtcontent <- content[[1]]
    keeplayouttxtcontent <- content[[2]]
    
    ## html content per page
    for (pa in 3:length(content)) txthtmlcontent[[(pa-2)]] <- content[[pa]]
    
  } ## end replace abbreviations
  
  ## if no search words are detected in document
  if (filterwords.go == TRUE &&
      searchwords.go == FALSE &&
      integrity.indicator == TRUE){
    print_message <- paste0("No text with search words for \'",id,".pdf\' found.")
    out_msg <- c(out_msg, print_message)
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
    update_progress_info(print_message)
    ## write an empty file
    if (write.txt.doc.file == TRUE) {
      dir.create(paste0(out,"/excl_by_sw"), showWarnings = FALSE)
      utils::write.table("No text line with search word found.",
                         paste0(out,"/excl_by_sw/",id, "_no_txt_w_swds",
                                out.table.ext),
                         sep = out.table.separator, row.names = FALSE,
                         col.names = FALSE, na = "")
    }
  }  ## end if search words were present
  
  ## 5) Sort the html content ---------------------------------------
  if (filterwords.go == TRUE && integrity.indicator == TRUE) {
    ## add the top and left ##
    for (p in 1:length(txthtmlcontent)) {
      ## if page has one dimension
      if (!"left" %in% colnames(htmlcontent[[p]]))
        htmlcontent[[p]] <- cbind(htmlcontent[[p]],
                                  left = NA)
      if (!"top" %in% colnames(htmlcontent[[p]]))
        htmlcontent[[p]] <- cbind(htmlcontent[[p]],
                                  top = NA)
      ## add font size
      if (!"font_size" %in% colnames(htmlcontent[[p]]))
        htmlcontent[[p]] <- cbind(htmlcontent[[p]],
                                  font_size = NA)
    }
    for (p in 1:length(txthtmlcontent)){
      ## 5.1) Assign top and left values ------------------------------
      
      start <- grep("^<body>",htmlcontent[[p]][,1])[1] + 1
      if (is.na(start)) start <- 1
      end <- grep("^</body>",htmlcontent[[p]][,1])[1] - 1
      if (is.na(end)) {
        end <- nrow(htmlcontent[[p]])
        print_message <- paste0("Page ", p, " of \'", id, ".html\' was incompletely read.",
                                " This might leader to incomplete table extraction but does not",
                                " affect search word detection")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        print_message_short <- paste0("Page ", p, " of \'", id, ".html\' was incompletely read",
                                      " (does not affect txt detection though).")
        update_progress_info(print_message_short)
      }
      
      ## make the copy of htmlcontent for the sorting
      ## single row
      if (start == end){
        out.htmlcontent <- rbind(htmlcontent[[p]][start, ])
        out.txthtmlcontent <- txthtmlcontent[[p]][start]
      } else {
        out.htmlcontent <- htmlcontent[[p]][start:end, ]
        out.txthtmlcontent <- txthtmlcontent[[p]][start:end]
      }
      
      ## for each line
      for (line.number in 1:(end - start + 1)) {
        ## if content line
        if (grepl("[\"|;]font-size:",
                  out.htmlcontent[line.number, 1])) {
          ## get the left position
          pos.left.start <- regexpr("left:",
                                    out.htmlcontent[line.number, 1])[[1]] + 5
          out.htmlcontent[line.number, 1] <- iconv(out.htmlcontent[line.number, 1],"latin1","ASCII",sub="")
          pos.left.end <- regexpr("px",
                                  substr(out.htmlcontent[line.number, 1],
                                         pos.left.start,
                                         nchar(out.htmlcontent[line.number, 1])))[[1]] - 2 + pos.left.start
          left.value <- suppressWarnings(as.integer(substr(out.htmlcontent[line.number, 1], pos.left.start, pos.left.end)))
          if (is.na(left.value) && grepl(";vertical-align:baseline;",
                                         out.htmlcontent[line.number, 1])) {
            left.value <- out.htmlcontent[line.number-1, "left"]
          }
          
          out.htmlcontent[line.number, "left"] <- left.value
          
          ## get the top information
          pos.top.start <- regexpr("top:",
                                   out.htmlcontent[line.number, 1])[[1]] + 4
          out.htmlcontent[line.number, 1] <- iconv(out.htmlcontent[line.number, 1],"latin1","ASCII",sub="")
          pos.top.end <- regexpr("px",
                                 substr(out.htmlcontent[line.number, 1],
                                        pos.top.start,
                                        nchar(out.htmlcontent[line.number, 1])))[[1]] - 2 + pos.top.start
          top.value <- suppressWarnings(as.integer(substr(out.htmlcontent[line.number, 1], pos.top.start, pos.top.end)))
          if (is.na(top.value) && grepl(";vertical-align:baseline;",
                                        out.htmlcontent[line.number, 1])) {
            top.value <- out.htmlcontent[line.number-1, "top"]
          }
          
          out.htmlcontent[line.number, "top"] <- top.value
          
          ## get font_size information
          pos.fontsize.start <- regexpr("font-size:",
                                        out.htmlcontent[line.number, 1])[[1]] + 10
          out.htmlcontent[line.number, 1] <- iconv(out.htmlcontent[line.number, 1],"latin1","ASCII",sub="")
          pos.fontsize.end <- regexpr("px;",substr(out.htmlcontent[line.number, 1],
                                                   pos.fontsize.start, 
                                                   nchar(out.htmlcontent[line.number, 1])))[[1]] + pos.fontsize.start - 2
          fontsize.value <- substr(out.htmlcontent[line.number, 1],
                                   pos.fontsize.start, pos.fontsize.end)
          fontsize.value <- as.numeric(fontsize.value)
          out.htmlcontent[line.number, "font_size"] <- fontsize.value
        } else {
          ## if the line does not have position info
          out.htmlcontent[line.number, "left"] <- 0
          out.htmlcontent[line.number, "top"] <- 9999
          out.htmlcontent[line.number, "font_size"] <- 1
        }
      }
      
      
      ## 5.2) Sort lines according to top -------------------
      ## only the lines with top value
      lines.with.top.value <- out.htmlcontent[!is.na(out.htmlcontent[, "top"]), ]
      txtlines.with.top.value <- out.txthtmlcontent[!is.na(out.htmlcontent[, "top"])]
      ## if it is only one line no sorting necessary
      if (length(which(!is.na(out.htmlcontent[, "top"]))) > 1) {
        htmlorder <- order(as.numeric(lines.with.top.value[,"top"]), as.numeric(lines.with.top.value[,"left"]))
        lines.with.top.value.sorted <- lines.with.top.value[htmlorder, ]
        txtlines.with.top.value.sorted <- txtlines.with.top.value[htmlorder]
      } else {
        lines.with.top.value.sorted <- lines.with.top.value
        txtlines.with.top.value.sorted <- txtlines.with.top.value
      }
      out.htmlcontent[!is.na(out.htmlcontent[, "top"]), ] <- lines.with.top.value.sorted
      out.txthtmlcontent[!is.na(out.htmlcontent[, "top"])] <- txtlines.with.top.value.sorted
      
      ## make all 0 and 9999 to NA
      no.pos.info.lines <- (out.htmlcontent[, "top"] == 9999)
      out.htmlcontent[no.pos.info.lines, "left"] <- NA
      out.htmlcontent[no.pos.info.lines, "top"] <- NA
      out.htmlcontent[no.pos.info.lines, "font_size"] <- NA
      
      htmlcontent[[p]][start:end, ] <- out.htmlcontent
      txthtmlcontent[[p]][start:end] <- out.txthtmlcontent
    }  
  }
  
  ## 6) Extract Tables --------------------------------------------
  ## Explanation: The table detection is important to destinguish tables from text
  ## even if tables will not be exported they will not be a part of the sentence
  ## detection if search words.
  
  ## Use html to find table end line by adding 5 lines and then search for span id= change
  ## test if file has tables --> only process tables when table is present
  
  ## 6.1) Test if document has tables -------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE) {
    ## if there is no additional heading
    if (table.heading.words[1, "words"] == "") {
      tablestart.pos <- c(grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table )[0-99|MDCLXVI]+( )",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+$",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                               txthtmlcontent[[j]], ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+$",
                               txthtmlcontent[[j]], ignore.case = TRUE),
                          grep("^(\f|)(Table )[0-99|MDCLXVI]+( )",
                               txthtmlcontent[[j]], ignore.case = TRUE))
    } else {
      word.txtline.th <- NULL
      for (i in 1:nrow(table.heading.words)) {
        ## search for lines with searchword info
        ## [txtcontent] ##
        word <- table.heading.words[i, "words"]
        ignore.case.th <- table.heading.words[i,
                                              "ignore.case.th"]
        word.txtline.th <- c(word.txtline.th,
                             grep(word, txtcontent, ignore.case = ignore.case.th))
      }
      tablestart.pos <- c(word.txtline.th,
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table )[0-99|MDCLXVI]+( )",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+$",
                               txtcontent, ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                               txthtmlcontent[[j]], ignore.case = TRUE),
                          grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+$",
                               txthtmlcontent[[j]], ignore.case = TRUE),
                          grep("^(\f|)(Table )[0-99|MDCLXVI]+( )",
                               txthtmlcontent[[j]], ignore.case = TRUE))
    }
  }
  
  ## 6.2) Detect the table start positions by detecting headings ----------------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0) {
    
    ## Initialize master table for positions
    htmltablelines <- data.frame(page = NULL,
                                 tableheading = NULL, tablestart.pos = NULL,
                                 tablelastline = NULL, tableend.pos = NULL,
                                 legendlastline = NULL,
                                 legendend.pos = NULL, txtfirstline = NULL)
    txttablelines <- data.frame(page = NULL,
                                tableheading = NULL, tablestart.pos = NULL,
                                tablelastline = NULL, tableend.pos = NULL,
                                legendlastline = NULL,
                                legendend.pos = NULL, txtfirstline = NULL)
    
    ## go through pages
    for (j in 1:length(txthtmlcontent)) {
      
      ## if there is no additional heading
      if (table.heading.words[1, "words"] == "") {
        html.tablestart.pos <- c(grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                                      txthtmlcontent[[j]], ignore.case = TRUE),
                                 grep("^(\f|)(Table )[0-99|MDCLXVI]+( )(*)",
                                      txthtmlcontent[[j]], ignore.case = TRUE))
        ## search for tables with line break in title
        lb.html.tablestart.pos <- c(grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+$",
                                         txthtmlcontent[[j]], ignore.case = TRUE),
                                    grep("^(\f|)(Table )[0-99|MDCLXVI]+( )$",
                                         txthtmlcontent[[j]], ignore.case = TRUE))
        ## only look if the table is having also page
        if (length(splittxtcontent) >= j) {
          txt.tablestart.pos <- c(grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                                       splittxtcontent[[j]], ignore.case = TRUE),
                                  grep("^(\f|)(Table )[0-99|MDCLXVI]+( )(*)",
                                       splittxtcontent[[j]], ignore.case = TRUE))
          ## search for tables with line break in title
          lb.txt.tablestart.pos <- c(grep("^(\f|)(Table )[0-99|MDCLXVI]+$",
                                          splittxtcontent[[j]], ignore.case = TRUE),
                                     grep("^(\f|)(Table )[0-99|MDCLXVI]+( )$",
                                          splittxtcontent[[j]], ignore.case = TRUE))
        } else if (length(html.tablestart.pos) > 0) {
          if (nrow(htmltablelines) == 0) {
            htmltablelines <- data.frame(page = j,
                                         tableheading = NA, tablestart.pos = NA,
                                         tablelastline = NA, legendlastline = NA,
                                         legendend.pos = NA, txtfirstline = NA,
                                         detected.in = "htmlonly")
          } else {
            newrow <- htmltablelines[1, ]
            newrow <- NA
            newrow["page"] <- j
            newrow["detected.in"] <- "htmlonly"
            htmltablelines <- rbind(htmltablelines,
                                    newrow)
          }
        }
      } else  {
        ## if there is an additional heading
        txthtml.word.txtline.th <- NULL
        for (i in 1:nrow(table.heading.words)) {
          ## search for lines with searchword info
          ## [txtcontent] ##
          word <- table.heading.words[i, "words"]
          ignore.case.th <- table.heading.words[i, "ignore.case.th"]
          txthtml.word.txtline.th <- c(txthtml.word.txtline.th,
                                       grep(word, txthtmlcontent[[j]],
                                            ignore.case = ignore.case.th))
        }
        html.tablestart.pos <- c(txthtml.word.txtline.th,
                                 grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                                      txthtmlcontent[[j]], ignore.case = TRUE),
                                 grep("^(\f|)(Table )[0-99|MDCLXVI]+( )(*)",
                                      txthtmlcontent[[j]], ignore.case = TRUE))
        ## search for tables with line break in title
        lb.html.tablestart.pos <- c(grep("^(\f|)(Table )[0-99|MDCLXVI]+$",
                                         txthtmlcontent[[j]], ignore.case = TRUE),
                                    grep("^(\f|)(Table )[0-99|MDCLXVI]+( )$",
                                         txthtmlcontent[[j]], ignore.case = TRUE))
        ## only look if the table is having also page
        if (length(splittxtcontent) >= j) {
          splittxt.word.txtline.th <- NULL
          for (i in 1:nrow(table.heading.words)) {
            ## search for lines with searchword info
            ## [txtcontent] ##
            word <- table.heading.words[i, "words"]
            ignore.case.th <- table.heading.words[i, "ignore.case.th"]
            splittxt.word.txtline.th <- c(splittxt.word.txtline.th,
                                          grep(word, splittxtcontent[[j]],
                                               ignore.case = ignore.case.th))
          }
          txt.tablestart.pos <- c(splittxt.word.txtline.th,
                                  grep("^(\f|)(Table |Tab. )[0-99|MDCLXVI]+(\\.)(*)",
                                       splittxtcontent[[j]], ignore.case = TRUE),
                                  grep("^(\f|)(Table )[0-99|MDCLXVI]+( )(*)",
                                       splittxtcontent[[j]], ignore.case = TRUE))
          ## search for tables with line break in title
          lb.txt.tablestart.pos <- c(grep("^(\f|)(Table )[0-99|MDCLXVI]+$",
                                          splittxtcontent[[j]], ignore.case = TRUE),
                                     grep("^(\f|)(Table )[0-99|MDCLXVI]+( )$",
                                          splittxtcontent[[j]], ignore.case = TRUE))
        } else if (length(html.tablestart.pos) > 0) {
          if (nrow(htmltablelines) == 0) {
            htmltablelines <- data.frame(page = j,
                                         tableheading = NA, tablestart.pos = NA,
                                         tablelastline = NA, legendlastline = NA,
                                         legendend.pos = NA, txtfirstline = NA,
                                         detected.in = "htmlonly")
          } else {
            newrow <- htmltablelines[1, ]
            newrow <- NA
            newrow["page"] <- j
            newrow["detected.in"] <- "htmlonly"
            htmltablelines <- rbind(htmltablelines,
                                    newrow)
          }
        }
      }  ## end if there is (no) additional heading
      
      ## Look at tables in htmlcontent
      if (length(html.tablestart.pos) > 0) {
        for (i in 1:length(html.tablestart.pos)) {
          ## Removing the Table wording
          line <- txthtmlcontent[[j]][html.tablestart.pos[i]]
          tableheading <- substr(line,
                                 1, 50)
          ## replace the paranthesis so that they don't give
          ## problems for grep
          for (symbol in c("\\?","\\+","\\(", "\\)",
                           "\\[", "\\]", "\\/", "\\{",
                           "\\}")) {
            tableheading <- gsub(symbol,
                                 paste0("\\", symbol),
                                 tableheading)
          }
          ## Remove backreferences (\1) from tableheading
          tableheading <- remove_backref(tableheading)
          
          currentstartlines <- html.tablestart.pos[i]
          ## if the heading is on the site then add it to the
          ## table
          if (length(currentstartlines) != 0) {
            htmltablelines <- rbind(htmltablelines,
                                    data.frame(page = j,
                                               tableheading = tableheading,
                                               tablestart.pos = currentstartlines,
                                               tablelastline = NA,
                                               txtfirstline = NA,
                                               legendlastline = NA,
                                               legendend.pos = NA))
          }
        }
      }  ## end if length(html.tablestart.pos) > 0
      
      if (length(lb.html.tablestart.pos) > 0) {
        for (i in 1:length(lb.html.tablestart.pos)) {
          ## when +1 is empty
          s <- 1
          while (txthtmlcontent[[j]][lb.html.tablestart.pos[i] + s] == "") {
            s <- s + 1
            if (s > 5 || (lb.html.tablestart.pos[i] + s) == length(txthtmlcontent[[j]])) break
          }
          if ((lb.html.tablestart.pos[i] + s) == length(txthtmlcontent[[j]])) next
          ## Removing the Table wording
          line <- paste0(txthtmlcontent[[j]][lb.html.tablestart.pos[i]]," ",
                         txthtmlcontent[[j]][lb.html.tablestart.pos[i] + s])
          tableheading <- substr(line,
                                 1, 50)
          ## replace the paranthesis so that they don't give
          ## problems for grep
          for (symbol in c("\\?","\\+","\\(", "\\)",
                           "\\[", "\\]", "\\/", "\\{",
                           "\\}")) {
            tableheading <- gsub(symbol,
                                 paste0("\\", symbol),
                                 tableheading)
          }
          ## Remove backreferences (\1) from tableheading
          tableheading <- remove_backref(tableheading)
          
          currentstartlines <- lb.html.tablestart.pos[i]
          ## if the heading is on the site then add it to the
          ## table
          if (length(currentstartlines) != 0) {
            htmltablelines <- rbind(htmltablelines,
                                    data.frame(page = j,
                                               tableheading = tableheading,
                                               tablestart.pos = currentstartlines,
                                               tablelastline = NA,
                                               txtfirstline = NA,
                                               legendlastline = NA,
                                               legendend.pos = NA))
          }
        }
      }  ## end if there is a table on this page
      ## Look at tables in txtcontent

      if (length(txt.tablestart.pos) > 0) {
        for (i in 1:length(txt.tablestart.pos)) {
          ## Removing the Table wording
          line <- splittxtcontent[[j]][txt.tablestart.pos[i]]
          ## extract the first 50 characters
          tableheading <- substr(line,
                                 1, 50)
          ## replace the paranthesis so that they don't give
          ## problems for grep
          tableheading <- gsub("\\f", "", tableheading)
          for (symbol in c("\\?","\\+","\\(", "\\)",
                           "\\[", "\\]", "\\/", "\\{",
                           "\\}","\\*")) {
            tableheading <- gsub(symbol,
                                 paste0("\\", symbol),
                                 tableheading)
          }
          ## Remove backreferences (\1) from tableheading
          tableheading <- remove_backref(tableheading)
          
          ## from is where the heading was detected
          from <- txt.tablestart.pos[i]
          if (j > 1){
            for (p in 1:(j-1)){
              from <- from + length(splittxtcontent[[p]])
            }
          }
          
          ## to has to be the end of the page
          to <- 0
          if (j > 1){
            for (p in 1:j){
              to <- to + length(splittxtcontent[[p]])
            }
          }
          ##remove backreference from txtcontent
          txtcontent[from:to] <- remove_backref(txtcontent[from:to])
          
          ## heading
          currentstartlines <- grep(tableheading,
                                    txtcontent[from:to], fixed = TRUE)[1] + from - 1
          ## if the heading is on the site then add it to the
          ## table
          if (length(currentstartlines) != 0) {
            txttablelines <- rbind(txttablelines,
                                   data.frame(page = j,
                                              tableheading = tableheading,
                                              tablestart.pos = currentstartlines,
                                              tablelastline = NA, tableend.pos = NA,
                                              txtfirstline = NA,
                                              legendlastline = NA,
                                              legendend.pos = NA))
          }
        }
      }  ## end if there is a table on this page
      
      if (length(lb.txt.tablestart.pos) > 0) {
        for (i in 1:length(lb.txt.tablestart.pos)) {
          ## when +1 is empty
          s <- 1
          while (splittxtcontent[[j]][lb.txt.tablestart.pos[i] + s] == "") {
            s <- s + 1
            if (s > 5) break
          }
          ## Removing the Table wording
          line <- paste0(splittxtcontent[[j]][lb.txt.tablestart.pos[i]]," ",
                         splittxtcontent[[j]][lb.txt.tablestart.pos[i] + s])
          tableheading <- substr(line, 1, 50)
          ## replace the paranthesis so that they don't give
          ## problems for grep
          for (symbol in c("\\?","\\+","\\(", "\\)",
                           "\\[", "\\]", "\\/", "\\{",
                           "\\}","\\*")) {
            tableheading <- gsub(symbol,
                                 paste0("\\", symbol),
                                 tableheading)
          }
          ## Remove backreferences (\1) from tableheading
          tableheading <- remove_backref(tableheading)
          
          ## get the position of the line that has exactly the
          ## heading
          ## from is where the heading was detected
          from <- lb.txt.tablestart.pos[i]
          if (j > 1){
            for (p in 1:(j-1)){
              from <- from + length(splittxtcontent[[p]])
            }
          }
          
          ## to has to be the end of the page
          to <- 0
          if (j > 1){
            for (p in 1:j){
              to <- to + length(splittxtcontent[[p]])
            }
          }
          currentstartlines <- grep(splittxtcontent[[j]][lb.txt.tablestart.pos[i]],
                                    txtcontent[from:to][1]) + from - 1
          ## if the heading is on the site then add it to the
          ## table
          if (length(currentstartlines) != 0) {
            txttablelines <- rbind(txttablelines,
                                   data.frame(page = j,
                                              tableheading = tableheading,
                                              tablestart.pos = currentstartlines,
                                              tablelastline = NA, tableend.pos = NA,
                                              txtfirstline = NA,
                                              legendlastline = NA,
                                              legendend.pos = NA))
          }
        }
      }  ## end if there is a table on this page
    }  ## end go through each page j
    
    ## end if file has no tables (if function started
    ## then PDF file has to have search words)
  } else if (searchwords.go == TRUE && filterwords.go == TRUE &&
             integrity.indicator == TRUE && length(tablestart.pos) == 0) {
    outtable <- cbind(txtcontent, layout = "txt",
                      rownumber = 1:length(txtcontent))
    txtlines <- outtable[(outtable[, "layout"] == "txt"), ]
    txttablelines <- NULL
    htmltablelines <- NULL
  } else {
    outtable <- NULL
    txtlines <- NULL
    txttablelines <- NULL
    htmltablelines <- NULL
  }
  
  ## 6.3) Determine if tables were found in html, txt to both files ------------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0) {
    ## sort the htmltablelines table according to page
    ## and then tablestart.pos
    htmltablelines <- htmltablelines[with(htmltablelines,
                                          order(page, tablestart.pos)), ]
    ## add information about where table was found
    if (!"detected.in" %in% colnames(htmltablelines))
      htmltablelines <- cbind(htmltablelines,
                              detected.in = NA)
    if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
      ## sort the txttablelines table according to page
      ## and then tablestart.pos
      txttablelines <- txttablelines[with(txttablelines,
                                          order(page, tablestart.pos)), ]
      if (!"detected.in" %in% colnames(txttablelines)) {
        txttablelines <- cbind(txttablelines,
                               detected.in = NA)
      }
      
      ## go through each line of the tables and compare
      ## which tables are in the txt only
      for (txtrow in 1:nrow(txttablelines)) {
        txttablelines[txtrow, ] <- find_similar_row(originrow = txttablelines[txtrow, ],
                                                    targettablelines = htmltablelines,
                                                    relative.match.col = "tableheading",
                                                    output.for.originrow.only = "txtonly",
                                                    output.for.match = "txtandhtml",
                                                    output.column.name = "detected.in")$originrow
        htmltablelines <- find_similar_row(originrow = txttablelines[txtrow, ], 
                                           targettablelines = htmltablelines,
                                           relative.match.col = "tableheading",
                                           output.for.originrow.only = "txtonly",
                                           output.for.match = "txtandhtml",
                                           output.column.name = "detected.in")$targettablelines
      }
    }

    ## name all the rows that are only found in html
    if (nrow(htmltablelines[is.na(htmltablelines[, "detected.in"]), ]) > 0)
      htmltablelines[is.na(htmltablelines[, "detected.in"]), "detected.in"] = "htmlonly"
  }
  
  
  ## 6.4) Add positional value to tables -----
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0) {
    
    ## add tableend.pos if column does not exist
    if (!"tableend.pos" %in% colnames(htmltablelines)){
      htmltablelines <- cbind(htmltablelines, tableend.pos = NA)
    }
    htmltablelines <-  htmltablelines[!(is.na(htmltablelines[,"page"])),]
    for (i in 1:nrow(htmltablelines)) {
      position <- as.numeric(htmltablelines[i, "tablestart.pos"])
      p <- as.numeric(htmltablelines[i, "page"])
      ## if end of page or the txt ended
      npos <- position + 7
      ## test if the table is too short for detection
      if (nrow(htmlcontent[[p]]) < npos){
        outofbound <- TRUE
      } else {
        outofbound <- FALSE
      }
      if (outofbound == FALSE) {
        if (is.null(htmlcontent[[p]][npos, "top"])){
          notop <- TRUE 
        } else {
          if (is.na(htmlcontent[[p]][npos, "top"])){
            notop <- TRUE 
          } else {
            notop <- FALSE
          }
        }
      }
      
      if ((outofbound == TRUE) || (notop == TRUE)) {
        ## end is at the end of the page
        htmltablelines[i, "tableend.pos"] <- nrow(htmlcontent[[p]])
        htmltablelines[i, "detected.in"] <- "txtonly.notabledetected"
        if (nrow(txttablelines) > 0 &&
            ncol(txttablelines) > 0) {
          ## go through each line of the tables and compare
          ## which tables are in the txt only
          txttablelines <-  txttablelines[!(is.na(txttablelines[,"page"])),]
          txttablelines <- find_similar_row(originrow = htmltablelines[i,],
                                            targettablelines = txttablelines,
                                            relative.match.col = "tableheading",
                                            output.for.originrow.only = "error",
                                            output.for.match = "txtonly.notabledetected",
                                            output.column.name = "detected.in")$targettablelines
        }
        ## else start with +5
      } else {
        ## end of the this is the start line +5
        htmltablelines[i, "tableend.pos"] <- htmltablelines[i, "tablestart.pos"] + 5
      }
    }  ## end go through each row
  }
  
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0) {
    ## determine if top is constant (when not start point +1)
    ## do for each table row
    nexti <- FALSE
    htmltablelines <-  htmltablelines[!(is.na(htmltablelines[,"page"])),]
    for (i in 1:nrow(htmltablelines)) {
      ## ignore the lines with txtonly
      if ((htmltablelines[i, "detected.in"] == "txtonly.notabledetected") ||
          (htmltablelines[i, "detected.in"] == "txtonly")) {
        if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
          ## go through each line of the tables and compare
          ## which tables are in the txt only
          txttablelines <-  txttablelines[!(is.na(txttablelines[,"page"])),]
          txttablelines <- find_similar_row(originrow = htmltablelines[i, ],
                                            targettablelines = txttablelines,
                                            relative.match.col = "tableheading",
                                            output.for.originrow.only = "error",
                                            output.for.match = "txtonly.notabledetected",
                                            output.column.name = "detected.in")$targettablelines
        }
        next
      }
      
      ## 6.5) Detect how many columns by evaluating how many different left values ---------------
      
      ## set the current line pos and page for the while loop
      currentline.pos <- htmltablelines[i, "tableend.pos"]
      currentline.page <- htmltablelines[i, "page"]
      ## initialize out.left.list
      out.left.list <- NA
      all.left.found <- FALSE
      while (all.left.found == FALSE) {
        constant.value <- NULL
        
        while (is.null(constant.value)) {
          ## currentline.pos is either set before the loop or at the end
          currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
          
          ## get the top information
          oritop.value <- paste0("top:", currentline["top"], "px")
          
          ## go to the next line
          currentline.pos <- currentline.pos + 1
          ## set the start for of the next table
          if (!is.na(htmltablelines[i + 1, "tablestart.pos"]) &&
              htmltablelines[i + 1, "page"] == htmltablelines[i, "page"])
            nextstartpos <- htmltablelines[i + 1, "tablestart.pos"] else nextstartpos <- 999999
          ## test if end of page is reached
          currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
          
          ## get the top information
          top.value <- paste0("top:",
                              currentline["top"], "px")
          
          ## if beyond the end of the html page
          if ((currentline.pos > nrow(htmlcontent[[currentline.page]])) ||
              (top.value == "top:NApx")) {
            # htmltablelines[i, "detected.in"] <- "txtonly.notabledetected"
            if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
              # ## go through each line of the tables and compare
              # ## which tables are in the txt only
              # txttablelines <- txttablelines[!(is.na(txttablelines[,"page"])),]
              # txttablelines <- find_similar_row(originrow = htmltablelines[i, ],
              #                                   targettablelines = txttablelines,
              #                                   relative.match.col = "tableheading",
              #                                   output.for.originrow.only = "error",
              #                                   output.for.match = "txtonly.notabledetected",
              #                                   output.column.name = "detected.in")$targettablelines
              htmltablelines[i, "tableend.pos"] <- currentline.pos - 1
              htmltablelines[i, "legendend.pos"] <- currentline.pos - 1
              ## fill the txtcontent columns
              htmltablelines[i, "tablelastline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                                 "tableend.pos"])]
              htmltablelines[i, "legendlastline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                                  "legendend.pos"])]
              htmltablelines[i, "txtfirstline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                                "legendend.pos"]) + 1]
              
            }
            currentline.pos <- currentline.pos - 1
            all.left.found <- TRUE
            constant.value <- top.value
            # nexti <- TRUE
            break
            ## if tables overlap
          } else if (currentline.pos >= nextstartpos) {
            htmltablelines[i, "tableend.pos"] <- as.numeric(htmltablelines[i + 1, "tablestart.pos"]) - 1
            htmltablelines[i, "legendend.pos"] <- as.numeric(htmltablelines[i + 1, "tablestart.pos"]) - 1
            ## fill the txtcontent columns
            htmltablelines[i, "tablelastline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                               "tableend.pos"])]
            htmltablelines[i, "legendlastline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                                "legendend.pos"])]
            htmltablelines[i, "txtfirstline"] <- txthtmlcontent[[currentline.page]][as.numeric(htmltablelines[i, 
                                                                                                              "legendend.pos"]) + 1]
            currentline.pos <- as.numeric(htmltablelines[i + 1, "tablestart.pos"]) - 1
            
            if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
              txttablelines[i, "tableend.pos"] <- as.numeric(txttablelines[i + 1, "tablestart.pos"]) - 1
              txttablelines[i, "legendend.pos"] <- as.numeric(txttablelines[i + 1, "tablestart.pos"]) - 1
              txttablelines[i, "tablelastline"] <-  txtcontent[as.numeric(txttablelines[i, "tableend.pos"])]
              txttablelines[i, "legendlastline"] <- txtcontent[as.numeric(txttablelines[i, "legendend.pos"])]
              txttablelines[i, "txtfirstline"] <-  txtcontent[as.numeric(txttablelines[i, "legendend.pos"])+1]
            }
            nexti <- TRUE
            break
          }  ## end if
          
          if (oritop.value == top.value) {
            ## the constant value is the top
            constant.value <- top.value
          } else {
            ## use this currentline.pos as new start
            htmltablelines[i, "tableend.pos"] <- currentline.pos
          }
          
        }  ## end while is.null(constant.value)
        
        if (nexti == TRUE) break
        ## determine the left range ##
        
        ## go to min
        ind <- TRUE
        while (ind == TRUE) {
          currentline.pos <- currentline.pos - 1
          currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
          ind <- grepl(constant.value, currentline[1])
        }
        currentline.pos <- currentline.pos + 1
        currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
        
        left.list <- NULL
        ind <- TRUE
        while (ind == TRUE) {
          # get the left position
          left.value <- paste0("left:", currentline["left"], "px")
          left.list <- c(left.list,  left.value)
          currentline.pos <- currentline.pos + 1
          currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
          ind <- grepl(constant.value,
                       currentline[1])
        }
        
        ## test if left.list (column list) is complete ##
        if (all(left.list %in% out.left.list)) {
          ## if the left.list if complete, end the loop
          all.left.found <- TRUE
          break
        } else {
          ## if left.list is incomplete, add the current
          ## left.list
          out.left.list <- c(out.left.list[!is.na(out.left.list)],
                             left.list)
          out.left.list <- unique(out.left.list)
        }
      }  ## end of searching for all left values
      
      ## if only txt table to tables beside another table
      if (nexti == TRUE) {
        nexti <- FALSE
        next
      }
      
      ## 6.6) Determine the end of the table through the highest top value ---------------
      ## search for the max with all the left values ##
      
      ## if two tables are on the same page restrict lines
      if (!is.na(htmltablelines[i + 1, "tablestart.pos"]) &&
          htmltablelines[i + 1, "page"] == htmltablelines[i, "page"]){
        nexttablestartpos <- htmltablelines[i + 1, "tablestart.pos"]
      } else {
        nexttablestartpos <- nrow(htmlcontent[[currentline.page]])
      }
      
      ## make a toplist
      top.list <- NULL
      out.left.list <- out.left.list[out.left.list != "left:NApx"]
      
      ## if out.left.list is empty go to the next table
      if (length(out.left.list) == 0) {
        nexti <- FALSE
        next
      }
      for (left.item in out.left.list) {
        max.line <- max(grep(left.item,
                             htmlcontent[[currentline.page]][1:nexttablestartpos, 1]))
        currentline <- htmlcontent[[currentline.page]][max.line, ]
        top.value <- currentline["top"]
        top.list <- c(top.list, top.value)
      }
      ## choose the max top count that is at least double
      top.value.found <- FALSE
      unq.top.list <- unique(top.list)
      match.list <- NULL
      for (ti in 1:length(unq.top.list)) {
        curr.top.value <- unq.top.list[ti]
        match.list[ti] <- 0
        for (li in 1:length(out.left.list)) {
          match.one <- intersect(grep(out.left.list[li],
                                      htmlcontent[[currentline.page]][1:nexttablestartpos, 1]),
                                 grep(paste0("top:",curr.top.value, "px;"),
                                      htmlcontent[[currentline.page]][1:nexttablestartpos, 1]))
          ## when there was a match and the left.item is not the last in the list
          if (length(match.one) > 0){
            match.list[ti] <- match.list[ti] + 1
          }
        }
      }
      max.top.value <- suppressWarnings(max(strtoi(unq.top.list[match.list > 1])))
      
      ## if not one value is duplicated then go with max
      if (!any(match.list > 1))max.top.value <- max(strtoi(top.list))
      
      ## if not one value is duplicated then go with max
      htmltablelines[i, "tableend.pos"] <- max(grep(paste0("top:",
                                                           max.top.value, "px;"),
                                                    htmlcontent[[currentline.page]][,1]))
      currentline.pos <- htmltablelines[i, "tableend.pos"]
      currentline <- htmlcontent[[currentline.page]][currentline.pos, ]
      
      ## add the last line to the htmltablelines ## add
      ## the last line to the htmltablelines
      htmltablelines[i, "tablelastline"] <- txthtmlcontent[[currentline.page]][currentline.pos]
      ## save the end of the table
      htmltablelines[i, "tableend.pos"] <- currentline.pos
      
      ## add everything below the table ##
      
      ## 6.7) Detect the legend by extracting all lines with a lower font that than the table -----
      ## get the current font size information ##
      pos.fontsize.start <- regexpr("font-size:",
                                    currentline[1])[[1]] + 10
      pos.fontsize.end <- regexpr("px;",substr(currentline[1],
                                               pos.fontsize.start, 
                                               nchar(currentline[1])))[[1]] + pos.fontsize.start - 2
      current.fontsize <- substr(currentline[1],
                                 pos.fontsize.start, pos.fontsize.end)
      current.fontsize <- as.numeric(current.fontsize)
      
      ## add everything with smaller font size
      fontsize <- 0
      while (current.fontsize >= fontsize) {
        ##determine max font size in table
        ## if next line does not exist
        currentline.pos <- currentline.pos + 1
        if (currentline.pos > nrow(htmlcontent[[currentline.page]])) break
        currentline <- htmlcontent[[currentline.page]][currentline.pos, 1]
        ## get the font size information
        pos.fontsize.start <- regexpr("font-size:",
                                      currentline)[[1]] + 10
        if (pos.fontsize.start > 9){
          pos.fontsize.end <- regexpr("px;",substr(currentline,
                                                   pos.fontsize.start, 
                                                   nchar(currentline)))[[1]] + pos.fontsize.start - 2
        } else {
          break
        }
        str.fontsize <- substr(currentline,
                               pos.fontsize.start, pos.fontsize.end)
        ## when the next line has no font size
        if (str.fontsize == "") break
        fontsize <- as.numeric(str.fontsize)
        
      }
      
      currentline.pos <- currentline.pos - 1
      ## add the last line to the htmltablelines
      linecontent <- txthtmlcontent[[currentline.page]][currentline.pos]
      htmltablelines[i, "legendlastline"] <- linecontent
      htmltablelines[i, "legendend.pos"] <- currentline.pos
      
      ## add the last line to the htmltablelines
      if (currentline.pos + 1 > nrow(htmlcontent[[currentline.page]])) {
        htmltablelines[i, "txtfirstline"] <- ""
      } else {
        linecontent <- txthtmlcontent[[currentline.page]][currentline.pos + 1]
        htmltablelines[i, "txtfirstline"] <- linecontent
      }
    }  ## end for (i in 1:nrow(htmltablelines)) {
  }
  
  ## 6.8) Transfer information gained from html file to txt and keeplayouttxt tables-----------
  
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0) {
    
    htmltablelines <- htmltablelines[,
                                     c("page", "tableheading", "tablestart.pos",
                                       "tablelastline", "tableend.pos",
                                       "legendlastline", "legendend.pos",
                                       "txtfirstline", "detected.in")]
    if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
      ## adjust txttablelines ##
      txttablelines <- txttablelines[,c("page", "tableheading", "tablestart.pos",
                                        "tablelastline", "tableend.pos",
                                        "legendlastline", "legendend.pos",
                                        "txtfirstline", "detected.in")]
      
      ## convert into dataframe without levels
      txttablelines <- data.frame(lapply(txttablelines,
                                         as.character), stringsAsFactors = FALSE)
      txttablelines <-  txttablelines[!(is.na(txttablelines[,"page"])),]
      for (i in 1:nrow(txttablelines)) {
        ## adjust all tablestart.pos
        start <- as.numeric(txttablelines[i, "tablestart.pos"])
        ## get legendlastline from html
        matchingrow_from_html <- find_similar_row(originrow = txttablelines[i, ],
                                                  targettablelines = htmltablelines,
                                                  relative.match.col = "tableheading",
                                                  output.for.originrow.only = NA,
                                                  output.for.match = NA,
                                                  output.column.name = "detected.in")$targettablerow
        
        
        ## if table was not detected table is only one line
        if (txttablelines[i, "detected.in"] == "txtonly.notabledetected" ||
            txttablelines[i, "detected.in"] == "txtonly") {
          txttablelines[i, "tableend.pos"] <- txttablelines[i, "tablestart.pos"]
          txttablelines[i, "legendend.pos"] <- txttablelines[i, "tablestart.pos"]
          txttablelines[i, "tablelastline"] <-  txtcontent[as.numeric(txttablelines[i, "tableend.pos"])]
          txttablelines[i, "legendlastline"] <- txtcontent[as.numeric(txttablelines[i, "legendend.pos"])]
          txttablelines[i, "txtfirstline"] <-  txtcontent[as.numeric(txttablelines[i, "legendend.pos"]) + 1]
          
          next
        } else {
          ## get legendlastline from html
          txttablelines[i,"legendlastline"] <- as.character(matchingrow_from_html[["legendlastline"]])
          ## get txtfirstline from html
          txttablelines[i,"txtfirstline"] <- as.character(matchingrow_from_html[["txtfirstline"]])
          ## get tablelastline from html
          txttablelines[i,"tablelastline"] <- as.character(matchingrow_from_html[["tablelastline"]])
        }
        ## if txtfirstline is not empty (i.e. at the end of
        ## the page)
        ## if tables overlapped (i.e. legend.end was determined by table afterwards
        if (!is.na(txttablelines[i, "legendend.pos"])){
          ## adjust txtfirstline
          leg.pos <- as.numeric(txttablelines[i, "legendend.pos"])
          txt.pos <- leg.pos+1
          txttablelines[i, "legendlastline"] <- txtcontent[leg.pos]
          txttablelines[i, "tablelastline"] <- txtcontent[leg.pos]
          txttablelines[i, "txtfirstline"] <- txtcontent[txt.pos]
          
        } else if (!txttablelines[i, "txtfirstline"] == "") {
          ## adjust txtfirstline
          leg.pos <- suppressWarnings(min(grep(txttablelines[i, "legendlastline"],
                                               txtcontent[start:length(txtcontent)],
                                               fixed = TRUE))) + start - 1
          txt.pos <- suppressWarnings(min(grep(txttablelines[i, "txtfirstline"], 
                                               txtcontent[start:length(txtcontent)],
                                               fixed = TRUE))) + start - 1
          ## shorten the txtfirstline till hit is found
          while (leg.pos == Inf && txt.pos == Inf &&
                 txttablelines[i, "txtfirstline"] != "") {
            txttablelines[i, "txtfirstline"] <- substr(txttablelines[i, "txtfirstline"],1,
                                                        (nchar(txttablelines[i, "txtfirstline"]) - 1))
            txt.pos <- suppressWarnings(min(grep(txttablelines[i, "txtfirstline"],
                                                 txtcontent[start:length(txtcontent)],
                                                 fixed = TRUE))) + start - 1
          }
          if (txttablelines[i, "txtfirstline"] == "" || txt.pos == Inf)
            txt.pos <- start + 2
          ## if min of integer(0) then Inf
          if (leg.pos == Inf)
            leg.pos <- start + 1
          txttablelines[i, "legendend.pos"] <- max(leg.pos,
                                                   txt.pos - 1)
          ## reverse order detection
          leg.pos.final <- as.numeric(txttablelines[i, "legendend.pos"])
          rev.pos.search.item <- txttablelines[i, "tablelastline"]
          for (symbol in c("\\+", "\\*","\\?",
                           "\\/", "\\(", "\\)", "\\[",
                           "\\]", "\\{", "\\}")) {
            rev.pos.search.item <- gsub(symbol,
                                        paste0("\\", symbol),
                                        rev.pos.search.item)
          }
          rev.pos <- suppressWarnings(min(grep(rev.pos.search.item,
                                               rev(txtcontent[1:leg.pos.final]))))
          if (rev.pos == Inf)
            rev.pos <- 1
          txttablelines[i, "tableend.pos"] <- leg.pos.final - rev.pos + 1
        } else {
          ## reverse order detection
          leg.pos.final <- suppressWarnings(min(grep(txttablelines[i, "legendlastline"],
                                                     txtcontent[start:length(txtcontent)],
                                                     fixed = TRUE))) + start - 1
          if (leg.pos.final == Inf)
            leg.pos.final <- start + 1
          txttablelines[i, "legendend.pos"] <- leg.pos.final
          rev.pos.search.item <- txttablelines[i, "tablelastline"]
          for (symbol in c("\\+", "\\*","\\?",
                           "\\/", "\\(", "\\)", "\\[",
                           "\\]", "\\{", "\\}")) {
            rev.pos.search.item <- gsub(symbol,
                                        paste0("\\", symbol),
                                        rev.pos.search.item)
          }
          rev.pos <- suppressWarnings(min(grep(rev.pos.search.item,
                                               rev(txtcontent[1:leg.pos.final]))))
          if (rev.pos == Inf)
            rev.pos <- 2
          txttablelines[i, "tableend.pos"] <- leg.pos.final - rev.pos + 1
        }
      }  ## end
      
      ## adjust keeplayouttxttablelines ##
      
      keeplayouttxttablelines <- htmltablelines[,
                                                c("page", "tableheading", "tablestart.pos",
                                                  "tablelastline", "tableend.pos",
                                                  "legendlastline", "legendend.pos",
                                                  "txtfirstline", "detected.in")]
      ## convert into dataframe without levels
      keeplayouttxttablelines <- data.frame(lapply(keeplayouttxttablelines,
                                                   as.character), stringsAsFactors = FALSE)
      for (i in 1:nrow(keeplayouttxttablelines)) {
        
        ## adjust tablestart.pos
        tabhead <- keeplayouttxttablelines[i, "tableheading"]
        for (symbol in c("\\+", "\\*","\\?",
                         "\\/", "\\(", "\\)", "\\[",
                         "\\]", "\\{", "\\}")) {
          tabhead <- gsub(symbol,"\\", tabhead)
        }
        ## Remove backreferences (\1) from tableheading
        tabhead <- remove_backref(tabhead)
        
        start <- grep(as.character(tabhead), keeplayouttxtcontent)[1]
        
        keeplayouttxttablelines[i, "tableheading"] <- tabhead
        trimed <- tabhead
        while (is.na(start)) {
          headinglength <- nchar(as.character(trimed))
          trimed <- substr(as.character(trimed),1,
                            (headinglength - 1))
          headinglength <- nchar(as.character(trimed))
          lastchar <- substr(as.character(trimed),
                             headinglength, headinglength)
          while (lastchar == "\\"){
            trimed <- substr(as.character(trimed),1
                              (headinglength - 1))
            headinglength <- nchar(as.character(trimed))
            lastchar <- substr(as.character(trimed),
                               headinglength, headinglength)
          }
          start <- grep(trimed, keeplayouttxtcontent)[1]
        }
        keeplayouttxttablelines[i, "tablestart.pos"] <- start
        ## if table keeplayouttxttablelines not detected
        ## table is only one line
        if (keeplayouttxttablelines[i, "detected.in"] == "txtonly.notabledetected") {
          keeplayouttxttablelines[i, "tableend.pos"] <- keeplayouttxttablelines[i, "tablestart.pos"]
          keeplayouttxttablelines[i, "legendend.pos"] <- keeplayouttxttablelines[i, "tablestart.pos"]
          next
        }
        ## if keeplayouttxtfirstline is not empty (i.e. at
        ## the end of the page)
        if (!is.na(keeplayouttxttablelines[i, "legendend.pos"])){
          ## adjust txtfirstline
          leg.pos <- as.numeric(keeplayouttxttablelines[i, "legendend.pos"])
          txt.pos <- leg.pos+1
          keeplayouttxttablelines[i, "legendlastline"] <- keeplayouttxtcontent[leg.pos]
          keeplayouttxttablelines[i, "tablelastline"] <- keeplayouttxtcontent[leg.pos]
          keeplayouttxttablelines[i, "txtfirstline"] <- keeplayouttxtcontent[leg.pos+1]
          
        } else if (!keeplayouttxttablelines[i, "txtfirstline"] == "") {
          ## adjust keeplayouttxtfirstline
          leg.pos <- suppressWarnings(min(grep(keeplayouttxttablelines[i, "legendlastline"], 
                                               keeplayouttxtcontent[start:length(keeplayouttxtcontent)],
                                               fixed = TRUE))) + start - 1
          
          keeplayouttxt.pos <- suppressWarnings(min(grep(keeplayouttxttablelines[i, "txtfirstline"],
                                                         keeplayouttxtcontent[start:length(keeplayouttxtcontent)],
                                                         fixed = TRUE))) + start - 1
          ## shorten the keeplayouttxtfirstline till hit is
          ## found
          while (leg.pos == Inf && keeplayouttxt.pos ==
                 Inf && keeplayouttxttablelines[i,"txtfirstline"] != "") {
            keeplayouttxttablelines[i, "txtfirstline"] <- substr(keeplayouttxttablelines[i, "txtfirstline"],1,
                                                                  (nchar(keeplayouttxttablelines[i, "txtfirstline"]) - 1))
            keeplayouttxt.pos <- suppressWarnings(min(grep(keeplayouttxttablelines[i, "txtfirstline"],
                                                           keeplayouttxtcontent[start:length(keeplayouttxtcontent)],
                                                           fixed = TRUE))) + start - 1
          }
          if (keeplayouttxttablelines[i, "txtfirstline"] == "" ||
              keeplayouttxt.pos == Inf)
            keeplayouttxt.pos <- start + 2
          ## if min of integer(0) then Inf
          if (leg.pos == Inf)
            leg.pos <- start + 1
          keeplayouttxttablelines[i, "legendend.pos"] <- max(leg.pos, keeplayouttxt.pos - 1)
          ## reverse order detection
          leg.pos.final <- as.numeric(keeplayouttxttablelines[i, "legendend.pos"])
          rev.pos.search.item <- gsub("/", "_",
                                      keeplayouttxttablelines[i, "tablelastline"])
          for (symbol in c("\\+", "\\*","\\?",
                           "\\/", "\\(", "\\)", "\\[",
                           "\\]", "\\{", "\\}")) {
            rev.pos.search.item <- gsub(symbol,
                                        paste0("\\", symbol),
                                        rev.pos.search.item)
          }
          rev.pos.search.item <- gsub("\\(",
                                      "", rev.pos.search.item)
          rev.pos.search.item <- gsub("\\)",
                                      "", rev.pos.search.item)
          rev.pos <- suppressWarnings(min(grep(rev.pos.search.item,
                                               rev(keeplayouttxtcontent[1:leg.pos.final]))))
          if (rev.pos == Inf)
            rev.pos <- 1
          keeplayouttxttablelines[i, "tableend.pos"] <- leg.pos.final - rev.pos + 1
        } else {
          ## reverse order detection
          leg.pos.final <- suppressWarnings(min(grep(keeplayouttxttablelines[i, "legendlastline"],
                                                     keeplayouttxtcontent[start:length(keeplayouttxtcontent)],
                                                     fixed = TRUE))) + start - 1
          if (leg.pos.final == Inf)
            leg.pos.final <- start + 1
          keeplayouttxttablelines[i, "legendend.pos"] <- leg.pos.final
          rev.pos.search.item <- gsub("/",
                                      "_", keeplayouttxttablelines[i, "tablelastline"])
          for (symbol in c("\\+", "\\*","\\?",
                           "\\/", "\\(", "\\)", "\\[",
                           "\\]", "\\{", "\\}")) {
            rev.pos.search.item <- gsub(symbol,
                                        paste0("\\", symbol),
                                        rev.pos.search.item)
          }
          rev.pos.search.item <- gsub("\\(",
                                      "", rev.pos.search.item)
          rev.pos.search.item <- gsub("\\)",
                                      "", rev.pos.search.item)
          rev.pos <- suppressWarnings(min(grep(rev.pos.search.item,
                                               rev(keeplayouttxtcontent[1:leg.pos.final]))))
          if (rev.pos == Inf)
            rev.pos <- 2
          keeplayouttxttablelines[i, "tableend.pos"] <- leg.pos.final - rev.pos + 1
        }
      }
    } #end if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
  }
  
  ## 6.9) Assign table, legend or txt to the each html and txtline ----------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0) {
    
    
    ##take care of overlapping tables HTML 
    if (nrow(htmltablelines) > 1){
      for (i in 1:(nrow(htmltablelines)-1)) {
        ##if the tables are on the same page
        if (as.numeric(htmltablelines[i, "page"]) == as.numeric(htmltablelines[(i+1), "page"]) &&
            htmltablelines[i, "detected.in"] == "txtandhtml" && htmltablelines[(i+1), "detected.in"] == "txtandhtml"){
          ##if the tables overlap
          ##TODEL
          print(htmltablelines)
          if (as.numeric(htmltablelines[i, "legendend.pos"]) >= as.numeric(htmltablelines[(i+1), "tablestart.pos"])){
            ##adjust legendend and tableend
            htmltablelines[i, "legendend.pos"] <- (as.numeric(htmltablelines[(i+1), "tablestart.pos"]) - 1)
            htmltablelines[i, "tableend.pos"] <- (as.numeric(htmltablelines[(i+1), "tablestart.pos"]) - 1)
            current_page <- htmltablelines[i, "page"]
            htmltablelines[i, "tablelastline"] <- txthtmlcontent[[current_page]][as.numeric(htmltablelines[i, 
                                                                                                           "tableend.pos"])]
            htmltablelines[i, "legendlastline"] <- txthtmlcontent[[current_page]][as.numeric(htmltablelines[i, 
                                                                                                            "legendend.pos"])]
            htmltablelines[i, "txtfirstline"] <- htmltablelines[(i+1), "txtfirstline"]
          }
        }
      }
    }
    
    
    ##take care of overlapping tables HTML
    if (nrow(txttablelines) > 1){
      for (i in 1:(nrow(txttablelines)-1)) {
        ##if the tables are on the same page
        if (as.numeric(txttablelines[i, "page"]) == as.numeric(txttablelines[(i+1), "page"]) &&
            txttablelines[i, "detected.in"] == "txtandhtml" && txttablelines[(i+1), "detected.in"] == "txtandhtml"){
          ##if the tables overlap
          if (as.numeric(txttablelines[i, "legendend.pos"]) >= as.numeric(txttablelines[(i+1), "tablestart.pos"])){
            ##adjust legendend and tableend
            txttablelines[i, "legendend.pos"] <- (as.numeric(txttablelines[(i+1), "tablestart.pos"]) - 1)
            txttablelines[i, "tableend.pos"] <- (as.numeric(txttablelines[(i+1), "tablestart.pos"]) - 1)
            current_page <- txttablelines[i, "page"]
            txttablelines[i, "tablelastline"] <- txtcontent[as.numeric(txttablelines[i, 
                                                                                     "tableend.pos"])]
            txttablelines[i, "legendlastline"] <- txtcontent[as.numeric(txttablelines[i, 
                                                                                      "legendend.pos"])]
            txttablelines[i, "txtfirstline"] <- txttablelines[(i+1), "txtfirstline"]
          }
        }
      }
    }
    
    ## Assign table, legend or txt to the each htmlline
    ## initialize the output table
    htmltables <- list()
    ## export htmltables
    if (nrow(htmltablelines) > 0) {
      for (i in 1:nrow(htmltablelines)) {
        if ((htmltablelines[i, "detected.in"] == "txtonly.notabledetected") ||
            (htmltablelines[i, "detected.in"] == "txtonly")) {
          htmltables[[i]] <- as.character(htmltablelines[i, "tableheading"])
          names(htmltables)[i] <- as.character(htmltablelines[i, "tableheading"])
          print_message <- paste0("The following table was detected but not processable for extraction: ",
                                  htmltablelines[i, "tableheading"])
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          ## create empty table
          if (write.tab.doc.file == TRUE) {
            ## write the mock table
            tableheader <- as.character(htmltablelines[i, "tableheading"])
            tablenumber <- suppressWarnings(as.numeric( sub("\\D*(\\d+).*", "\\1", tableheader)))
            if (is.na(tablenumber)){
              tablenum <- "table"
            } else{
              tablenum <- paste0("table",tablenumber)
            }
            
            ## write table file
            dir.create(paste0(out,"/tables"), showWarnings = FALSE)
            ## if nrow(htmltablelines) is double digits convert i
            if (nrow(htmltablelines) > 99){
              i_num <- paste0("00",as.character(i))
            } else if (nrow(htmltablelines) > 9) {
              i_num <- paste0("0",as.character(i))
            } else{
              i_num <- as.character(i)
            }
            
            outputtable.name <- paste0(out,gsub(" ","_",paste0("/tables/",id,
                                                    "_#", i_num, "_",
                                                    tablenum)))
            if (nchar(outputtable.name) > 255) {
              print_message <- paste0("The file path of ",paste0(outputtable.name,
                                                                 out.table.ext),
                                      " might be too long to be read by some programs. Consider using a shorter output path.")
              out_msg <- c(out_msg, print_message)
              if (verbose) cat(utils::tail(out_msg,1), sep="\n")
              update_progress_info(paste0(outputtable.name,
                                          out.table.format,
                                          " file path maybe too long."))
              utils::write.table(print_message, file = paste0(outputtable.name,
                                                              out.table.ext),
                                 sep = out.table.separator,
                                 row.names = FALSE, col.names = FALSE,
                                 na = "")
            } else {
              utils::write.table(print_message, file = paste0(outputtable.name,
                                                              out.table.ext),
                                 sep = out.table.separator,
                                 row.names = FALSE, col.names = FALSE,
                                 na = "")
            }
          } ##end if if (write.tab.doc.file == TRUE) {
          next
        }
        ## save the current page
        p <- htmltablelines[i, "page"]
        ## copy the page as currenttable
        currenttable <- cbind(txthtmlcontent[[p]],
                              left = htmlcontent[[p]][, "left"],
                              top = htmlcontent[[p]][, "top"],
                              font_size = htmlcontent[[p]][, "font_size"],
                              layout = NA)
        ## add legend info to layout
        currenttable[htmltablelines[i, "tableend.pos"]:htmltablelines[i, "legendend.pos"], "layout"] <- "legend"
        ## add table info to layout
        currenttable[htmltablelines[i, "tablestart.pos"]:htmltablelines[i, "tableend.pos"], "layout"] <- "table"
        outtable <- currenttable[htmltablelines[i, "tablestart.pos"]:htmltablelines[i, "legendend.pos"], ]
        htmltables[[i]] <- outtable
        names(htmltables)[i] <- as.character(htmltablelines[i, "tableheading"])
      }
    } else {
      htmltables <- list()
    }
    
    ## assign table, legend or txt to the each txtline
    ##
    
    ## initialize outtable
    outtable <- cbind(txtcontent, layout = NA,
                      rownumber = 1:length(txtcontent))
    ## export htmltables
    if (nrow(txttablelines) > 0 && ncol(txttablelines) > 0) {
      for (i in 1:nrow(txttablelines)) {
        ## add legend info to layout
        outtable[txttablelines[i, "tableend.pos"]:txttablelines[i, "legendend.pos"], "layout"] <- "legend"
        ## add table info to layout
        outtable[txttablelines[i, "tablestart.pos"]:txttablelines[i, "tableend.pos"], "layout"] <- "table"
      }
    }
    outtable[is.na(outtable[, "layout"]),
             "layout"] <- "txt"
    
    txtlines <- outtable[(outtable[, "layout"] == "txt"), ]
  } else if (filterwords.go == TRUE &&
             integrity.indicator == TRUE) {
    print_message <- paste0("\'",id,".pdf\' has no tables that could be detected")
    out_msg <- c(out_msg, print_message)
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
    update_progress_info(print_message)
    
    if (write.tab.doc.file == TRUE) {
      dir.create(paste0(out,"/no_tab"), showWarnings = FALSE)
      utils::write.table("no table found",
                         paste0(out,"/no_tab/",id, "_no_tab_found",
                                out.table.ext),
                         sep = out.table.separator,
                         row.names = FALSE, col.names = FALSE,
                         na = "")
    }
    
    ## all lines are txt lines (no table or legend)
    if (!is.null(txttablelines)){
      if (nrow(txttablelines) > 0){
        txttablelines[, "detected.in"] <- "txtonly"
      }
    }
    htmltablelines <- NULL
    outtable <- cbind(txtcontent, layout = "txt",
                      rownumber = 1:length(txtcontent))
    txtlines <- outtable[(outtable[, "layout"] == "txt"), ]
  }
  
  ## 7.1) Second search of search words in htmllines -----------------------------------------------
  if (filterwords.go == TRUE &&
      integrity.indicator == TRUE &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0 &&
      (whattoextr == "tabandtxt" ||
       whattoextr == "tab" ||
       whattoextr == "table" ||
       whattoextr == "txtandtab")) {
    ## search for tables with search word in html to
    ## differentiate table content from sentences ##
    ## if column does not exist
    if (!"searchwords.found" %in% colnames(htmltablelines)) htmltablelines <- cbind(htmltablelines,
                                                                                    searchwords.found = NA)
    word.table <- NULL
    ## start search only when search words are in the
    ## list
    if (!is.na(search.word.table[1, "words"])) {
      if (search.word.table[1, "words"] != "") {
        pdf_searchwords <- NULL
        ## go through each word
        for (i in 1:nrow(search.word.table)) {
          ## search for tables with searchword ##
          word <- search.word.table[i, "words"]
          ignore.case.sw <- search.word.table[i,
                                         "ignore.case.sw"]
          word.table <- c(word.table,
                          grep(word, htmltables, ignore.case = ignore.case.sw))
          word.table <- unique(word.table)
          ## count search words in tables
          if (ignore.case.sw == TRUE){
            ic <- "ic"
          } else {
            ic <- "nic"
          }
          if (regex.sw == TRUE){
            reg <- "regex"
          } else {
            reg <- "nregex"
          }
          pdf_searchwords <- c(pdf_searchwords, paste0("SW_",ic,"_",reg,":",word))
          current_word.tabpos.sw <- NULL
          for (t in word.table){
            ## skip the table if it wasn't extracted
            if (length(nrow(htmltables[[t]])) < 1){
              next
            }
            htmltablelines_w_sw <- grep(word, htmltables[[t]][,1], ignore.case = ignore.case.sw)
            for (li in htmltablelines_w_sw){
              pos_of_words <-  gregexpr(word, htmltables[[t]][li,1], ignore.case = ignore.case.sw)[[1]]
              if (pos_of_words == -1){
                pos_of_words <- NULL
              }
              current_word.tabpos.sw <- c(current_word.tabpos.sw,
                                          pos_of_words)
            }
          }
          pdf_searchword_times <- c(pdf_searchword_times, length(current_word.tabpos.sw))
          pdf_searchword_names <- c(pdf_searchword_names, word)
          pdf_searchword_total <- sum(pdf_searchword_times)
          sw_in_tab_counted <- TRUE
          search.word.category_total <- NULL
          if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
            for (swc in 1:length(unique(search.word.categories))){
              search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
            }
          }
        }
        
        ## choose tables with search word
        processed.tables <- htmltables[word.table]
        names(processed.tables) <- names(htmltables)[word.table]
        if (length(word.table) > 0) {
          htmltablelines[word.table, "searchwords.found"] <- "YES"
          htmltablelines[-word.table, "searchwords.found"] <- "NO"
        } else {
          htmltablelines["searchwords.found"] <- "NO"
        }
        
        ## add all tables that have the word 'continue' in
        ## it and follow a table with the search word
        n <- 1
        while (n < nrow(htmltablelines)) {
          ## if current row is having search word and next
          ## table not
          if ((htmltablelines[n, "searchwords.found"] == "YES") && (htmltablelines[n + 1, 
                                                                                   "searchwords.found"] == "NO")) {
            ## if next table includes 'continue'
            if (grepl("continue", htmltables[n + 1], ignore.case = "TRUE")) {
              htmltablelines[n + 1, "searchwords.found"] <- "YES"
            }  ## else it remains NO
          }
          n <- n + 1
        }
      } else{
        processed.tables <- htmltables
        names(processed.tables) <- names(htmltables)
        pdf_searchword_total <- NA
        pdf_searchword_times <- NA
        pdf_searchwords <- "search_word_list"
        search.word.category_total <- NULL
        if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
          for (swc in 1:length(unique(search.word.categories))){
            search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
          }
        }
      }
      
    } else {
      processed.tables <- htmltables
      names(processed.tables) <- names(htmltables)
      pdf_searchword_times <- NA
      pdf_searchword_total <- NA
      pdf_searchwords <- "search_word_list"
      search.word.category_total <- NULL
      if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
        for (swc in 1:length(unique(search.word.categories))){
          search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
        }
      }
    }
  }
  
  
  ## 7.2) Export TABLE only if tab function was called --------------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0 &&
      (whattoextr == "tabandtxt" ||
       whattoextr == "tab" ||
       whattoextr == "table" ||
       whattoextr == "txtandtab")) {
    
    ## export tables ##
    
    ## sort processed tables
    
    ## look if any table with search word was found
    if (!length(processed.tables) == 0) {
      processed.tables <- processed.tables[order(names(processed.tables))]
      for (t in 1:length(processed.tables)) {
        ## when table was not detected then skip export
        if (is.null(nrow(processed.tables[[t]]))) {
          next
        } else if (nrow(processed.tables[[t]]) < 2) {
          ## write the table
          tableheader <- names(processed.tables)[[t]]
          tablenumber <- suppressWarnings(as.numeric( sub("\\D*(\\d+).*", "\\1", tableheader)))
          if (is.na(tablenumber)){
            tablenum <- "table"
          } else{
            tablenum <- paste0("table",tablenumber)
          }
          dir.create(paste0(out,"/tables"), showWarnings = FALSE)
          ## if nrow(htmltablelines) is double digits convert i
          if (nrow(htmltablelines) > 99){
            t_num <- paste0("00",as.character(t))
          } else if (nrow(htmltablelines) > 9) {
            t_num <- paste0("0",as.character(t))
          } else{
            t_num <- as.character(t)
          }
          
          outputtable.name <- paste0(out,gsub(" ","_",paste0("/tables/",id,
                                                  "_#", t_num, "_",
                                                  tablenum)))
          if (nchar(outputtable.name) > 255) {
            print_message <- paste0("The file path of ",paste0(outputtable.name,
                                                               out.table.ext),
                                    " might be too long to be read by some programs. Consider using a shorter output path.")
            out_msg <- c(out_msg, print_message)
            if (verbose) cat(utils::tail(out_msg,1), sep="\n")
            update_progress_info(paste0(outputtable.name,
                                        out.table.format,
                                        " file path maybe too long."))
            utils::write.table(names(processed.tables)[[t]], file = paste0(outputtable.name,
                                                                           out.table.ext),
                               sep = out.table.separator,
                               row.names = FALSE, col.names = FALSE,
                               na = "")
          } else {
            utils::write.table(names(processed.tables)[[t]], file = paste0(outputtable.name,
                                                                           out.table.ext),
                               sep = out.table.separator,
                               row.names = FALSE, col.names = FALSE,
                               na = "")
          }
          next
        }
        
        for (o in c("left", "top")) {
          ## save the range of orient values
          change.table <- processed.tables[[t]][order(strtoi(processed.tables[[t]][, o]), strtoi(processed.tables[[t]][, "font_size"])), ]
          orient.range <- sort(strtoi(unique(processed.tables[[t]][, o])), decreasing = FALSE)
          ## combine if deviation is first value stays the
          ## same
          # change.table <- data.frame(from = orient.range[1],
          #                            to = orient.range[1])
          
          ## only combine for left values and when it is a table with more than 1 column (orient.range > 1)
          if (o == "left" && length(orient.range) > 1) {
            for (i in 2:nrow(change.table)) {
              
              if (dev_x > 0 &&
                  (strtoi(change.table[i, o]) - dev_x) <= strtoi(change.table[i - 1, o])) {
                change.table[i, o] <- change.table[i - 1, o]
              }
            }  ## end for
          } else if (o == "top" && length(orient.range) > 1) {
            for (i in 2:nrow(change.table)) {
              ## check if dev_y is dynamic (adjusted to font_size) or static
              if (dev_y == 9999 && change.table[i, o] != change.table[i - 1, o]){
                if ((strtoi(change.table[i, o]) - strtoi(change.table[i - 1, "font_size"])) <= strtoi(change.table[i - 1, o])){
                  change.table[i, o] <- change.table[i - 1, o]
                }
              } else if (dev_y > 0 && change.table[i, o] != change.table[i - 1, o]){
                if (strtoi(change.table[i, o])  - dev_y <= strtoi(change.table[i - 1, o])){
                  change.table[i, o] <- change.table[i - 1, o]
                }
              }
            }  ## end for
          }
          ## change the pixel values to index
          orient.range <- sort(strtoi(unique(change.table[, o])), decreasing = FALSE)
          for (index in 1:length(orient.range)) {
            pixel_value <- as.character(orient.range[index])
            change.table[grep(paste0("^",pixel_value,"$"),change.table[, o]),o] <- index
          }
          
          ## save the processed orient value
          processed.tables[[t]] <- change.table[order(strtoi(change.table[,"left"]),strtoi(change.table[,"top"])),]
          
        }  ## end orient
        for (l in 1:nrow(processed.tables[[t]])) {
          ## replace abbreviations
          processed.tables[[t]][l, 1] <- sub(" \\(","",paste0(gsub("^.*\\$\\*","",
                                                                   paste0(" (",strsplit(as.character(processed.tables[[t]][l, 1])," \\(")[[1]])),collapse = ""))
        }
        
        ## make table fuse lines that have the same
        ## coordinates
        new.processed.table <- processed.tables[[t]]
        li <- 1
        while (li <= nrow(new.processed.table)) {
          ## find rows with same left value
          same.left <- which(new.processed.table[, "left"] %in% new.processed.table[li, "left"])
          ## find rows with same top value
          same.top <- which(new.processed.table[, "top"] %in% new.processed.table[li, "top"])
          ## this gives at least the current line and the
          ## duplicated
          duplicated.lines <- intersect(same.left,
                                        same.top)
          ## change the currentline when lines with same
          ## coordinates are present
          if (length(duplicated.lines) >
              1) {
            ## make a new current line
            new.processed.table[li, 1] <- paste(unlist(new.processed.table[c(duplicated.lines), 1]), 
                                                collapse = " ")
            ## delete the other lines
            new.processed.table <- new.processed.table[-duplicated.lines[-1], ]
          }
          li <- li + 1
        }
        processed.tables[[t]] <- new.processed.table
        
        output.table <- data.frame(matrix(NA,
                                          nrow = max(strtoi(processed.tables[[t]][, "top"])),
                                          ncol = max(strtoi(processed.tables[[t]][, "left"]))))
        for (l in 1:nrow(processed.tables[[t]])) {
          output.table[strtoi(processed.tables[[t]][l, "top"]),
                       strtoi(processed.tables[[t]][l, "left"])] <- processed.tables[[t]][l, 1]
        }
        ## write the table
        tableheader <- names(processed.tables)[[t]]
        tablenumber <- suppressWarnings(as.numeric( sub("\\D*(\\d+).*", "\\1", tableheader)))
        if (is.na(tablenumber)){
          tablenum <- "table"
        } else{
          tablenum <- paste0("table",tablenumber)
        }
        ## write table file
        dir.create(paste0(out,"/tables"), showWarnings = FALSE)
        ## if nrow(htmltablelines) is double digits convert i
        if (nrow(htmltablelines) > 99){
          t_num <- paste0("00",as.character(t))
        } else if (nrow(htmltablelines) > 9) {
          t_num <- paste0("0",as.character(t))
        } else{
          t_num <- as.character(t)
        }
        
        outputtable.name <- paste0(out,gsub(" ","_",paste0("/tables/",id,
                                                "_#", t_num, "_",
                                                tablenum)))
        if (nchar(outputtable.name) > 255) {
          print_message <- paste0("The file path of ",paste0(outputtable.name,
                                                             out.table.ext),
                                  " might be too long to be read by some programs. Consider using a shorter output path.")
          out_msg <- c(out_msg, print_message)
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
          update_progress_info(paste0(outputtable.name,
                                      out.table.format,
                                      " file path maybe too long."))
          outtable <- NULL
          for (row in 1:nrow(output.table)){
            out.row <- paste(paste0("\"",output.table[row,],"\""), collapse = ",")
            outtable <- c(outtable, gsub("\"NA\"","",out.row))
          }
          suppressWarnings(utils::write.table(outtable, file = paste0(outputtable.name,
                                                                      out.table.ext),
                                              sep = out.table.separator,quote = FALSE,
                                              row.names = FALSE, col.names = FALSE,
                                              na = "", fileEncoding = out.encoding))
        } else {
          outtable <- NULL
          for (row in 1:nrow(output.table)){
            out.row <- paste(paste0("\"",output.table[row,],"\""), collapse = ",")
            outtable <- c(outtable, gsub("\"NA\"","",out.row))
          }
          suppressWarnings(utils::write.table(outtable, file = paste0(outputtable.name,
                                                                      out.table.ext),
                                              sep = out.table.separator,quote = FALSE,
                                              row.names = FALSE, col.names = FALSE,
                                              na = "",fileEncoding = out.encoding))
        }
      }  ## end each table
      print_message <- paste0(length(names(processed.tables))," table(s) found in \'",
                              id,".pdf\'.")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      ## end if search words were found in table
    } else {
      print_message <- paste0("No table with search words for '",
                              id,".pdf\' found.")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      ## write an empty file
      if (write.tab.doc.file == TRUE) {
        dir.create(paste0(out,"/no_tab"), showWarnings = FALSE)
        utils::write.table("no table with search word found",
                           paste0(out,"/no_tab/",id, "_no_tab_w_swds",
                                  out.table.ext),
                           sep = out.table.separator,
                           row.names = FALSE, col.names = FALSE,
                           na = "")
      }
    }
  }  ## end if (whattoextr == 'tabandtxt' || whattoextr == 'tab' || whattoextr == 'table' || whattoextr == 'txtandtab')
  
  ## 7.3) Export TABLE INFO only if tab function was called --------------
  if (searchwords.go == TRUE && filterwords.go == TRUE &&
      integrity.indicator == TRUE && !length(tablestart.pos) == 0 &&
      nrow(as.data.frame((htmltablelines))) > 0 && ncol(as.data.frame((htmltablelines))) > 0 &&
      (whattoextr == "tabandtxt" ||
       whattoextr == "tab" ||
       whattoextr == "table" ||
       whattoextr == "txtandtab")) {
    ## write.table.locations ##
    
    if (write.table.locations == TRUE) {
      if (!is.null(htmltablelines) &&
          nrow(htmltablelines) > 0) {
        dir.create(paste0(out,"/html.docu"), showWarnings = FALSE)
        utils::write.table(htmltablelines,
                           paste0(out,"/html.docu/",
                                  id,"_htmltablelines", out.table.ext),
                           sep = out.table.separator,
                           row.names = FALSE)
      }
      if (!is.null(txttablelines) &&
          nrow(txttablelines) > 0) {
        dir.create(paste0(out,"/txt.docu"), showWarnings = FALSE)
        utils::write.table(txttablelines,
                           paste0(out,"/txt.docu/", id,"_txttablelines",
                                  out.table.ext),
                           sep = out.table.separator,
                           row.names = FALSE)
      }
      if (!is.null(keeplayouttxttablelines) &&
          nrow(keeplayouttxttablelines) > 0) {
        dir.create(paste0(out,"/keeptxt.docu"), showWarnings = FALSE)
        utils::write.table(keeplayouttxttablelines,
                           paste0(out,"/keeptxt.docu/", id,
                                  "_keeplayouttablelines", out.table.ext),
                           sep = out.table.separator,
                           row.names = FALSE)
      }
    }
    
    ## Return htmltablelines,txttablelines and
    ## keeplayouttxttablelines ##
    stat_output <- cbind(pdf_word_count = pdf_word_count, pdf_page_count = pdf_page_count,
                         pdf_filterword_total = pdf_filterword_total, 
                         pdf_filterword_percentage = paste0(as.character(as.numeric(pdf_filterword_total)/as.numeric(pdf_word_count)*100),"%"),
                         pdf_searchword_total = pdf_searchword_total,rbind(search.word.category_total),
                         rbind(pdf_filterword_times), rbind(pdf_searchword_times))
    extracol_num <- (ncol(stat_output) - 
                       length(pdf_filterword_times) - 
                       length(pdf_searchword_times) -
                       length(search.word.category_total) + 1)
    colnames(stat_output)[extracol_num:ncol(stat_output)] <- c(unique(search.word.categories),pdf_filterwords,pdf_searchwords)
    rownames(stat_output) <- id
    stat_output <- data.frame(cbind(stat_output, 
                              pdf_sentences_w_searchwords = NA),
                                           check.names = FALSE)

    output_files <- list(stat_output = stat_output,
                         htmltablelines = htmltablelines,
                         txttablelines = txttablelines,
                         keeplayouttxttablelines = keeplayouttxttablelines,
                         id = id)
    
  }  ## end if (whattoextr == 'tabandtxt' || whattoextr == 'tab' || whattoextr == 'table' || whattoextr == 'txtandtab')
  
  ## 8) Export of SENTENCES if txt function was called -----------------------------------------------
  if (filterwords.go == TRUE &&
      integrity.indicator == TRUE &&
      (whattoextr == "tabandtxt" || whattoextr == "txt" || whattoextr == "txtandtab")) {
    
    ## search for txt lines with search word in
    ## txtcontent ##
    txtpositionstable <- NULL
    pdf_searchwords <- NULL
    ## if there are search words
    if (!search.word.table[1, "words"] == "") {
      word_added <- FALSE
      for (i in 1:nrow(search.word.table)) {
        ## search for lines with searchword ##
        word <- search.word.table[i, "words"]
        ignore.case.sw <- search.word.table[i, "ignore.case.sw"]
        words.found.in.lines <- grep(word, txtcontent,
                                              ignore.case = ignore.case.sw)]
        word_added <- FALSE
        
        ##if search words were evaluated in tables
        if (sw_in_tab_counted == TRUE){
          pdf_searchword_times[i] <- 0
        }
        ## count search words in tables
        if (ignore.case.sw == TRUE){
          ic <- "ic"
        } else {
          ic <- "nic"
        }
        if (regex.sw == TRUE){
          reg <- "regex"
        } else {
          reg <- "nregex"
        }
        pdf_searchwords <- c(pdf_searchwords, paste0("SW_",ic,"_",reg,":",word))
        ## search each line for the exact position of the
        ## word
        current_word.txtpos.sw <- NULL
        if (length(words.found.in.lines)==0) {
          ## if tables were not searched/if search word were found in tables
          if (sw_in_tab_counted == FALSE && word_added == FALSE){
            pdf_searchword_times <- c(pdf_searchword_times, 0)
            pdf_searchword_names <- c(pdf_searchword_names, word)
            word_added <- TRUE
            ## if tables were searched (add to existing numbers)
          } else {
            pdf_searchword_times[i] <- pdf_searchword_times[i] + 0
            pdf_searchword_names[i] <- word
          }
          pdf_searchword_total <- sum(pdf_searchword_times)
          search.word.category_total <- NULL
          if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
            for (swc in 1:length(unique(search.word.categories))){
              search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
            }
          }
        } else {
          word_added <- FALSE
          for (line in words.found.in.lines) {
            ## detect all the positions of the word
            word.positions <- as.vector(gregexpr(word,
                                                 txtcontent[line],
                                                 ignore.case = ignore.case.sw)[[1]])
            if (word.positions == -1){
              word.positions <- NULL
            }
            ## if tables were not searched
            if (sw_in_tab_counted == FALSE && word_added == FALSE){
              pdf_searchword_times <- c(pdf_searchword_times, length(word.positions))
              pdf_searchword_names <- c(pdf_searchword_names, word)
              word_added <- TRUE
              ## if tables were searched (add to existing numbers)
            } else {
              pdf_searchword_times[i] <- pdf_searchword_times[i] + length(word.positions)
              pdf_searchword_names[i] <- word
            }
            pdf_searchword_total <- sum(pdf_searchword_times)
            search.word.category_total <- NULL
            if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
              for (swc in 1:length(unique(search.word.categories))){
                search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
              }
            }
            ## loop through all the positions
            for (word.pos in word.positions) {
              ## A) Detect sentences after
              postword.current.line <- strtoi(line)
              postword.current.sent.end <- word.pos
              postword.txtcontent <- ""
              ## search for the sentence beginning till context-th
              ## sentence is found
              for (x in 1:(context + 1)) {
                ## do till sentence begninning is found or postword
                ## of document test if sentence postword is in this
                ## line
                while (grepl("\\. [0-9A-Z]",
                             substr(txtcontent[postword.current.line],
                                    postword.current.sent.end,
                                    nchar(txtcontent[postword.current.line]))) == FALSE) {
                  ## if there is no postword sentence end, add everthing till end of line
                  # everthing till end of line
                  postword.txtcontent <- paste0(postword.txtcontent,
                                                substr(txtcontent[postword.current.line],
                                                       postword.current.sent.end,
                                                       nchar(txtcontent[postword.current.line])),
                                                " ")
                  ## if the end of the document is reached, end
                  if (postword.current.line >= length(txtcontent)) {
                    postword.current.sent.end <- nchar(txtcontent[postword.current.line])
                    break
                  } else {
                    ## set the beginning of the new line as begnning
                    postword.current.sent.end <- 1
                    postword.current.line <- postword.current.line + 1
                  }
                }
                ## new start point is set to the postword of the
                ## sentence +1
                new.postword.current.sent.end <- postword.current.sent.end +
                  as.vector(regexpr("\\. [0-9A-Z]",
                                    substr(txtcontent[postword.current.line],
                                           postword.current.sent.end,
                                           nchar(txtcontent[postword.current.line])))) + 1
                postword.txtcontent <- paste0(postword.txtcontent,
                                              substr(txtcontent[postword.current.line],
                                                     postword.current.sent.end,
                                                     new.postword.current.sent.end - 1))
                postword.current.sent.end <- new.postword.current.sent.end
              }  ## postword context loop
              numb.sentences.after <- x - 1
              postword.txtcontent <- substr(postword.txtcontent, 1, nchar(postword.txtcontent) - 1)
              postword.current.sent.end <- postword.current.sent.end - 1
              
              ## A) Detect sentences before
              preword.current.line <- strtoi(line)
              preword.current.sent.start <- word.pos - 1
              preword.txtcontent <- NULL
              for (y in 1:(context + 1)) {
                ## do till sentence begninning is found or beginning
                ## of document test if sentence pre.word is in this
                ## line
                while (grepl("\\. [0-9A-Z]",
                             substr(txtcontent[preword.current.line], 1,
                                    preword.current.sent.start)) == FALSE) {
                  ## only add if the whole paragraph has sentences in
                  ## it if (grepl('\\.
                  ## [:alnum:]',txtcontent[preword.current.line])) if
                  ## there is no preword sentence start, add everthing
                  ## till current position
                  preword.txtcontent <- paste0(" ",
                                               substr(txtcontent[preword.current.line],
                                                      1, preword.current.sent.start),
                                               preword.txtcontent)
                  ## if the beeginning of the document is reached, end
                  if (preword.current.line == 1) {
                    preword.current.sent.start <- 1
                    break
                  } else {
                    ## set the ends of sentence as of the new line as
                    ## begnning
                    preword.current.line <- preword.current.line - 1
                    preword.current.sent.start <- nchar(txtcontent[preword.current.line])
                  }
                }
                
                ## new sent.start point is set to the sent.start of
                ## the sentence +1
                new.preword.current.sent.start <- max(as.vector(gregexpr("\\. [0-9A-Z]",
                                                                         substr(txtcontent[preword.current.line],
                                                                                1, preword.current.sent.start))[[1]]))
                preword.txtcontent <- paste0(substr(txtcontent[preword.current.line],
                                                    new.preword.current.sent.start + 1,
                                                    preword.current.sent.start),
                                             preword.txtcontent)
                preword.current.sent.start <- new.preword.current.sent.start
              }  ## sent.start context loop
              numb.sentences.before <- y - 1
              preword.txtcontent <- substr(preword.txtcontent,
                                           2, nchar(preword.txtcontent))
              preword.current.sent.start <- preword.current.sent.start + 1
              
              ## find the page info
              currentlinenumb <- 0
              page_numb <- 0
              maxlinenumb <- sum(lengths(splittxtcontent))
              while ((currentlinenumb < preword.current.line) && (currentlinenumb != maxlinenumb)){
                page_numb <- page_numb + 1
                currentlinenumb <- currentlinenumb + length(splittxtcontent[[page_numb]])
              }
              start.paragraph <- preword.current.line - currentlinenumb + length(splittxtcontent[[page_numb]])
              end.paragraph <- postword.current.line - currentlinenumb + length(splittxtcontent[[page_numb]])
              #count sentences number (search word location_total sentences)
              outputrow <- data.frame(txtcontent = paste0(preword.txtcontent,
                                                          postword.txtcontent),
                                      page = page_numb,
                                      start.line = preword.current.line,
                                      start.pos = preword.current.sent.start,
                                      start.paragraph = start.paragraph,
                                      end.line = postword.current.line,
                                      end.pos = postword.current.sent.end,
                                      end.paragraph = end.paragraph,
                                      search.word.loc_total = paste0(as.character(numb.sentences.before + 1),"_",
                                                                     as.character(numb.sentences.before + 1 + numb.sentences.after)))
              txtpositionstable <- rbind(txtpositionstable,
                                         outputrow)
              txtpositionstable <- unique(txtpositionstable)
            }  ## end each search word
          }  ## end for each line
        } ## else if length(words.found.in.lines)==0
      }  ## end for each search word again
    
    
      ## export txt info
      if (!is.null(txtpositionstable)) {
      outputtable <- txtpositionstable[order(txtpositionstable$page,
                                             txtpositionstable$start.paragraph),
                                       c("txtcontent","page",
                                         "start.paragraph","end.paragraph",
                                         "search.word.loc_total")]
      
      sw_id <- create_id_from_stringlist(search.word.table[, "words"])
      pdf_sentences_w_searchwords <- nrow(outputtable)
      stat_output <- cbind(pdf_word_count = pdf_word_count, pdf_page_count = pdf_page_count,
                           pdf_filterword_total = pdf_filterword_total, 
                           pdf_filterword_percentage = paste0(as.character(as.numeric(pdf_filterword_total)/as.numeric(pdf_word_count)*100),"%"),
                           pdf_searchword_total = pdf_searchword_total,rbind(search.word.category_total),
                           rbind(pdf_filterword_times), rbind(pdf_searchword_times))
      extracol_num <- (ncol(stat_output) - 
                         length(pdf_filterword_times) - 
                         length(pdf_searchword_times) -
                         length(search.word.category_total) + 1)
      colnames(stat_output)[extracol_num:ncol(stat_output)] <- c(unique(search.word.categories),pdf_filterwords,pdf_searchwords)
      rownames(stat_output) <- id
      stat_output <- data.frame(cbind(stat_output, 
                                      pdf_sentences_w_searchwords = pdf_sentences_w_searchwords),
                                check.names = FALSE)
      if (sw_in_tab_counted == TRUE){
        ## output_files exist then just overwrite
        output_files$stat_output <- stat_output
      } else{
        ##otherwise generate
        output_files <- list(stat_output = stat_output,
                             htmltablelines = htmltablelines,
                             txttablelines = txttablelines,
                             keeplayouttxttablelines = keeplayouttxttablelines,
                             id = id)
      }
      print_message <- paste0(nrow(outputtable)," sentences with search words were found in \'",id,".pdf\'.")
      out_msg <- c(out_msg, print_message)
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      update_progress_info(print_message)
      dir.create(paste0(out,"/txt+-", context), showWarnings = FALSE)
      ## create file with all searchwords
      utils::write.table(c(paste(search.word.table[, "words"], collapse = ","),
                           search.word.table[, "words"]), 
                         file = paste0(out,"/txt+-", context,"/search_word_list_ID_",sw_id,
                                       ".txt"),
                         quote = FALSE,
                         row.names = FALSE,
                         col.names = FALSE)
      
      ##export table
      utils::write.table(outputtable, file = paste0(out,"/txt+-", context,"/",id,"_",sw_id,
                                                    "_swds_txt+-", context,
                                                    out.table.ext),
                         sep = out.table.separator,
                         row.names = FALSE,
                         fileEncoding = "UTF-8")
    } else if (filterwords.go == TRUE &&
               integrity.indicator == TRUE) {
        print_message <- paste0("No text with search words in \'",id,".pdf\' found")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
        ## write an empty file
        if (write.txt.doc.file == TRUE) {
          dir.create(paste0(out,"/no_txt_w_sw"), showWarnings = FALSE)
          utils::write.table("no text line with search word found.",
                             paste0(out,"/no_txt_w_sw/",id, "_no_txt_w_swds",
                                    out.table.ext),
                             sep = out.table.separator,
                             row.names = FALSE, col.names = FALSE,
                             na = "")
        }
      }
      # ##update output
      stat_output <- cbind(pdf_word_count = pdf_word_count, pdf_page_count = pdf_page_count,
                           pdf_filterword_total = pdf_filterword_total, 
                           pdf_filterword_percentage = paste0(as.character(as.numeric(pdf_filterword_total)/as.numeric(pdf_word_count)*100),"%"),
                           pdf_searchword_total = pdf_searchword_total,rbind(search.word.category_total),
                           rbind(pdf_filterword_times), rbind(pdf_searchword_times))
      extracol_num <- (ncol(stat_output) - 
                         length(pdf_filterword_times) - 
                         length(pdf_searchword_times) -
                         length(search.word.category_total) + 1)
      colnames(stat_output)[extracol_num:ncol(stat_output)] <- c(unique(search.word.categories),pdf_filterwords,pdf_searchwords)
      rownames(stat_output) <- id
      output_files$stat_output <- data.frame(cbind(stat_output,
                                                   pdf_sentences_w_searchwords = pdf_sentences_w_searchwords),
                                             check.names = FALSE)
    } else { ## end if (!search.word.table[1, "words"] == "") {
      pdf_searchword_total <- NA
      pdf_searchword_times <- NA
      pdf_searchwords <- "search_word_list"
      search.word.category_total <- NULL
      if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
        for (swc in 1:length(unique(search.word.categories))){
          search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
        }
      }
    }
  }  ## end if (whattoextr == 'tabandtxt' || whattoextr == 'txt' || whattoextr == 'txtandtab')
  
  ## 9) if the algorithm was not run in table detection ---------------------
  ## setting tablelines is NULL
  if (length(output_files) > 0) {
    
    if (exp.nondetc.tabs == TRUE) {
      if (nrow(output_files$htmltablelines) > 0 &&
          !is.null(output_files$htmltablelines)) {
        exp_nondetc_tabs(input_table = output_files$htmltablelines,
                         pdfpath = pdfpath, outputpath = out,
                         detectinfile = htmlpath,
                         based.on.column = "detected.in",
                         matches_end = "txtonly.notabledetected")
      }
      if (nrow(output_files$txttablelines) > 0 &&
          !is.null(output_files$htmltablelines)) {
        exp_nondetc_tabs(input_table = output_files$txttablelines,
                         pdfpath = pdfpath, outputpath = out,
                         detectinfile = txtpath,
                         based.on.column = "detected.in",
                         matches_end = "txtonly")
      }
    }
  }  ## end if algorithm was not run in table mode
  
  ## delete files which are not necessary anymore
  out_msg <- c(out_msg, deletefile(verbose))
  print_message <- paste0("Analysis of \'",id,".pdf\' complete.")
  out_msg <- c(out_msg, print_message)
  if (verbose) cat(utils::tail(out_msg,1), sep="\n")
  update_progress_info(print_message)
  if (filterwords.go == TRUE){
    ## if cpymv is either cpy or mv
    if (cpy_mv == "cpy"){
      dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
      dir.create(paste0(out,"/pdfs/incl_by_fw"), showWarnings = FALSE)
      if (!is.null(search.word.categories) && length(search.words) == length(search.word.categories)){
        ## 1) Determine best category
        search.word.category_total <- NULL
        for (swc in 1:length(unique(search.word.categories))){
          search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
        }
        max_position <- which(search.word.category_total == max(search.word.category_total))
        if (length(max_position) > 1){
          best_fit_category <- "mixed_categ"
        } else {
          best_fit_category <- unique(search.word.categories)[max_position]
        }
        ## 2) create folder for category
        dir.create(paste0(out,"/pdfs/incl_by_fw/",best_fit_category), showWarnings = FALSE)
        ## 3) move file into folder
        file.copy(pdf, paste0(out,"/pdfs/incl_by_fw/",best_fit_category))
        print_message <- paste0("\'",basename(pdf),
                                "\' was copied to \'", out,"/pdfs/incl_by_fw/",best_fit_category,"\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      } else {
        file.copy(pdf, paste0(out,"/pdfs/incl_by_fw"))
        print_message <- paste0("\'",basename(pdf),
                                "\' was copied to \'", out,"/pdfs/incl_by_fw\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      }
    } else if (cpy_mv == "mv"){
      dir.create(paste0(out,"/pdfs"), showWarnings = FALSE)
      dir.create(paste0(out,"/pdfs/incl_by_fw"), showWarnings = FALSE)
      if (is.null(search.word.categories)){
        ## 1) Determine best category
        search.word.category_total <- NULL
        for (swc in 1:length(unique(search.word.categories))){
          search.word.category_total[swc] <- sum(pdf_searchword_times[search.word.categories %in% unique(search.word.categories)[swc]], na.rm = TRUE)
        }
        max_position <- which(search.word.category_total == max(search.word.category_total))
        if (length(max_position) > 1){
          best_fit_category <- "mixed_categ"
        } else{
          best_fit_category <- unique(search.word.categories)[max_position]
        }
        ## 2) create folder for category
        dir.create(paste0(out,"/pdfs/incl_by_fw/",best_fit_category), showWarnings = FALSE)
        ## 3) move file into folder
        file.copy(pdf, paste0(out,"/pdfs/incl_by_fw/",best_fit_category))
        unlink(pdf)
        print_message <- paste0("\'",basename(pdf),
                                "\' was moved to \'", out,"/pdfs/incl_by_fw/",best_fit_category,"\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      } else {
        file.copy(pdf, paste0(out,"/pdfs/incl_by_fw"))
        unlink(pdf)
        print_message <- paste0("\'",basename(pdf),
                                "\' was moved to \'", out,"/pdfs/incl_by_fw\'.")
        out_msg <- c(out_msg, print_message)
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        update_progress_info(print_message)
      }
    }
  } # end filterwords.go
  
  ##update stats one last time
  stat_output <- cbind(pdf_word_count = pdf_word_count, pdf_page_count = pdf_page_count,
                       pdf_filterword_total = pdf_filterword_total, 
                       pdf_filterword_percentage = paste0(as.character(as.numeric(pdf_filterword_total)/as.numeric(pdf_word_count)*100),"%"),
                       pdf_searchword_total = pdf_searchword_total,rbind(search.word.category_total),
                       rbind(pdf_filterword_times), rbind(pdf_searchword_times))
  extracol_num <- (ncol(stat_output) - 
                     length(pdf_filterword_times) - 
                     length(pdf_searchword_times) -
                     length(search.word.category_total) + 1)
  colnames(stat_output)[extracol_num:ncol(stat_output)] <- c(unique(search.word.categories),pdf_filterwords,pdf_searchwords)
  rownames(stat_output) <- id
  output_files$stat_output <- data.frame(cbind(stat_output, 
                                               pdf_sentences_w_searchwords = pdf_sentences_w_searchwords),
                                         check.names = FALSE)
  output_files$out_msg <- out_msg
  return(output_files)
}  ## end function

#'Extracting data from PDF (Portable Document Format) files
#'
#'\code{PDE_extr_data_from_pdfs} extracts sentences or tables from a single PDF
#'file and writes output in the corresponding folder.
#'
#'@param pdfs String. A list of paths to the PDF files to be analyzed.
#'@param whattoextr String. Either \emph{txt}, \emph{tab}, or \emph{tabandtxt}
#'  for PDFS2TXT (extract sentences from a PDF file) or PDFS2TABLE (table of a PDF
#'  file to a Microsoft Excel file) extraction. \emph{tab} allows the extraction
#'  of tables with and without search words while \emph{txt} and \emph{tabandtxt}
#'  require search words.
#'@param out String. Directory chosen to save analysis results in. Default:
#'  \code{"."}.
#'@param filter.words List of strings. The list of filter words. If not
#'  \code{NA} or \code{""} a hit will be counted every time a word from the list
#'  is detected in the article.
#'   Default: \code{""}.
#'@param regex.fw Logical. If TRUE filter words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.fw Logical. Are the filter words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param filter.word.times Numeric or string. Can either be expressed as absolute number or percentage 
#'  of the total number of words (by adding the "%" sign). The minimum number of hits described for
#'  \code{filter.words} for a paper to be further analyzed. Default: \code{0.2\%}.
#'@param table.heading.words List of strings. Different than standard (TABLE,
#'  TAB or table plus number) headings to be detected. Regex rules apply (see
#'  also
#'  \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'   Default = \code{""}.
#'@param ignore.case.th Logical. Are the additional table headings (see
#'  \code{table.heading.words}) case-sensitive (does capitalization matter)?
#'  Default = \code{FALSE}.
#'@param search.words List of strings. List of search words. To extract all
#'  tables from the PDF files leave \code{search.words = ""}.
#'@param search.word.categories List of strings. List of categories with the 
#'  same length as the list of search words. Accordingly, each search word can be 
#'  assigned to a category, of which the word counts will be summarized in the
#'  \code{PDE_analyzer_word_stats.csv} file. If search.word.categories is a
#'  different length than search.words the parameter will be ignored.
#'  Default: \code{NULL}. 
#'@param regex.sw Logical. If TRUE search words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.sw Logical. Are the search words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param eval.abbrevs Logical. Should abbreviations for the search words be
#'  automatically detected and then replaced with the search word + "$*"?
#'  Default: \code{TRUE}.
#'@param out.table.format String. Output file format. Either comma separated
#'  file \code{.csv} or tab separated file \code{.tsv}. The encoding indicated
#'  in parantheses should be selected according to the operational system 
#'  exported tables are opened in, i.e., Windows: \code{"(WINDOWS-1252)"}; Mac: 
#'  \code{(macintosh)}; Linux: \code{(UTF-8)}. Default: \code{".csv"} and 
#'  encoding depending on the operational system.
#'@param dev_x Numeric. For a table the size of indention which would be
#'  considered the same column. Default: \code{20}.
#'@param dev_y Numeric. For a table the vertical distance which would be
#'  considered the same row. Can be either a number or set to dynamic detection 
#'  [9999], in which case the font size is used to detect which words are in the 
#'  same row. 
#'  Default: \code{9999}.
#'@param context Numeric. Number of sentences extracted before and after the
#'  sentence with the detected search word. If \code{0} only the sentence with
#'  the search word is extracted. Default: \code{0}.
#'@param write.table.locations Logical. If \code{TRUE}, a separate file with the
#'  headings of all tables, their relative location in the generated html and
#'  txt files, as well as information if search words were found will be
#'  generated. Default: \code{FALSE}.
#'@param exp.nondetc.tabs Logical. If \code{TRUE}, if a table was detected in a
#'  PDF file but is an image or cannot be read, the page with the table with be
#'  exported as a png. Default: \code{TRUE}.
#'@param write.tab.doc.file Logical. If \code{TRUE}, if search words are used
#'  for table detection and no search words were found in the tables of a PDF 
#'  file, a \strong{no.table.w.search.words}. Default: \code{TRUE}.
#'@param write.txt.doc.file Logical. If \code{TRUE}, if no search words were
#'  found in the sentences of a PDF file, a file will be created with the PDF
#'  filename followed by \strong{no.txt.w.search.words}. If the PDF file is
#'  empty, a file will be created with the PDF filename followed by
#'  \strong{no.content.detected}. If the filter word threshold is not met, 
#'  a file will be created with the PDF filename followed by 
#'  \strong{no.txt.w.filter.words}. Default: \code{TRUE}.
#'@param delete Logical. If \code{TRUE}, the intermediate \strong{txt},
#'  \strong{keeplayouttxt} and \strong{html} copies of the PDF files will be 
#'  deleted. Default: \code{TRUE}.
#'@param cpy_mv String. Either "nocpymv", "cpy", or "mv". If filter words are used in the
#'  analyses, the processed PDF files will either be copied ("cpy") or moved ("mv") into the
#'  /pdf/ subfolder of the output folder. Default: \code{"nocpymv"}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'@return If tables were extracted from the PDF file the function returns a list of
#'  following tables/items: 1) \strong{htmltablelines}, 2)
#'  \strong{txttablelines}, 3) \strong{keeplayouttxttablelines}, 4) \strong{id},
#'  5) \strong{out_msg}.
#'  The \strong{tablelines} are tables that provide the heading and position of
#'  the detected tables. The \strong{id} provide the name of the PDF file. The
#'  \strong{out_msg} includes all messages printed to the console or the suppressed
#'  messages if \code{verbose=FALSE}.
#'
#'@examples
#'
#'## Running a simple analysis with filter and search words to extract sentences and tables
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_extr_data_from_pdfs(pdfs = c(paste0(system.file(package = "PDE"),
#'                                                  "/examples/Methotrexate/29973177_!.pdf"),
#'                                                  paste0(system.file(package = "PDE"),
#'                                                  "/examples/Methotrexate/31083238_!.pdf")),
#'  whattoextr = "tabandtxt",
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_output+-0_test/"),
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  ignore.case.fw = TRUE,
#'  regex.fw = FALSE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  ignore.case.sw = FALSE,
#'  regex.sw = TRUE)
#'}
#'
#'## Running an advanced analysis with filter and search words to
#'## extract sentences and tables and obtain documentation files
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_extr_data_from_pdfs(pdfs = c(paste0(system.file(package = "PDE"),
#'                                                  "/examples/Methotrexate/29973177_!.pdf"),
#'                                                   paste0(system.file(package = "PDE"),
#'                                                  "/examples/Methotrexate/31083238_!.pdf")),
#'  whattoextr = "tabandtxt",
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_output+-1_test/"),
#'  context = 1,
#'  dev_x = 20,
#'  dev_y = 9999,
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants",";")[[1]],
#'  ignore.case.fw = TRUE,
#'  regex.fw = FALSE,
#'  filter.word.times = "0.2%",
#'  table.heading.words = "",
#'  ignore.case.th = FALSE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  regex.sw = TRUE,
#'  ignore.case.sw = FALSE,
#'  eval.abbrevs = TRUE,
#'  out.table.format = ".csv (WINDOWS-1252)",
#'  write.table.locations = TRUE,
#'  write.tab.doc.file = TRUE,
#'  write.txt.doc.file = TRUE,
#'  exp.nondetc.tabs = TRUE,
#'  cpy_mv = "nocpymv",
#'  delete = TRUE)
#'}
#'
#'@seealso
#'\code{\link{PDE_pdfs2table}},\code{\link{PDE_pdfs2table_searchandfilter}},\code{\link{PDE_pdfs2txt_searchandfilter}}
#'
#'@export
PDE_extr_data_from_pdfs <- function(pdfs, whattoextr,
                                    out = ".", filter.words = "", regex.fw = TRUE, ignore.case.fw = FALSE, 
                                    filter.word.times = "0.2%",
                                    table.heading.words = "", ignore.case.th = FALSE, search.words, search.word.categories = NULL, regex.sw = TRUE,
                                    ignore.case.sw = FALSE, eval.abbrevs = TRUE, 
                                    out.table.format = ".csv (WINDOWS-1252)", dev_x = 20, dev_y = 9999, context = 0,
                                    write.table.locations = FALSE, exp.nondetc.tabs = TRUE, write.tab.doc.file = TRUE,
                                    write.txt.doc.file = TRUE,  delete = TRUE, cpy_mv = "nocpymv",verbose = TRUE){
  ## run the analysis ----------------------------------------------------------------
  tablelines <- NULL
  output <- NULL
  
  for (pdf in pdfs) {
    
    if(file.exists(pdf)){
      tablelines <- .PDE_extr_data_from_pdf(pdf = pdf, whattoextr = whattoextr,
                                            out = out, context = context, dev_x = dev_x,
                                            filter.words = filter.words, regex.fw = regex.fw,
                                            ignore.case.fw = ignore.case.fw,
                                            filter.word.times = filter.word.times, 
                                            table.heading.words = table.heading.words,
                                            ignore.case.th = ignore.case.th, 
                                            search.words = search.words,search.word.categories = search.word.categories,
                                            regex.sw = regex.sw,
                                            ignore.case.sw = ignore.case.sw, eval.abbrevs = eval.abbrevs,
                                            write.table.locations = write.table.locations,
                                            write.tab.doc.file = write.tab.doc.file, 
                                            write.txt.doc.file = write.txt.doc.file,
                                            exp.nondetc.tabs = exp.nondetc.tabs, 
                                            out.table.format = out.table.format,
                                            cpy_mv = cpy_mv,
                                            delete = delete, verbose = verbose)
      ## example data
    } else {
      id <- sub("^(.*)\\..*$", "\\1", basename(pdf))
      tablelines <- list(htmltablelines = "",
                         txttablelines = "",
                         keeplayouttxttablelines = "",
                         id = id)
    }
    ## if the algorithm was not run in table detection
    ## setting tablelines is NULL
    if (length(tablelines) > 0) {
      
      # ## add new tablelines to output
      # output[[length(output) + 1]] <- tablelines
      
      ##make stat_output mastertable
      if (is.null(output)){
        output <- tablelines$stat_output
      } else {
        output[setdiff(names(tablelines$stat_output), names(output))] <- NA
        tablelines$stat_output[setdiff(names(output), names(tablelines$stat_output))] <- NA
        output <- rbind(output,tablelines$stat_output)
      }
    }
    if (grepl("csv", out.table.format)) {
      out.table.separator <- ","
      out.table.ext <- ".csv"
    }
    if (grepl("tsv", out.table.format)) {
      out.table.separator <- "\t"
      out.table.ext <- ".tsv"
    }
    output_table <- cbind(pdf_file_name = rownames(output), output)
    utils::write.table(output_table, file = paste0(out,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                   out.table.ext),
                       sep = out.table.separator,
                       row.names = FALSE)
    
  }  ## end for each PDF file
  
  ## write the PDE_analyzer_word_stats table
  if ("pdf_searchword_total" %in% colnames(output)){
    if ("pdf_filterword_total" %in% colnames(output)){
      output <- output[order(output$pdf_searchword_total,
                             output$pdf_filterword_total, decreasing = TRUE),,drop=FALSE]
    } else {
      output <- output[order(output$pdf_searchword_total, decreasing = TRUE),,drop=FALSE]
    }
  }
  todays.date_time <- paste0(format(Sys.Date(), "%Y-%m-%d"),format(Sys.time(), "-%Hh-%Mm"))
  if (grepl("csv", out.table.format)) {
    out.table.separator <- ","
    out.table.ext <- ".csv"
  }
  if (grepl("tsv", out.table.format)) {
    out.table.separator <- "\t"
    out.table.ext <- ".tsv"
  }
  output_table <- cbind(pdf_file_name = rownames(output), output)
  utils::write.table(output_table, file = paste0(out,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                 out.table.ext),
                     sep = out.table.separator,
                     row.names = FALSE)
  
  return(output)
  
}

#'Extracting all tables from a PDF (Portable Document Format) file
#'
#'\code{PDE_pdfs2table} extracts all tables from a single PDF
#'file and writes output in the corresponding folder.
#'
#'@param pdfs String. A list of paths to the PDF files to be analyzed.
#'@param out String. Directory chosen to save tables in. Default:
#'  \code{"."}.
#'@param table.heading.words List of strings. Different than standard (TABLE,
#'  TAB or table plus number) headings to be detected. Regex rules apply (see
#'  also
#'  \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'   Default = \code{""}.
#'@param ignore.case.th Logical. Are the additional table headings (see
#'  \code{table.heading.words}) case-sensitive (does capitalization matter)?
#'  Default = \code{FALSE}.
#'@param out.table.format String. Output file format. Either comma separated
#'  file \code{.csv} or tab separated file \code{.tsv}. The encoding indicated
#'  in parantheses should be selected according to the operational system 
#'  exported tables are opened in, i.e., Windows: \code{"(WINDOWS-1252)"}; Mac: 
#'  \code{(macintosh)}; Linux: \code{(UTF-8)}. Default: \code{".csv"} and 
#'  encoding depending on the operational system.
#'@param dev_x Numeric. For a table the size of indention which would be
#'  considered the same column. Default: \code{20}.
#'@param dev_y Numeric. For a table the vertical distance which would be
#'  considered the same row. Can be either a number or set to dynamic detection 
#'  [9999], in which case the font size is used to detect which words are in the 
#'  same row. 
#'  Default: \code{9999}.
#'@param write.table.locations Logical. If \code{TRUE}, a separate file with the
#'  headings of all tables, their relative location in the generated html and
#'  txt files, as well as information if search words were found will be
#'  generated. Default: \code{FALSE}.
#'@param exp.nondetc.tabs Logical. If \code{TRUE}, if a table was detected in a
#'  PDF file but is an image or cannot be read, the page with the table with be
#'  exported as a png. Default: \code{FALSE}.
#'@param delete Logical. If \code{TRUE}, the intermediate \strong{txt},
#'  \strong{keeplayouttxt} and \strong{html} copies of the PDF file will be 
#'  deleted. Default: \code{TRUE}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'@examples
#'## Running a simple table extraction
#'if(PDE_check_Xpdf_install() == TRUE){
#' outputtables <- PDE_pdfs2table(pdf = paste0(system.file(package = "PDE"),
#'                  "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/29973177_tables/"))
#'}
#'
#'## Running a the same table extraction as above with all paramaters shown
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_pdfs2table(pdf = paste0(system.file(package = "PDE"),
#'                                  "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/29973177_tables/"),
#'  dev_x = 20,
#'  dev_y = 9999,
#'  table.heading.words = "",
#'  ignore.case.th = FALSE,
#'  out.table.format = ".csv (WINDOWS-1252)",
#'  write.table.locations = FALSE,
#'  exp.nondetc.tabs = FALSE,
#'  delete = TRUE)
#'}
#'
#'@seealso
#'\code{\link{PDE_extr_data_from_pdfs}},\code{\link{PDE_pdfs2table_searchandfilter}}
#'
#'@export
PDE_pdfs2table <- function(pdfs, out = ".", table.heading.words = "", ignore.case.th = FALSE, 
                           out.table.format = ".csv (WINDOWS-1252)", dev_x = 20, dev_y = 9999,
                           write.table.locations = FALSE, exp.nondetc.tabs = TRUE, delete = TRUE, 
                           verbose=TRUE){
  
  ## run the analysis ----------------------------------------------------------------
  no_output <- NULL
  for (pdf in pdfs) {
    
    if(file.exists(pdf)){
      no_output <- .PDE_extr_data_from_pdf(pdf=pdf,
                                           whattoextr = "tab",
                                           out = out,
                                           filter.words = "",
                                           ignore.case.fw = FALSE,
                                           filter.word.times = "0.2%",
                                           table.heading.words = table.heading.words,
                                           ignore.case.th = ignore.case.th,
                                           search.words = "",search.word.categories = NULL,
                                           ignore.case.sw = FALSE,
                                           eval.abbrevs = FALSE,
                                           out.table.format = out.table.format,
                                           dev_x = dev_x,
                                           dev_y = dev_y,
                                           context = 0,
                                           write.table.locations = write.table.locations,
                                           exp.nondetc.tabs = exp.nondetc.tabs,
                                           write.tab.doc.file = FALSE,
                                           write.txt.doc.file = FALSE,
                                           cpy_mv = "nocpymv",
                                           delete = delete, verbose = verbose)
    }
  }
}

#'Extracting tables from a PDF (Portable Document Format) file
#'
#'\code{PDE_pdfs2table_searchandfilter} extracts tables from a single PDF file
#'according to filter and search words and writes output in the corresponding
#'folder.
#'
#'@param pdfs String. A list of paths to the PDF files to be analyzed.
#'@param out String. Directory chosen to save analysis results in. Default:
#'  \code{"."}.
#'@param filter.words List of strings. The list of filter words. If not
#'  \code{NA} or \code{""} a hit will be counted every time a word from the list
#'  is detected in the article.
#'   Default: \code{""}.
#'@param regex.fw Logical. If TRUE filter words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.fw Logical. Are the filter words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param filter.word.times Numeric or string. Can either be expressed as absolute number or percentage 
#'  of the total number of words (by adding the "%" sign). The minimum number of hits described for
#'  \code{filter.words} for a paper to be further analyzed. Default: \code{0.2\%}.
#'@param table.heading.words List of strings. Different than standard (TABLE,
#'  TAB or table plus number) headings to be detected. Regex rules apply (see
#'  also
#'  \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'   Default = \code{""}.
#'@param ignore.case.th Logical. Are the additional table headings (see
#'  \code{table.heading.words}) case-sensitive (does capitalization matter)?
#'  Default = \code{FALSE}.
#'@param search.words List of strings. List of search words. To extract all
#'  tables from the PDF file leave \code{search.words = ""}.
#'@param search.word.categories List of strings. List of categories with the 
#'  same length as the list of search words. Accordingly, each search word can be 
#'  assigned to a category, of which the word counts will be summarized in the
#'  \code{PDE_analyzer_word_stats.csv} file. If search.word.categories is a
#'  different length than search.words the parameter will be ignored.
#'  Default: \code{NULL}.
#'@param regex.sw Logical. If TRUE search words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.sw Logical. Are the search words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param eval.abbrevs Logical. Should abbreviations for the search words be
#'  automatically detected and then replaced with the search word + "$*"?
#'  Default: \code{TRUE}.
#'@param out.table.format String. Output file format. Either comma separated
#'  file \code{.csv} or tab separated file \code{.tsv}. The encoding indicated
#'  in parantheses should be selected according to the operational system 
#'  exported tables are opened in, i.e., Windows: \code{"(WINDOWS-1252)"}; Mac: 
#'  \code{(macintosh)}; Linux: \code{(UTF-8)}. Default: \code{".csv"} and 
#'  encoding depending on the operational system.
#'@param dev_x Numeric. For a table the size of indention which would be
#'  considered the same column. Default: \code{20}.
#'@param dev_y Numeric. For a table the vertical distance which would be
#'  considered the same row. Can be either a number or set to dynamic detection 
#'  [9999], in which case the font size is used to detect which words are in the 
#'  same row. 
#'  Default: \code{9999}.
#'@param write.table.locations Logical. If \code{TRUE}, a separate file with the
#'  headings of all tables, their relative location in the generated html and
#'  txt files, as well as information if search words were found will be
#'  generated. Default: \code{FALSE}.
#'@param exp.nondetc.tabs Logical. If \code{TRUE}, if a table was detected in a
#'  PDF file but is an image or cannot be read, the page with the table with be
#'  exported as a png. Default: \code{TRUE}.
#'@param write.tab.doc.file Logical. If \code{TRUE}, if search words are used
#'  for table detection and no search words were found in the tables of a PDF 
#'  file, a \strong{no.table.w.search.words}. Default: \code{TRUE}.
#'@param delete Logical. If \code{TRUE}, the intermediate \strong{txt},
#'  \strong{keeplayouttxt} and \strong{html} copies of the PDF file will be 
#'  deleted. Default: \code{TRUE}.
#'@param cpy_mv String. Either "nocpymv", "cpy", or "mv". If filter words are used in the
#'  analyses, the processed PDF files will either be copied ("cpy") or moved ("mv") into the
#'  /pdf/ subfolder of the output folder. Default: \code{"nocpymv"}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'@return If tables were extracted from the PDF file the function returns a list of
#'  following tables/items: 1) \strong{htmltablelines}, 2)
#'  \strong{txttablelines}, 3) \strong{keeplayouttxttablelines}, 4) \strong{id},
#'  5) \strong{out_msg}.
#'  The \strong{tablelines} are tables that provide the heading and position of
#'  the detected tables. The \strong{id} provide the name of the PDF file. The
#'  \strong{out_msg} includes all messages printed to the console or the suppressed
#'  messages if \code{verbose=FALSE}.
#'
#'@examples
#'
#'## Running a simple analysis with filter and search words to extract tables
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_pdfs2table_searchandfilter(pdf = paste0(system.file(package = "PDE"),
#'                                    "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/29973177_tables/"),
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  regex.fw = FALSE,
#'  ignore.case.fw = TRUE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  regex.sw = TRUE,
#'  ignore.case.sw = FALSE)
#'}
#'
#'## Running an advanced analysis with filter and search words to
#'## extract tables and obtain documentation files
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_pdfs2table_searchandfilter(pdf = paste0(system.file(package = "PDE"),
#'                                    "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/29973177_tables/"),
#'  dev_x = 20,
#'  dev_y = 9999,
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  regex.fw = FALSE,
#'  ignore.case.fw = TRUE,
#'  filter.word.times = "0.2%",
#'  table.heading.words = "",
#'  ignore.case.th = FALSE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  regex.sw = TRUE,
#'  ignore.case.sw = FALSE,
#'  eval.abbrevs = TRUE,
#'  out.table.format = ".csv (WINDOWS-1252)",
#'  write.table.locations = TRUE,
#'  write.tab.doc.file = TRUE,
#'  exp.nondetc.tabs = TRUE,
#'  cpy_mv = "nocpymv",
#'  delete = TRUE)
#'}
#'
#'@seealso \code{\link{PDE_extr_data_from_pdfs}}, \code{\link{PDE_pdfs2table}}
#'
#'@export
PDE_pdfs2table_searchandfilter <- function(pdfs, out = ".", filter.words = "", regex.fw = TRUE, 
                                           ignore.case.fw = FALSE, filter.word.times = "0.2%",
                                           table.heading.words = "", ignore.case.th = FALSE, search.words,search.word.categories = NULL,
                                           regex.sw = TRUE, ignore.case.sw = FALSE, eval.abbrevs = TRUE, 
                                           out.table.format = ".csv (WINDOWS-1252)", dev_x = 20, dev_y = 9999,
                                           write.table.locations = FALSE, exp.nondetc.tabs = TRUE, 
                                           write.tab.doc.file = TRUE,
                                           delete = TRUE, cpy_mv = "nocpymv", verbose = TRUE){
  tablelines <- NULL
  output <- NULL
  for (pdf in pdfs) {
    
    if(file.exists(pdf)){
      tablelines <- .PDE_extr_data_from_pdf(pdf=pdf,
                                            whattoextr = "tab",
                                            out = out,
                                            filter.words = filter.words,
                                            ignore.case.fw = ignore.case.fw,
                                            filter.word.times = filter.word.times,
                                            table.heading.words = table.heading.words,
                                            ignore.case.th = ignore.case.th,
                                            search.words = search.words,search.word.categories = search.word.categories,
                                            ignore.case.sw = ignore.case.sw,
                                            eval.abbrevs = eval.abbrevs,
                                            out.table.format = out.table.format,
                                            dev_x = dev_x,
                                            dev_y = dev_y,
                                            context = 0,
                                            write.table.locations = write.table.locations,
                                            exp.nondetc.tabs = exp.nondetc.tabs,
                                            write.tab.doc.file = write.tab.doc.file,
                                            write.txt.doc.file = FALSE,
                                            delete = delete, cpy_mv = cpy_mv, verbose = verbose)
    } else {
      id = sub("^(.*)\\..*$", "\\1", basename(pdf))
      output = list(output = "",
                    txttablelines = "",
                    keeplayouttxttablelines = "",
                    id = id)
    }
    ## if the algorithm was not run in table detection
    ## setting tablelines is NULL
    if (length(tablelines) > 0) {
      
      # ## add new tablelines to output
      # output[[length(output) + 1]] <- tablelines
      
      ##make stat_output mastertable
      if (is.null(output)){
        output <- tablelines$stat_output
      } else {
        output[setdiff(names(tablelines$stat_output), names(output))] <- NA
        tablelines$stat_output[setdiff(names(output), names(tablelines$stat_output))] <- NA
        output <- rbind(output,tablelines$stat_output)
      }
    }
    
  }  ## end for each pdf
  
  ## write the PDE_analyzer_word_stats table
  if ("pdf_searchword_total" %in% colnames(output)){
    if ("pdf_filterword_total" %in% colnames(output)){
      output <- output[order(output$pdf_searchword_total,
                             output$pdf_filterword_total, decreasing = TRUE),,drop=FALSE]
    } else {
      output <- output[order(output$pdf_searchword_total, decreasing = TRUE),,drop=FALSE]
    }
  }
  todays.date_time <- paste0(format(Sys.Date(), "%Y-%m-%d"),format(Sys.time(), "-%Hh-%Mm"))
  if (grepl("csv", out.table.format)) {
    out.table.separator <- ","
    out.table.ext <- ".csv"
  }
  if (grepl("tsv", out.table.format)) {
    out.table.separator <- "\t"
    out.table.ext <- ".tsv"
  }
  output_table <- cbind(pdf_file_name = rownames(output), output)
  utils::write.table(output_table, file = paste0(out,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                 out.table.ext),
                     sep = out.table.separator,
                     row.names = FALSE)
  
  return(output)
}

#'Extracting sentences from a PDF (Portable Document Format) file
#'
#'\code{PDE_pdfs2txt_searchandfilter} extracts sentences from a single PDF file
#'according to search and filter words and writes output in the corresponding
#'folder.
#'
#'@param pdfs String. A list of paths to the PDF files to be analyzed.
#'@param out String. Directory chosen to save analysis results in. Default:
#'  \code{"."}.
#'@param filter.words List of strings. The list of filter words. If not
#'  \code{NA} or \code{""} a hit will be counted every time a word from the list
#'  is detected in the article.
#'   Default: \code{""}.
#'@param regex.fw Logical. If TRUE filter words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.fw Logical. Are the filter words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param filter.word.times Numeric or string. Can either be expressed as absolute number or percentage 
#'  of the total number of words (by adding the "%" sign). The minimum number of hits described for
#'  \code{filter.words} for a paper to be further analyzed. Default: \code{0.2\%}.
#'@param search.words List of strings. List of search words. 
#'@param search.word.categories List of strings. List of categories with the 
#'  same length as the list of search words. Accordingly, each search word can be 
#'  assigned to a category, of which the word counts will be summarized in the
#'  \code{PDE_analyzer_word_stats.csv} file. If search.word.categories is a
#'  different length than search.words the parameter will be ignored.
#'  Default: \code{NULL}.
#'@param regex.sw Logical. If TRUE search words will follow the regex rules
#' (see \url{https://github.com/erikstricker/PDE/blob/master/inst/examples/cheatsheets/regex.pdf}).
#'  Default = \code{TRUE}.
#'@param ignore.case.sw Logical. Are the search words case-sensitive (does
#'  capitalization matter)? Default: \code{FALSE}.
#'@param eval.abbrevs Logical. Should abbreviations for the search words be
#'  automatically detected and then replaced with the search word + "$*"?
#'  Default: \code{TRUE}.
#'@param eval.abbrevs Logical. Should abbreviations for the search words be
#'  automatically detected and then replaced with the search word + "$*"?
#'  Default: \code{TRUE}.
#'@param out.table.format String. Output file format. Either comma separated
#'  file \code{.csv} or tab separated file \code{.tsv}. The encoding indicated
#'  in parantheses should be selected according to the operational system 
#'  exported tables are opened in, i.e., Windows: \code{"(WINDOWS-1252)"}; Mac: 
#'  \code{(macintosh)}; Linux: \code{(UTF-8)}. Default: \code{".csv"} and 
#'  encoding depending on the operational system.
#'@param context Numeric. Number of sentences extracted before and after the
#'  sentence with the detected search word. If \code{0} only the sentence with
#'  the search word is extracted. Default: \code{0}.
#'@param write.txt.doc.file Logical. If \code{TRUE}, if no search words were
#'  found in the sentences of a PDF file, a file will be created with the PDF
#'  filename followed by \strong{no.txt.w.search.words}. If the PDF file is
#'  empty, a file will be created with the PDF filename followed by
#'  \strong{no.content.detected}. If the filter word threshold is not met, 
#'  a file will be created with the PDF filename followed by 
#'  \strong{no.txt.w.filter.words}. Default: \code{TRUE}.
#'@param delete Logical. If \code{TRUE}, the intermediate \strong{txt},
#'  \strong{keeplayouttxt} and \strong{html} copies of the PDF file will be 
#'  deleted. Default: \code{TRUE}.
#'@param cpy_mv String. Either "nocpymv", "cpy", or "mv". If filter words are used in the
#'  analyses, the processed PDF files will either be copied ("cpy") or moved ("mv") into the
#'  /pdf/ subfolder of the output folder. Default: \code{"nocpymv"}.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'@examples
#'## Running a simple analysis with filter and search words to extract sentences
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_pdfs2txt_searchandfilter(pdf = paste0(system.file(package = "PDE"),
#'                                       "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_txt+-0/"),
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  regex.fw = FALSE,
#'  ignore.case.fw = TRUE,
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  regex.sw = TRUE,
#'  ignore.case.sw = FALSE)
#'}
#'
#'## Running an advanced analysis with filter and search words to
#'## extract sentences and obtain documentation files
#'if(PDE_check_Xpdf_install() == TRUE){
#'  outputtables <- PDE_pdfs2txt_searchandfilter(pdf = paste0(system.file(package = "PDE"),
#'                                        "/examples/Methotrexate/29973177_!.pdf"),
#'  out = paste0(system.file(package = "PDE"),"/examples/MTX_txt+-1/"),
#'  context = 1,
#'  filter.words = strsplit("cohort;case-control;group;study population;study participants", ";")[[1]],
#'  regex.fw = FALSE,
#'  ignore.case.fw = TRUE,
#'  filter.word.times = "0.2%",
#'  search.words = strsplit("(M|m)ethotrexate;(T|t)rexal;(R|r)heumatrex;(O|o)trexup", ";")[[1]],
#'  regex.sw = TRUE,
#'  ignore.case.sw = FALSE,
#'  eval.abbrevs = TRUE,
#'  out.table.format = ".csv (WINDOWS-1252)",
#'  write.txt.doc.file = TRUE,
#'  cpy_mv = "nocpymv",
#'  delete = TRUE)
#'}
#'
#'@seealso \code{\link{PDE_extr_data_from_pdfs}}
#'
#'@export
PDE_pdfs2txt_searchandfilter = function(pdfs, out = ".", filter.words = "", regex.fw = TRUE,
                                        ignore.case.fw = FALSE, filter.word.times = "0.2%", search.words,search.word.categories = NULL,
                                        regex.sw = TRUE,
                                        ignore.case.sw = FALSE, eval.abbrevs = TRUE, 
                                        out.table.format = ".csv (WINDOWS-1252)", context = 0,
                                        write.txt.doc.file = TRUE,
                                        delete = TRUE, cpy_mv = "nocpymv",verbose=TRUE){
  
  no_output <- NULL
  for (pdf in pdfs) {
    
    if(file.exists(pdf)){
      no_output <- .PDE_extr_data_from_pdf(pdf=pdf,
                                           whattoextr = "txt",
                                           out = out,
                                           filter.words = filter.words,
                                           ignore.case.fw = ignore.case.fw,
                                           filter.word.times = filter.word.times,
                                           table.heading.words = "",
                                           ignore.case.th = FALSE,
                                           search.words = search.words,search.word.categories = search.word.categories,
                                           ignore.case.sw = ignore.case.sw,
                                           eval.abbrevs = eval.abbrevs,
                                           out.table.format = out.table.format,
                                           dev_x = 1,
                                           dev_y = 9999,
                                           context = context,
                                           write.table.locations = FALSE,
                                           exp.nondetc.tabs = FALSE,
                                           write.tab.doc.file = FALSE,
                                           write.txt.doc.file = write.txt.doc.file,
                                           delete = delete, cpy_mv = cpy_mv, verbose = verbose)
    }
  }
  
}


## Start the PDE analyzer ------------------------------------------
#' Extracting data from PDF (Portable Document Format) files using a user interface
#'
#' The \code{PDE_analyzer_i} provides a user interface for 
#' the sentence and table extraction from multiple PDF files.
#' 
#' @param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#' @section Note:
#' A detailed description of the elements in the user interface 
#' can be found in the markdown file (README_PDE.md).
#'
#' @import tcltk tcltk2
#' 
#' @examples 
#' 
#' PDE_analyzer_i()
#' 
#' 
#' @export
PDE_analyzer_i <- function(verbose=TRUE) {
  
  ##functions --------------------------------------------------
  tooltip <- function(text, targetWidget, width = 350){
    
    end <- function(){
      tkdestroy(base)
    }
    
    tipX <- as.numeric(tkwinfo("rootx", targetWidget)) +
      as.numeric(tkwinfo("width", targetWidget))
    tipY <- as.numeric(tkwinfo("rooty", targetWidget))
    
    # Takes out the frame and title bar
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    on.exit(tkdestroy(base))
    # Put the TW in the right place
    tkwm.geometry(base, paste("+", tipX, "+", tipY, sep = ""))
    tip <- tklabel(base, text = text, background = "white",
                   wraplength = width)
    tkpack(tip)
    
    tkbind(targetWidget, "<Leave>", end)
    
    tkwait.window(base)
    
    return(invisible())
  }
  
  ## set the variables ------------------------------------------
  out_msg <- NULL
  tsv_location.var <- tcltk::tclVar("")
  ## in_output
  pdfs.var <- tcltk::tclVar("")
  whattoextr.var <- tcltk::tclVar("tab")
  outputfolder.var <- tcltk::tclVar("")
  if (Sys.info()["sysname"] == "Windows") {
    out.table.format.var <- tcltk::tclVar(".csv (WINDOWS-1252)")
    ext <- ".exe"
  } else if (Sys.info()["sysname"] == "Darwin") {
    out.table.format.var <- tcltk::tclVar(".csv (macintosh)")
  } else if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "SunOS") {
    out.table.format.var <- tcltk::tclVar(".csv (UTF-8)")
  } else {
    out.table.format.var <- tcltk::tclVar(".csv (WINDOWS-1252)")
  }
  
  ## paras
  table.heading.words.var <- tcltk::tclVar("")
  ignore.case.th.var <- tcltk::tclVar("FALSE")
  dev_x.var <- tcltk::tclVar("20")
  dev_y.var <- tcltk::tclVar("9999")
  dynamic.dev_y.var <- tcltk::tclVar("1")
  percentage.fw.var <- tcltk::tclVar("1")
  regex.sw.var <- tcltk::tclVar("0")
  regex.fw.var <- tcltk::tclVar("0")
  l15.filter.words.rbValue <- tcltk::tclVar("no")
  filter.words.var <- tcltk::tclVar("")
  filter.word.times.var <- tcltk::tclVar("20")
  filter.word.times_value.var <- tcltk::tclVar("20")
  ignore.case.fw.var <- tcltk::tclVar("FALSE")
  copy_move_pdfs.var <- tcltk::tclVar("nocpymv")
  l19.rbValue <- tcltk::tclVar("all")
  search.words.var <- tcltk::tclVar("")
  ignore.case.sw.var <- tcltk::tclVar("FALSE")
  context.var <- tcltk::tclVar("0")
  eval.abbrevs.var <- tcltk::tclVar("TRUE")
  ## docus
  write.table.locations.var <- tcltk::tclVar("FALSE")
  exp.nondetc.tabs.var <- tcltk::tclVar("TRUE")
  write.tab.doc.file.var <- tcltk::tclVar("TRUE")
  write.txt.doc.file.var <- tcltk::tclVar("TRUE")
  delete.var <- tcltk::tclVar("TRUE")
  proc.pdf <- tcltk::tclVar("")
  progress <- tcltk::tclVar("0")
  scrollpos <- tcltk::tclVar("0")
  ## start, pause, stop, close
  start.pause.but.label.var <- tcltk::tclVar("Start analysis")
  close.stop.but.label.var <- tcltk::tclVar("Close session")
  
  ## variable for the table display
  wrap.var <- tcltk::tclVar("1")
  columnnumber.var <- tcltk::tclVar("0")
  table.location.var <- tcltk::tclVar("")
  
  ## find os.font
  if (Sys.info()["sysname"] == "Windows") {
    os.font <- "Arial"
    os.font.ten.bold <- "Arial 10 bold"
    os.font.twelve.bold <- "Arial 12 bold"
  } else if (Sys.info()["sysname"] == "Darwin") {
    os.font <- "Helvetica"
    os.font.ten.bold <- "Helvetica 10 bold"
    os.font.twelve.bold <- "Helvetica 12 bold"
  } else if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "SunOS") {
    os.font <- grDevices::X11Fonts()$sans
    os.font.ten.bold <- paste0(os.font, " 10 bold")
    os.font.twelve.bold <- paste0(os.font, " 12 bold")
  } else {
    os.font <- "Arial"
    os.font.ten.bold <- "Arial 10 bold"
    os.font.twelve.bold <- "Arial 12 bold"
  }
  
  ## components of the form -------------------------------------
  ## tcltk test specifically relevant for Mac
  tcltk.test <- try(test <- tcltk::tktoplevel(bg = "#05F2DB"), silent = TRUE)
  tcltk.test2 <- try(tcltk::tkdestroy(test), silent = TRUE)
  if (inherits(tcltk.test,"try-error")){
    out_msg <- c(out_msg, paste("Package tcltk is not working properly.",
                                "If you are on a Mac try installing the latest version of xquartz to use tcltk correctly."))
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
  } else {
    ## if tcltk works continue
    PDE.globals$ttanalyzer <- tcltk::tktoplevel()
    notebook <- tcltk::ttknotebook(PDE.globals$ttanalyzer)
    tcltk::tkwm.geometry(PDE.globals$ttanalyzer, "+0+0")
    tcltk::tkwm.title(PDE.globals$ttanalyzer, "PDE analyzer - Choose your variables")
    
    ## style ------------------------------------------------------
    themes <- try(tcltk2::tk2theme.list(), silent = TRUE)
    if (!inherits(themes, "try-error")) {
      if ("vista" %in% themes) {
        # This must be aquaTk on a Mac
        try(tcltk2::tk2theme("vista"), silent = TRUE)
      } else if ("clam" %in% themes) {
        try(tcltk2::tk2theme("clam"), silent = TRUE)
      } else if ("aquaTk" %in% themes) {
        # This must be Vista or Win 7
        try(tcltk2::tk2theme("aquaTk"), silent = TRUE)
      }
    }
    
    ## min size so that no buttons disappear vertically
    tcltk::tkwm.minsize(PDE.globals$ttanalyzer, 620, 335)
    
    
    ## frames --------------------------------------------
    ## right frame
    r <- tcltk::tkframe(PDE.globals$ttanalyzer)
    ## line 1 with buttons
    l1 <- tcltk::tkframe(PDE.globals$ttanalyzer)
    ## line 37-39 with extracted table if applicable
    l37 <- tcltk::tkframe(PDE.globals$ttanalyzer)
    l38 <- tcltk::tkframe(PDE.globals$ttanalyzer)
    l39 <- tcltk::tkframe(PDE.globals$ttanalyzer)
    ## line in_output (line6-10) with buttons
    in_output_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                    bg = "#05F2C7",
                                    class="Notebook")
    l7 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## pdfs
    l7.2 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## pdfs
    l9 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## outputfolder
    l9.2 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## outputfolder
    l10 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## out.table.format
    l10.2 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## out.table.format
    l11 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## step 4 label
    l11.2 <- tcltk::tkframe(in_output_tab, bg = "#05F2C7")  ## step 5 label
    ## line serachwords
    searchwords_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                      bg = "#05F2C7",
                                      class="Notebook")
    l8 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## whattoextr
    l19 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## extract all tables?
    l20 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## search.words
    l20.2 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## search.words
    l21 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## ignore.case.sw
    l22 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## context.caption
    l23 <- tcltk::tkframe(searchwords_tab, bg = "#05F2C7")  ## eval.abbrevs.caption
    
    
    ## line filterwords
    filterwords_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                      bg = "#05F2C7",
                                      class="Notebook")
    l15 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## filter words?
    l16 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## filter.words
    l16.2 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## filter.words
    l17 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## ignore.case.fw
    l18.1 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## filter.word.times.caption
    l18.2 <- tcltk::tkframe(filterwords_tab, bg = "#05F2C7")  ## filter.word.times.caption
    
    ## line paras (line11-23) with buttons
    paras_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                bg = "#05F2C7",
                                class="Notebook")
    l12 <- tcltk::tkframe(paras_tab, bg = "#05F2C7")  ## table.heading.words
    l12.2 <- tcltk::tkframe(paras_tab, bg = "#05F2C7")  ## table.heading.words
    l13 <- tcltk::tkframe(paras_tab, bg = "#05F2C7")  ## ignore.case.th
    l14.1 <- tcltk::tkframe(paras_tab, bg = "#05F2C7")  ## dev_x.caption
    l14.2 <- tcltk::tkframe(paras_tab, bg = "#05F2C7")  ## dev_y.caption
    ## line docus (line24-28) with buttons
    docus_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                bg = "#05F2C7",
                                class="Notebook")
    l25 <- tcltk::tkframe(docus_tab, bg = "#05F2C7")  ## write table values
    l26 <- tcltk::tkframe(docus_tab, bg = "#05F2C7")  ## exp.nondetc.tabs
    l27 <- tcltk::tkframe(docus_tab, bg = "#05F2C7")  ## write.tab.doc.file
    l28 <- tcltk::tkframe(docus_tab, bg = "#05F2C7")  ## write.txt.doc.file
    l29 <- tcltk::tkframe(docus_tab, bg = "#05F2C7")  ## delete
    
    no.output_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                    bg = "#05F2C7",
                                    class="Notebook")
    
    l30 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table
    l31 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table 1
    l32 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table 2
    l33 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table 3
    l34 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table 4
    l35 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## no table 5
    l36 <- tcltk::tkframe(no.output_tab, bg = "#05F2C7")  ## url
    
    contact_tab <- tcltk::tkframe(notebook, borderwidth = 1, relief = "solid",
                                  bg = "#05F2C7",
                                  class="Notebook")
    
    
    e <- tcltk::tkframe(PDE.globals$ttanalyzer)
    le <- tcltk::tkframe(e)  ## progress bar
    
    ## test os.font ------------------------------------------------------------------------
    res <- try(tcltk2::tk2label(l1, text = "test", font = os.font.ten.bold), silent = TRUE)
    if (inherits(res[1],"try-error")) {
      out_msg <- c(out_msg, "There is an error with the allocation of tcl system fonts.")
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      os.font <- ""
      os.font.ten.bold <- ""
      os.font.twelve.bold <- ""
    }
    
    ## slider --------------------------------------------------------
    
    ## A function that changes the label
    ## paras l14.1.dev_x slider
    l14.1.onChange <- function(...) {
      l14.1.value <- as.integer(tcltk::tclvalue(dev_x.var))
      l14.1.entry <- sprintf("%s", l14.1.value)
      tcltk::tclvalue(dev_x.var) <- l14.1.entry
    }
    ## Add the slider
    l14.1.dev_x.slider <- tcltk2::tk2scale(l14.1, from = 0, to = 200,
                                           variable = dev_x.var, orient = "horizontal",
                                           length = 100, command = l14.1.onChange)
    
    ## A function that changes the label
    ## paras l14.2.dev_y slider
    l14.2.onChange <- function(...) {
      l14.2.value <- as.integer(tcltk::tclvalue(dev_y.var))
      l14.2.entry <- sprintf("%s", l14.2.value)
      tcltk::tclvalue(dev_y.var) <- l14.2.entry
      tcltk::tclvalue(dynamic.dev_y.var) <- "0"
    }
    ## Add the slider
    l14.2.dev_y.slider <- tcltk2::tk2scale(l14.2, from = 0, to = 200,
                                           variable = dev_y.var, orient = "horizontal",
                                           length = 100, command = l14.2.onChange)
    
    
    ## l18.1.filter.word.times slider A function that
    ## changes the label
    l18.1.onChange <- function(...) {
      if (tcltk::tclvalue(percentage.fw.var) == "1"){
        l18.1.value <- as.integer(tcltk::tclvalue(filter.word.times_value.var))
        tcltk::tclvalue(filter.word.times.var) <- paste0(l18.1.value/100,"%")
      } else {
        l18.1.value <- as.integer(tcltk::tclvalue(filter.word.times_value.var))
        l18.1.entry <- sprintf("%s", l18.1.value)
        tcltk::tclvalue(filter.word.times.var) <- l18.1.entry
        tcltk::tclvalue(filter.word.times_value.var) <- l18.1.entry
      }
    }
    ## Add the slider
    l18.1.filter.word.times.slider <- tcltk2::tk2scale(l18.1,
                                                     from = 0, to = 500, variable = filter.word.times_value.var,
                                                     orient = "horizontal", length = 100, command = l18.1.onChange)
    
    ## l22.context slider A function that changes the
    ## label
    l22.onChange <- function(...) {
      l22.value <- as.integer(tcltk::tclvalue(context.var))
      l22.entry <- sprintf("%s", l22.value)
      tcltk::tclvalue(context.var) <- l22.entry
    }
    ## Add the slider
    l22.context.slider <- tcltk2::tk2scale(l22, from = 0,
                                           to = 100, variable = context.var, orient = "horizontal",
                                           length = 100, command = l22.onChange)
    
    ## checkbuttons -----------------------------------------------------
    change.dynamic.dev_y <- function() {
      ## if uncheck
      if (tcltk::tclvalue(dynamic.dev_y.var) == "0"){
        tcltk::tclvalue(dev_y.var) <- "1"
      } else {
        tcltk::tclvalue(dev_y.var) <- "9999"
      }
    }
    
    l14.2.dynamic.dev_y.cbtn <- tcltk::tkcheckbutton(l14.2, variable = dynamic.dev_y.var, 
                                                     text = "dynamic",
                                                     state = "normal", background = "#05F2C7", 
                                                     command = change.dynamic.dev_y)
    change.pecentage.fw <- function() {
      if (tcltk::tclvalue(percentage.fw.var) == "1"){
        tcltk::tclvalue(filter.word.times.var) <- paste0(as.numeric(tcltk::tclvalue(filter.word.times.var))/100,"%")
        tcltk::tkconfigure(l18.1.filter.word.times.slider,to = 2000)
      } else {
        tcltk::tclvalue(filter.word.times.var) <- as.numeric(gsub("%","",tcltk::tclvalue(filter.word.times.var)))*100
        tcltk::tkconfigure(l18.1.filter.word.times.slider,to = 500)
      }
    }
    
    l18.1.percentage.fw.cbtn <- tcltk::tkcheckbutton(l18.1, variable = percentage.fw.var, 
                                                text = "%",
                                                state = "normal", background = "#05F2C7", 
                                                command = change.pecentage.fw)
    change.regex.sw <- function() {
      
    }
    
    l20.2.regex.sw.cbtn <- tcltk::tkcheckbutton(l20.2, variable = regex.sw.var, 
                                                text = "Regex",
                                                state = "normal", background = "#05F2C7", 
                                                command = change.regex.sw)
    change.regex.fw <- function() {
    }
    
    l16.2.regex.fw.cbtn <- tcltk::tkcheckbutton(l16.2, variable = regex.fw.var, 
                                                text = "Regex",
                                                state = "normal", background = "#05F2C7", 
                                                command = change.regex.fw)
    
    
    ## No output tab -----------------------------------------------------
    if (Sys.info()["sysname"] == "Windows") {
      os.font <- "Arial"
      os.font.ten <- "Arial 10"
      os.font.ten.bold <- "Arial 10 bold"
      os.font.twelve.bold <- "Arial 12 bold"
      os.font.ten.underlined <- "Arial 10 underline"
    } else if (Sys.info()["sysname"] == "Darwin") {
      os.font <- "Helvetica"
      os.font.ten <- "Helvetica 10"
      os.font.ten.bold <- "Helvetica 10 bold"
      os.font.twelve.bold <- "Helvetica 12 bold"
      os.font.ten.underlined <- "Helvetica 10 underline"
    } else if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "SunOS") {
      os.font <- grDevices::X11Fonts()$sans
      os.font.ten <- paste0(os.font, " 10")
      os.font.ten.bold <- paste0(os.font, " 10 bold")
      os.font.twelve.bold <- paste0(os.font, " 12 bold")
      os.font.ten.underlined <- paste0(os.font, " 10 underline")
    } else {
      os.font <- "Arial"
      os.font.ten <- "Arial 10"
      os.font.ten.bold <- "Arial 10 bold"
      os.font.twelve.bold <- "Arial 12 bold"
      os.font.ten.underlined <- "Arial 10 underline"
    }
    
    l30.no.tables.label <- tcltk2::tk2label(l30, text = "No tables?", background = "#05F2C7",
                                            font = os.font.twelve.bold)
    l31.no.tables.one.label <- tcltk2::tk2label(l31, text = paste0("1) Make your your tables have",
                                                                   " a standard scientific format with \"TABLE\"",
                                                                   ", \"TAB\", \"Table\" or \"table\" plus number.",
                                                                   " Use custom table headings under the Parameters ",
                                                                   "Tab if necessary"), 
                                                background = "#05F2C7",
                                                wraplength = 620 - 50)
    l32.no.tables.two.label <- tcltk2::tk2label(l32, text = paste0("2) Check that your PDF file is not secured (read-only)",
                                                                   ", consists only out of images, or has rotated pages. ",
                                                                   "Tables on rotated pages will be exported as png if the ",
                                                                   "second option is chosen in the Documentation Tab."), 
                                                background = "#05F2C7",
                                                wraplength = 620 - 50)
    l33.no.tables.three.label <- tcltk2::tk2label(l33, text = paste0("3) If you selected filter words, ensure that your PDF",
                                                                     " files includes enough filter words not to be excluded",
                                                                     " from analyses (double-check your filter word and",
                                                                     " case-sensivity selections in the Filter Words Tab.)"), 
                                                  background = "#05F2C7",
                                                  wraplength = 620 - 50)
    l34.no.tables.four.label <- tcltk2::tk2label(l34, text = paste0("4) If you selected search words, ensure that your tables",
                                                                    " include at least one of the search words (double-check",
                                                                    " your search word and case-sensivity selections in the",
                                                                    " Search Words Tab.)"), 
                                                 background = "#05F2C7",
                                                 wraplength = 620 - 50)
    l35.no.tables.five.label <- tcltk2::tk2label(l35, text = paste0("5) For presisting problems look for the maintainer",
                                                                    " contact information on the PDE CRAN page: "), 
                                                 background = "#05F2C7",
                                                 wraplength = 620 - 50)
    
    
    l36.url.label <- tcltk2::tk2label(l36, text = paste0("https://cran.r-project.org/web/packages/PDE/index.html"), 
                                      background = "#05F2C7", foreground="blue", cursor="hand2")
    
    tcltk::tkbind(l36.url.label, "<Button-1>",
                  function() {
                    utils::browseURL("https://cran.r-project.org/web/packages/PDE/index.html")
                  })
    
    
    ## adjust the label sizes (not working)------
    # w.width_old <- 620
    # tcltk::tkbind(PDE.globals$ttanalyzer, "<Configure>", function(W) {
    #   w.width <- as.integer(tkwinfo("width",W))
    #   if ((w.width_old - w.width)^2 > 2500 && w.width > 620){
    #     tcltk::tkconfigure(l31.no.tables.one.label, wraplength = w.width - 50)
    #     tcltk::tkconfigure(l32.no.tables.two.label, wraplength = w.width - 50)
    #     tcltk::tkconfigure(l33.no.tables.three.label, wraplength = w.width - 50)
    #     tcltk::tkconfigure(l34.no.tables.four.label, wraplength = w.width - 50)
    #     tcltk::tkconfigure(l35.no.tables.five.label, wraplength = w.width - 50)
    #     w.width_old <- w.width
    #   } else if (w.width == 500){
    #     tcltk::tkconfigure(l31.no.tables.one.label, wraplength = 620 - 50)
    #     tcltk::tkconfigure(l32.no.tables.two.label, wraplength = 620 - 50)
    #     tcltk::tkconfigure(l33.no.tables.three.label, wraplength = 620 - 50)
    #     tcltk::tkconfigure(l34.no.tables.four.label, wraplength = 620 - 50)
    #     tcltk::tkconfigure(l35.no.tables.five.label, wraplength = 620 - 50)
    #   }
    # })
    
    
    
    
    ## labels -----------------------------------------------------------
    
    ## line in_output (line6-10) with buttons
    l7.step1.label <- tcltk2::tk2label(l7, text = "Step 1:",
                                       font = os.font.ten.bold,
                                       background = "#05F2C7")
    l7.pdfs.label <- tcltk2::tk2label(l7, text = "Choose the PDF(s)/folder",
                                      font = os.font.ten,
                                      background = "#05F2C7")
    l8.whattoextr.label <- tcltk2::tk2label(l8, text = "Choose what to extract from the PDF files",
                                            background = "#05F2C7")
    l8.whattoextr.sentences.label <- tcltk2::tk2label(l8, text = "sentences",
                                                      background = "#05F2C7")
    l8.whattoextr.tables.label <- tcltk2::tk2label(l8, text = "tables",
                                                   background = "#05F2C7")
    l8.whattoextr.both.label <- tcltk2::tk2label(l8, text = "both",
                                                 background = "#05F2C7")
    l9.step2.label <- tcltk2::tk2label(l9, text = "Step 2:",
                                       font = os.font.ten.bold,
                                       background = "#05F2C7")
    l9.outputfolder.label <- tcltk2::tk2label(l9, text = "Choose the output folder",
                                              font = os.font.ten,
                                              background = "#05F2C7")
    l10.step3.label <- tcltk2::tk2label(l10, text = "Step 3 (optional):",
                                        font = os.font.ten.bold,
                                        background = "#05F2C7")
    l10.out.table.format.label <- tcltk2::tk2label(l10, text = "Choose the output format",
                                                   font = os.font.ten,
                                                   background = "#05F2C7")
    
    l11.step4.label <- tcltk2::tk2label(l11, text = "Step 4 (optional):",
                                        font = os.font.ten.bold,
                                        background = "#05F2C7")
    l11.step4_2.label <- tcltk2::tk2label(l11, text = "Adjust options in the tabs above.",
                                          font = os.font.ten,
                                          background = "#05F2C7")
    
    l11.2.step5.label <- tcltk2::tk2label(l11.2, text = "Step 5:",
                                          font = os.font.ten.bold,
                                          background = "#05F2C7")
    l11.2.step5_2.label <- tcltk2::tk2label(l11.2, text = "Start analysis.",
                                            font = os.font.ten,
                                            background = "#05F2C7")
    
    ## line paras (line11-23) with buttons
    l12.table.heading.words.label <- tcltk2::tk2label(l12,
                                                      text = "Enter all table headings other than \"table\" (separated by ; without extra spaces):",
                                                      background = "#05F2C7")
    l13.ignore.case.th.label <- tcltk2::tk2label(l13, text = "Are the table headings case sensitive (capitalization important)?",
                                                 background = "#05F2C7")
    l13.ignore.case.th.yes.label <- tcltk2::tk2label(l13,
                                                     text = "yes", background = "#05F2C7")
    l13.ignore.case.th.no.label <- tcltk2::tk2label(l13,
                                                    text = "no", background = "#05F2C7")
    l14.1.dev_x.label <- tcltk2::tk2label(l14.1, text = "Enter the pixel deviation where different table columns are considered same:",
                                          background = "#05F2C7")
    l14.2.dev_y.label <- tcltk2::tk2label(l14.2, text = paste0("Enter the pixel deviation where different table value heights are considered\n",
                                                               "the same row (enter 9999 for dynamic detection based on font size):"),
                                          background = "#05F2C7")
    l15.filter.words.yes.label <- tcltk2::tk2label(l15, text = "yes",
                                                   background = "#05F2C7")
    l15.filter.words.no.label <- tcltk2::tk2label(l15, text = "no",
                                                  background = "#05F2C7")
    l15.filter.words.yesno.label <- tcltk2::tk2label(l15, text = "Filtering of PDF files according to the presence of filter words?",
                                                     background = "#05F2C7")
    l16.filter.words.label <- tcltk2::tk2label(l16, text = "Filter words (separated by ; without extra spaces):",
                                               background = "#05F2C7")
    l17.ignore.case.fw.label <- tcltk2::tk2label(l17, text = "Are the filter words case sensitive (capitalization important)?",
                                                 background = "#05F2C7")
    l17.ignore.case.fw.yes.label <- tcltk2::tk2label(l17, text = "yes",
                                                     background = "#05F2C7")
    l17.ignore.case.fw.no.label <- tcltk2::tk2label(l17, text = "no",
                                                    background = "#05F2C7")
    l18.1.filter.word.times.label <- tcltk2::tk2label(l18.1,
                                                    text = "Min. number of words from filter word list required to be detected in the PDF file:",
                                                    background = "#05F2C7")
    l18.2.fw.label <- tcltk2::tk2label(l18.2, text = "Copy/move PDF files according to the presence of filter words?",
                                                 background = "#05F2C7")
    l18.2.fw.nocpymv.label <- tcltk2::tk2label(l18.2, text = "no copy/move",
                                                     background = "#05F2C7")
    l18.2.fw.cpy.label <- tcltk2::tk2label(l18.2, text = "copy",
                                               background = "#05F2C7")
    l18.2.fw.mv.label <- tcltk2::tk2label(l18.2, text = "move",
                                               background = "#05F2C7")
    l19.extract.all.tables.label <- tcltk2::tk2label(l19, text = "Extract all tables or only ones with search words?",
                                                     background = "#05F2C7")
    l19.extract.all.tables.all.label <- tcltk2::tk2label(l19,
                                                         text = "all", background = "#05F2C7")
    l19.extract.all.tables.searchwords.label <- tcltk2::tk2label(l19,
                                                                 text = "search words only", background = "#05F2C7")
    l20.search.words.label <- tcltk2::tk2label(l20, text = "Search words (separated by ; without extra spaces):",
                                               background = "#05F2C7")
    l21.ignore.case.sw.yes.label <- tcltk2::tk2label(l21, text = "yes",
                                                     background = "#05F2C7")
    l21.ignore.case.sw.no.label <- tcltk2::tk2label(l21, text = "no",
                                                    background = "#05F2C7")
    l21.ignore.case.sw.label <- tcltk2::tk2label(l21, text = "Are the search words case sensitive (capitalization important)?",
                                                 background = "#05F2C7")
    l22.context.label <- tcltk2::tk2label(l22, text = "Number of sentences extracted before and after the sentence with the search word:",
                                          background = "#05F2C7")
    l23.eval.abbrevs.label <- tcltk2::tk2label(l23, text = "Should abbreviations of the search word be replaced by the search word during the analysis?",
                                               background = "#05F2C7")
    l23.eval.abbrevs.yes.label <- tcltk2::tk2label(l23, text = "yes",
                                                   background = "#05F2C7")
    l23.eval.abbrevs.no.label <- tcltk2::tk2label(l23, text = "no",
                                                  background = "#05F2C7")
    # (line24-28) with buttons
    l25.write.table.locations.label <- tcltk2::tk2label(l25, text = "Should the table values (coordinates within the files) be written in an output file?",
                                                        background = "#05F2C7")
    l25.write.table.locations.yes.label <- tcltk2::tk2label(l25,
                                                            text = "yes", background = "#05F2C7")
    l25.write.table.locations.no.label <- tcltk2::tk2label(l25,
                                                           text = "no", background = "#05F2C7")
    l26.exp.nondetc.tabs.label <- tcltk2::tk2label(l26, text = "Should the tables with problems exported as pngs?",
                                                   background = "#05F2C7")
    l26.exp.nondetc.tabs.yes.label <- tcltk2::tk2label(l26,
                                                       text = "yes", background = "#05F2C7")
    l26.exp.nondetc.tabs.no.label <- tcltk2::tk2label(l26,
                                                      text = "no", background = "#05F2C7")
    l27.write.tab.doc.file.label <- tcltk2::tk2label(l27, text = "Should a table documentation file be created (file that reports if there was no table found)?",
                                                     background = "#05F2C7")
    l27.write.tab.doc.file.yes.label <- tcltk2::tk2label(l27,
                                                         text = "yes", background = "#05F2C7")
    l27.write.tab.doc.file.no.label <- tcltk2::tk2label(l27,
                                                        text = "no", background = "#05F2C7")
    l28.write.txt.doc.file.label <- tcltk2::tk2label(l28, text = "Should a sentence documentation file be created (file that reports if there was no sentence found)?",
                                                     background = "#05F2C7")
    l28.write.txt.doc.file.yes.label <- tcltk2::tk2label(l28,
                                                         text = "yes", background = "#05F2C7")
    l28.write.txt.doc.file.no.label <- tcltk2::tk2label(l28,
                                                        text = "no", background = "#05F2C7")
    l29.delete.label <- tcltk2::tk2label(l29, text = "Should the intermediate files be kept (not deleted)?",
                                         background = "#05F2C7")
    l29.delete.yes.label <- tcltk2::tk2label(l29, text = "yes",
                                             background = "#05F2C7")
    l29.delete.no.label <- tcltk2::tk2label(l29, text = "no",
                                            background = "#05F2C7")
    
    l37.wrap.label <- tcltk2::tk2label(l37, text = "wrap", background = "#05F2C7")
    l37.nowrap.label <- tcltk2::tk2label(l37, text = "don't wrap (for window resizing)",
                                         background = "#05F2C7")
    ## line (line29) with buttons
    PDE.globals$le.progress.textbox <- tcltk2::tk2combobox(le, values = c("",""), textvariable = "", justify = "center")
    
    ## entry boxes -----------------------------------------------
    
    ## in_output
    l7.2.pdfs.entry <- tcltk2::tk2entry(l7.2, textvariable = pdfs.var,
                                        width = 10)
    l9.2.outputfolder.entry <- tcltk2::tk2entry(l9.2, textvariable = outputfolder.var,
                                                width = 10)
    ## paras
    l12.2.table.heading.words.entry <- tcltk2::tk2entry(l12.2,
                                                        textvariable = table.heading.words.var, width = 10)
    l14.1.dev_x.entry <- tcltk2::tk2entry(l14.1, textvariable = dev_x.var,
                                          width = 4)
    l14.2.dev_y.entry <- tcltk2::tk2entry(l14.2, textvariable = dev_y.var,
                                          width = 4)
    l16.2.filter.words.entry <- tcltk2::tk2entry(l16.2, textvariable = filter.words.var,
                                                 width = 10)
    l18.1.filter.word.times.entry <- tcltk2::tk2entry(l18.1,
                                                    textvariable = filter.word.times.var, width = 6)
    l20.2.search.words.entry <- tcltk2::tk2entry(l20.2, textvariable = search.words.var,
                                                 width = 10)
    l22.context.entry <- tcltk2::tk2entry(l22, textvariable = context.var,
                                          width = 4)
    ## docus
    
    ## radio buttons 1 --------------------------------------------
    
    l13.ignore.case.th.yes.rb <- tcltk::tkradiobutton(l13,
                                                      variable = ignore.case.th.var, value = "FALSE",
                                                      background = "#05F2C7")
    l13.ignore.case.th.no.rb <- tcltk::tkradiobutton(l13,
                                                     variable = ignore.case.th.var, value = "TRUE",
                                                     background = "#05F2C7")
    l17.ignore.case.fw.yes.rb <- tcltk::tkradiobutton(l17,
                                                      variable = ignore.case.fw.var, value = "FALSE",
                                                      background = "#05F2C7")
    l17.ignore.case.fw.no.rb <- tcltk::tkradiobutton(l17,
                                                     variable = ignore.case.fw.var, value = "TRUE",
                                                     background = "#05F2C7")
    
    l18.2.fw.nocpymv.rb <- tcltk::tkradiobutton(l18.2,
                                                      variable = copy_move_pdfs.var, value = "nocpymv",
                                                      background = "#05F2C7")
    l18.2.fw.cpy.rb <- tcltk::tkradiobutton(l18.2,
                                                     variable = copy_move_pdfs.var, value = "cpy",
                                                     background = "#05F2C7")
    l18.2.fw.mv.rb <- tcltk::tkradiobutton(l18.2,
                                                        variable = copy_move_pdfs.var, value = "mv",
                                                        background = "#05F2C7")
    
    l15.onyes.click <- function() {
      tcltk::tkconfigure(l16.filter.words.label, state = "normal")
      tcltk::tkconfigure(l16.2.filter.words.entry, state = "normal")
      tcltk::tkconfigure(l16.2.regex.fw.cbtn, state = "normal")
      tcltk::tkconfigure(l17.ignore.case.fw.label, state = "normal")
      tcltk::tkconfigure(l17.ignore.case.fw.yes.rb, state = "normal")
      tcltk::tkconfigure(l17.ignore.case.fw.yes.label, state = "normal")
      tcltk::tkconfigure(l17.ignore.case.fw.no.label, state = "normal")
      tcltk::tkconfigure(l17.ignore.case.fw.no.rb, state = "normal")
      tcltk::tkconfigure(l18.1.percentage.fw.cbtn, state = "normal")
      tcltk::tkconfigure(l18.1.filter.word.times.label,
                         state = "normal")
      tcltk::tkconfigure(l18.1.filter.word.times.entry,
                         state = "normal")
      tcltk::tkconfigure(l18.1.filter.word.times.slider,
                         to = "500")
      tcltk::tclvalue(filter.word.times.var) = "0.2%"
      tcltk::tclvalue(percentage.fw.var) = "1"
      tcltk::tkconfigure(l18.2.fw.label,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.nocpymv.label,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.nocpymv.rb,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.cpy.label,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.cpy.rb,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.mv.label,
                         state = "normal")
      tcltk::tkconfigure(l18.2.fw.mv.rb,
                         state = "normal")
    }
    l15.onno.click <- function() {
      tcltk::tkconfigure(l16.filter.words.label, state = "disabled")
      tcltk::tkconfigure(l16.2.filter.words.entry, state = "disabled")
      tcltk::tkconfigure(l16.2.regex.fw.cbtn, state = "disabled")
      tcltk::tkconfigure(l17.ignore.case.fw.label, state = "disabled")
      tcltk::tkconfigure(l17.ignore.case.fw.yes.rb, state = "disabled")
      tcltk::tkconfigure(l17.ignore.case.fw.no.rb, state = "disabled")
      tcltk::tkconfigure(l17.ignore.case.fw.yes.label, state = "disabled")
      tcltk::tkconfigure(l17.ignore.case.fw.no.label, state = "disabled")
      tcltk::tkconfigure(l18.1.percentage.fw.cbtn, state = "disabled")
      tcltk::tkconfigure(l18.1.filter.word.times.label,
                         state = "disabled")
      tcltk::tkconfigure(l18.1.filter.word.times.entry,
                         state = "disabled")
      tcltk::tclvalue(filter.word.times.var) <- "0%"
      tcltk::tclvalue(percentage.fw.var) = "1"
      tcltk::tkconfigure(l18.1.filter.word.times.slider,
                         to = "0")
      tcltk::tkconfigure(l18.2.fw.label,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.nocpymv.label,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.nocpymv.rb,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.cpy.label,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.cpy.rb,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.mv.label,
                         state = "disabled")
      tcltk::tkconfigure(l18.2.fw.mv.rb,
                         state = "disabled")
      
      ## delete all filterwords
      tcltk::tclvalue(filter.words.var) <- ""
    }
    l15.onno.click()
    l15.filter.words.yes.rb <- tcltk::tkradiobutton(l15, variable = l15.filter.words.rbValue,
                                                    value = "yes", command = l15.onyes.click, background = "#05F2C7")
    l15.filter.words.no.rb <- tcltk::tkradiobutton(l15, variable = l15.filter.words.rbValue,
                                                   value = "no", command = l15.onno.click, background = "#05F2C7")
    
    l21.ignore.case.sw.yes.rb <- tcltk::tkradiobutton(l21,
                                                      variable = ignore.case.sw.var, value = "FALSE",
                                                      background = "#05F2C7")
    l21.ignore.case.sw.no.rb <- tcltk::tkradiobutton(l21,
                                                     variable = ignore.case.sw.var, value = "TRUE",
                                                     background = "#05F2C7")
    
    
    l19.onall.click <- function() {
      tcltk::tkconfigure(l20.search.words.label, state = "disabled")
      tcltk::tkconfigure(l20.2.search.words.entry, state = "disabled")
      tcltk::tkconfigure(l20.2.regex.sw.cbtn, state = "disabled")
      tcltk::tkconfigure(l21.ignore.case.sw.label, state = "disabled")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.label, state = "disabled")
      tcltk::tkconfigure(l21.ignore.case.sw.no.label, state = "disabled")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.rb, state = "disabled")
      tcltk::tkconfigure(l21.ignore.case.sw.no.rb, state = "disabled")
      tcltk::tkconfigure(l23.eval.abbrevs.yes.rb, state = "disabled")
      tcltk::tkconfigure(l23.eval.abbrevs.no.rb, state = "disabled")
      tcltk::tkconfigure(l23.eval.abbrevs.yes.label, state = "disabled")
      tcltk::tkconfigure(l23.eval.abbrevs.no.label, state = "disabled")
      ## delete all filterwords
      tcltk::tclvalue(search.words.var) <- ""
    }
    l19.onsearchwords.click <- function() {
      tcltk::tkconfigure(l20.search.words.label, state = "normal")
      tcltk::tkconfigure(l20.2.search.words.entry, state = "normal")
      tcltk::tkconfigure(l20.2.regex.sw.cbtn, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.rb, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.rb, state = "normal")
      tcltk::tkconfigure(l23.eval.abbrevs.yes.rb, state = "normal")
      tcltk::tkconfigure(l23.eval.abbrevs.no.rb, state = "normal")
      tcltk::tkconfigure(l23.eval.abbrevs.yes.label, state = "normal")
      tcltk::tkconfigure(l23.eval.abbrevs.no.label, state = "normal")
    }
    l19.extract.all.tables.all.rb <- tcltk::tkradiobutton(l19,
                                                          variable = l19.rbValue, value = "all", command = l19.onall.click,
                                                          background = "#05F2C7")
    l19.extract.all.tables.searchwords.rb <- tcltk::tkradiobutton(l19,
                                                                  variable = l19.rbValue, value = "searchwords",
                                                                  command = l19.onsearchwords.click, background = "#05F2C7")
    
    l23.eval.abbrevs.yes.rb <- tcltk::tkradiobutton(l23,
                                                    variable = eval.abbrevs.var, value = "TRUE",
                                                    background = "#05F2C7")
    l23.eval.abbrevs.no.rb <- tcltk::tkradiobutton(l23,
                                                   variable = eval.abbrevs.var, value = "FALSE",
                                                   background = "#05F2C7")
    
    l25.write.table.locations.yes.rb <- tcltk::tkradiobutton(l25,
                                                             variable = write.table.locations.var, value = "TRUE",
                                                             background = "#05F2C7")
    l25.write.table.locations.no.rb <- tcltk::tkradiobutton(l25,
                                                            variable = write.table.locations.var, value = "FALSE",
                                                            background = "#05F2C7")
    
    l26.exp.nondetc.tabs.yes.rb <- tcltk::tkradiobutton(l26,
                                                        variable = exp.nondetc.tabs.var, value = "TRUE",
                                                        background = "#05F2C7")
    l26.exp.nondetc.tabs.no.rb <- tcltk::tkradiobutton(l26,
                                                       variable = exp.nondetc.tabs.var, value = "FALSE",
                                                       background = "#05F2C7")
    
    l27.write.tab.doc.file.yes.rb <- tcltk::tkradiobutton(l27,
                                                          variable = write.tab.doc.file.var, value = "TRUE",
                                                          background = "#05F2C7")
    l27.write.tab.doc.file.no.rb <- tcltk::tkradiobutton(l27,
                                                         variable = write.tab.doc.file.var, value = "FALSE",
                                                         background = "#05F2C7")
    
    l28.write.txt.doc.file.yes.rb <- tcltk::tkradiobutton(l28,
                                                          variable = write.txt.doc.file.var, value = "TRUE",
                                                          background = "#05F2C7")
    l28.write.txt.doc.file.no.rb <- tcltk::tkradiobutton(l28,
                                                         variable = write.txt.doc.file.var, value = "FALSE",
                                                         background = "#05F2C7")
    
    l29.delete.yes.rb <- tcltk::tkradiobutton(l29, variable = delete.var,
                                              value = "FALSE", background = "#05F2C7")
    l29.delete.no.rb <- tcltk::tkradiobutton(l29, variable = delete.var,
                                             value = "TRUE", background = "#05F2C7")
    
    l8.ontxt.click <- function() {
      tcltk::tkconfigure(l12.table.heading.words.label, state = "disabled")
      tcltk::tkconfigure(l12.2.table.heading.words.entry, state = "disabled")
      tcltk::tkconfigure(l13.ignore.case.th.label, state = "disabled")
      tcltk::tkconfigure(l13.ignore.case.th.yes.rb, state = "disabled")
      tcltk::tkconfigure(l13.ignore.case.th.no.rb, state = "disabled")
      tcltk::tkconfigure(l13.ignore.case.th.yes.label, state = "disabled")
      tcltk::tkconfigure(l13.ignore.case.th.no.label, state = "disabled")
      tcltk::tkconfigure(l14.1.dev_x.label, state = "disabled")
      tcltk::tkconfigure(l14.1.dev_x.entry, state = "disabled")
      tcltk::tclvalue(dev_x.var) <- "0"
      tcltk::tkconfigure(l14.1.dev_x.slider, to = "0")
      tcltk::tkconfigure(l14.2.dev_y.label, state = "disabled")
      tcltk::tkconfigure(l14.2.dev_y.entry, state = "disabled")
      tcltk::tclvalue(dev_y.var) <- "0"
      tcltk::tkconfigure(l14.2.dev_y.slider, to = "0")
      tcltk::tclvalue(dynamic.dev_y.var) <- "0"
      tcltk::tkconfigure(l14.2.dynamic.dev_y.cbtn, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.label, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.all.rb, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.all.label, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.label, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.rb, state = "disabled")
      tcltk::tclvalue(l19.rbValue) <- "searchwords"
      tcltk::tkconfigure(l20.2.search.words.entry, state = "normal")
      tcltk::tkconfigure(l20.2.regex.sw.cbtn, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.rb, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.label, state = "disabled")
      tcltk::tkconfigure(l25.write.table.locations.yes.rb, state = "disabled")
      tcltk::tkconfigure(l25.write.table.locations.no.rb, state = "disabled")
      tcltk::tkconfigure(l25.write.table.locations.yes.label, state = "disabled")
      tcltk::tkconfigure(l25.write.table.locations.no.label, state = "disabled")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.label, state = "disabled")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.rb, state = "disabled")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.rb, state = "disabled")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.label, state = "disabled")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.label, state = "disabled")
      tcltk::tkconfigure(l27.write.tab.doc.file.label, state = "disabled")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.rb, state = "disabled")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.rb, state = "disabled")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.label, state = "disabled")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.label, state = "disabled")
      tcltk::tkconfigure(l20.search.words.label, state="normal")
      tcltk::tkconfigure(l22.context.label, state = "normal")
      tcltk::tkconfigure(l22.context.entry, state = "normal")
      tcltk::tkconfigure(l22.context.slider, to = "100")
      tcltk::tkconfigure(l23.eval.abbrevs.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.rb, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.rb, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.label, state = "normal")
    }
    l8.ontab.click <- function() {
      tcltk::tkconfigure(l12.table.heading.words.label, state = "normal")
      tcltk::tkconfigure(l12.2.table.heading.words.entry, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.label, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.yes.rb, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.no.rb, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.yes.label, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.no.label, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.label, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.entry, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.slider, to = "200")
      tcltk::tclvalue(dev_x.var) <- "20"
      tcltk::tclvalue(dev_y.var) <- "9999"
      tcltk::tclvalue(dynamic.dev_y.var) <- "1"
      tcltk::tkconfigure(l14.2.dev_y.label, state = "normal")
      tcltk::tkconfigure(l14.2.dev_y.entry, state = "normal")
      tcltk::tkconfigure(l14.2.dev_y.slider, to = "200")
      tcltk::tkconfigure(l14.2.dynamic.dev_y.cbtn, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.all.rb, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.all.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.label, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.yes.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.no.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.yes.label, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.no.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.rb, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.rb, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.rb, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.rb, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.label, state = "normal")
      tcltk::tkconfigure(l22.context.label, state = "disabled")
      tcltk::tkconfigure(l22.context.entry, state = "disabled")
      tcltk::tkconfigure(l23.eval.abbrevs.label, state = "disabled")
      tcltk::tclvalue(context.var) <- "0"
      tcltk::tkconfigure(l22.context.slider, to = "0")
      tcltk::tkconfigure(l28.write.txt.doc.file.label, state = "disabled")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.rb, state = "disabled")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.rb, state = "disabled")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.label, state = "disabled")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.label, state = "disabled")
      ## only accept a list of search words
      tcltk::tkconfigure(l19.extract.all.tables.all.rb, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.all.label, state = "normal")
    }
    l8.ontxtandtab.click <- function() {
      
      tcltk::tkconfigure(l12.table.heading.words.label, state = "normal")
      tcltk::tkconfigure(l12.2.table.heading.words.entry, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.label, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.yes.rb, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.no.rb, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.yes.label, state = "normal")
      tcltk::tkconfigure(l13.ignore.case.th.no.label, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.label, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.entry, state = "normal")
      tcltk::tkconfigure(l14.1.dev_x.slider, to = "200")
      tcltk::tclvalue(dev_x.var) <- "20"
      tcltk::tkconfigure(l14.2.dev_y.label, state = "normal")
      tcltk::tkconfigure(l14.2.dev_y.entry, state = "normal")
      tcltk::tkconfigure(l14.2.dev_y.slider, to = "200")
      tcltk::tclvalue(dev_y.var) <- "9999"
      tcltk::tclvalue(dynamic.dev_y.var) <- "1"
      tcltk::tkconfigure(l14.2.dynamic.dev_y.cbtn, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.all.rb, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.all.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.label, state = "normal")
      tcltk::tkconfigure(l19.extract.all.tables.searchwords.rb, state = "normal")
      tcltk::tclvalue(l19.rbValue) <- "searchwords"
      tcltk::tkconfigure(l20.search.words.label, state="normal")
      tcltk::tkconfigure(l20.2.search.words.entry, state = "normal")
      tcltk::tkconfigure(l20.2.regex.sw.cbtn, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.label, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.yes.rb, state = "normal")
      tcltk::tkconfigure(l21.ignore.case.sw.no.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.label, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.yes.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.no.rb, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.yes.label, state = "normal")
      tcltk::tkconfigure(l25.write.table.locations.no.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.rb, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.rb, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.yes.label, state = "normal")
      tcltk::tkconfigure(l26.exp.nondetc.tabs.no.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.rb, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.rb, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.yes.label, state = "normal")
      tcltk::tkconfigure(l27.write.tab.doc.file.no.label, state = "normal")
      tcltk::tkconfigure(l22.context.label, state = "normal")
      tcltk::tkconfigure(l22.context.entry, state = "normal")
      tcltk::tkconfigure(l22.context.slider, to = "100")
      tcltk::tkconfigure(l23.eval.abbrevs.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.rb, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.rb, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.yes.label, state = "normal")
      tcltk::tkconfigure(l28.write.txt.doc.file.no.label, state = "normal")
      ## only accept a list of search words
      l19.rbValue="searchwords"
      tcltk::tkconfigure(l19.extract.all.tables.all.rb, state = "disabled")
      tcltk::tkconfigure(l19.extract.all.tables.all.label, state = "disabled")
    }
    l8.ontab.click()
    l19.onall.click()
    l8.whattoextr.txt.rb <- tcltk::tkradiobutton(l8, variable = whattoextr.var, 
                                                 value = "txt", command = l8.ontxt.click,background="#05F2C7")
    l8.whattoextr.tab.rb <- tcltk::tkradiobutton(l8, variable = whattoextr.var, 
                                                 value = "tab", command = l8.ontab.click,background="#05F2C7")
    l8.whattoextr.txtandtab.rb <- tcltk::tkradiobutton(l8, variable = whattoextr.var, 
                                                       value = "txtandtab", 
                                                       command = l8.ontxtandtab.click,background="#05F2C7")
    
    
    
    ## combobox list -------------------------------------------------
    
    ## in_output
    l10.2.out.table.format.options <- c(".csv (WINDOWS-1252)", ".csv (macintosh)", ".csv (UTF-8)", 
                                        ".tsv (WINDOWS-1252)",".tsv (macintosh)",".tsv (UTF-8)")
    l10.2.out.table.format.combo <- tcltk2::tk2combobox(l10.2,
                                                        values = l10.2.out.table.format.options, state = "readonly")
    tcltk::tkconfigure(l10.2.out.table.format.combo, textvariable = out.table.format.var)
    
    ## progress bar --------------------------------------------------
    le.progress.bar.pb <- tcltk2::tk2progress(le, value = 0,
                                              maximum = 100, length = 500)
    
    ## radio buttons 2 -----------------------------------------------------
    changewrapping <- function() {
      if (!tcltk::tclvalue(columnnumber.var) == "0") {
        for (c in 0:(as.integer(tcltk::tclvalue(columnnumber.var)) - 1)) {
          tcltk::tcl(l38.table, "columnconfigure",
                     c, wrap = tcltk::tclvalue(wrap.var))
        }
      }
    }
    
    l37.wrap.rb <- tcltk::tkradiobutton(l37, variable = wrap.var, state = "disabled",
                                        value = "1", background = "#05F2C7", command = changewrapping)
    l37.no.wrap.rb <- tcltk::tkradiobutton(l37, variable = wrap.var, state = "disabled",
                                           value = "0", background = "#05F2C7", command = changewrapping)
    
    
    ## buttons -------------------------------------------------------
    
    ## line 1 load a tsv to fill in all the boxes
    load.tsv <- function() {
      
      tcltk::tclvalue(tsv_location.var) <- tcltk::tk_choose.files(default = tcltk::tclvalue(tsv_location.var),
                                                                  caption = "Choose the TSV file",
                                                                  multi = FALSE)
      
      if (length(tcltk::tclvalue(tsv_location.var)) > 0 && tcltk::tclvalue(tsv_location.var) != "") {
        values_table <- utils::read.table(tcltk::tclvalue(tsv_location.var),
                                          sep = "\t", header = TRUE, quote = "\"")
        res <- try(values_table[, "variable"],
                   silent = TRUE)
        if (inherits(res,"try-error")) {
          tcltk::tkmessageBox(title = "Error", type = "ok",
                              icon = "error", message = paste0("Please select a correctly formated TSV file."))
        } else {
          
          ## preset mandatory variables for running search
          ## word
          whattoextr <- sub(" ", "", as.character(values_table[(grep("whattoextr",
                                                                     values_table[, "variable"])), "value"]))
          
          ## correct spelling
          if (whattoextr == "table" || whattoextr ==
              "tables")
            whattoextr <- "tab"
          if (whattoextr == "tableandtxt" || whattoextr ==
              "tableandtext" || whattoextr == "tabandtext" ||
              whattoextr == "textandtab" || whattoextr ==
              "tabandtxt" || whattoextr == "txtandtable" ||
              whattoextr == "tablesandtxt" || whattoextr ==
              "txtandtables")
            whattoextr <- "txtandtab"
          ## test if whattoextr files exist
          if ((whattoextr == "txtandtab" || whattoextr ==
               "tab" || whattoextr == "txt"))
            tcltk::tclvalue(whattoextr.var) <- whattoextr
          
          ## uncheck the correct boxes
          if (whattoextr == "txt")
            l8.ontxt.click()
          if (whattoextr == "tab")
            l8.ontab.click()
          if (whattoextr == "txtandtab")
            l8.ontxtandtab.click()
          
          ## fill the approriate checkmarks
          l15.filter.words.rb <- as.character(values_table[(grep("l15.filter.words.rbValue",
                                                                 values_table[, "variable"])), "value"])
          if (!(length(l15.filter.words.rb) == 0 ||
                is.na(l15.filter.words.rb))) {
            if (l15.filter.words.rb == "yes") {
              l15.onyes.click()
            } else if (l15.filter.words.rb == "no") {
              l15.onno.click()
            }
            tcltk::tclvalue(l15.filter.words.rbValue) <- l15.filter.words.rb
          }
          
          
          l19.rb <- as.character(values_table[(grep("l19.rbValue",
                                                    values_table[, "variable"])), "value"])
          if (!(length(l19.rb) == 0 || is.na(l19.rb))) {
            if (l19.rb == "all") {
              l19.onall.click()
            } else if (l19.rb == "searchwords") {
              l19.onsearchwords.click()
            }
            tcltk::tclvalue(l19.rbValue) <- l19.rb
          }
          
          search.wds <- as.character(values_table[(grep("search.words",
                                                        values_table[, "variable"])), "value"])
          if (!(length(search.wds) == 0 || is.na(search.wds)))
            tcltk::tclvalue(search.words.var) <- search.wds
          ## ignore.case.sw
          regex.sw <- as.logical(values_table[(grep("regex.sw",
                                                    values_table[, "variable"])), "value"])
          if (!(length(regex.sw) == 0 || is.na(regex.sw))){
            if (regex.sw == TRUE){
              tcltk::tclvalue(regex.sw.var) <- "1"
            } else {
              tcltk::tclvalue(regex.sw.var) <- "0"
            }
          }
          ## ignore.case.sw
          ic.sw <- as.logical(values_table[(grep("ignore.case.sw",
                                                 values_table[, "variable"])), "value"])
          if (!(length(ic.sw) == 0 || is.na(ic.sw)))
            tcltk::tclvalue(ignore.case.sw.var) <- as.character(ic.sw)
          ## eval.abbrevs
          eval.abbrevs <- as.logical(values_table[(grep("eval.abbrevs",
                                                        values_table[, "variable"])), "value"])
          if (!(length(eval.abbrevs) == 0 || is.na(eval.abbrevs)))
            tcltk::tclvalue(eval.abbrevs.var) <- as.character(eval.abbrevs)
          
          
          ## write.table.locations (generates 3 tables with
          ## the locations of the tables within the txt/html
          ## files)
          wtl <- as.logical(values_table[(grep("write.table.locations",
                                               values_table[, "variable"])), "value"])
          if (!(length(wtl) == 0 || is.na(wtl))) tcltk::tclvalue(write.table.locations.var) <- as.character(wtl)
          
          outputfolder <- as.character(values_table[(grep("outputfolder",
                                                          values_table[, "variable"])), "value"])
          
          if (!(length(outputfolder) == 0 || is.na(outputfolder))) {
            if (grepl("^/examples/MTX_output", outputfolder) || grepl("^examples/MTX_output", outputfolder)){
              tcltk::tclvalue(outputfolder.var) <- paste0(system.file(package = "PDE"),"/",outputfolder)
            } else {
              tcltk::tclvalue(outputfolder.var) <- outputfolder
            }
          } else {
            tcltk::tclvalue(outputfolder.var) <- ""
          }
          
          
          pdfs <- as.character(values_table[(grep("pdfs",
                                                  values_table[, "variable"])), "value"])
          if (length(pdfs) == 0 || is.na(pdfs)) {
            pdfs <- as.character(values_table[(grep("pdffolder",
                                                    values_table[, "variable"])), "value"])
          }
          
          if (length(pdfs) == 0 || is.na(pdfs)) {
            pdfs <- ""
          }
          
          ## if it is a directory
          ## if it is a single file
          ## if example working directory
          if (dir.exists(pdfs) || 
              file.exists(pdfs) ||
              all(file.exists(unlist(strsplit(pdfs,
                                              ";"))))) {
            tcltk::tclvalue(pdfs.var) <- pdfs
            ## if it is a directory
            ## if it is a single file
          } else if (dir.exists(paste0(system.file(package = "PDE"),"/",pdfs)) ||
                     file.exists(paste0(system.file(package = "PDE"),"/",pdfs))) {
            tcltk::tclvalue(pdfs.var) <- paste0(system.file(package = "PDE"),"/",pdfs)
          } else if (all(file.exists(paste0(system.file(package = "PDE"),"/",unlist(strsplit(pdfs,
                                                                                             ";")))))) {
            tcltk::tclvalue(pdfs.var) <- paste0(paste0(system.file(package = "PDE"),"/",unlist(strsplit(pdfs,
                                                                                                        ";"))), collapse = ";")
          }
          
          ## preset the additional parameters filter words
          filter.wds <- as.character(values_table[(grep("^filter.words$",
                                                        values_table[, "variable"])), "value"])
          if (!(length(filter.wds) == 0 || is.na(filter.wds))) tcltk::tclvalue(filter.words.var) <- filter.wds
          if (length(grep(";", filter.wds)) > 0) tcltk::tclvalue(filter.words.var) <- filter.wds
          
          ## ignore.case.fw
          ic.fw <- as.logical(values_table[(grep("ignore.case.fw",
                                                 values_table[, "variable"])), "value"])
          if (!(length(ic.fw) == 0 || is.na(ic.fw))) tcltk::tclvalue(ignore.case.fw.var) <- as.character(ic.fw)
          
          ## regex.fw
          regex.fw <- as.logical(values_table[(grep("regex.fw",
                                                    values_table[, "variable"])), "value"])
          if (!(length(regex.fw) == 0 || is.na(regex.fw))){
            if (regex.fw == TRUE){
              tcltk::tclvalue(regex.fw.var) <- "1"
            } else {
              tcltk::tclvalue(regex.fw.var) <- "0"
            }
          }
          
          ## filter.word.times
          filter.word.times <- values_table[(grep("filter.word.times",
                                                         values_table[, "variable"])), "value"]
          if (!(length(filter.word.times) == 0 ||
                is.na(filter.word.times))) tcltk::tclvalue(filter.word.times.var) <- filter.word.times
          
          if (grepl("%",filter.word.times)){
            tcltk::tclvalue(percentage.fw.var) <- "1"
          } else {
            tcltk::tclvalue(percentage.fw.var) <- "0"
          }
          
          ## table headings
          table.heading.wds <- as.character(values_table[(grep("table.heading.words",
                                                               values_table[, "variable"])), "value"])
          if (!(length(table.heading.wds) == 0 ||
                is.na(filter.wds))) tcltk::tclvalue(table.heading.words.var) <- table.heading.wds
          if (length(grep(";", table.heading.wds)) > 0) tcltk::tclvalue(table.heading.words.var) <- table.heading.wds
          
          ## ignore.case.th
          ic.th <- as.logical(values_table[(grep("ignore.case.th",
                                                 values_table[, "variable"])), "value"])
          if (!(length(ic.th) == 0 || is.na(ic.th))) tcltk::tclvalue(ignore.case.th.var) <- as.character(ic.th)
          
          out.table.format <- as.character(values_table[(grep("out.table.format",
                                                              values_table[, "variable"])), "value"])
          valid.out.table.formats <- c(".csv (WINDOWS-1252)", ".csv (macintosh)", ".csv (UTF-8)", 
                                       ".tsv (WINDOWS-1252)",".tsv (macintosh)",".tsv (UTF-8)")
          if (!(length(out.table.format) == 0)){
            if (!is.na(out.table.format) &&
                out.table.format %in% valid.out.table.formats){
              tcltk::tclvalue(out.table.format.var) <- out.table.format
            }
          }
          
          dev_x <- strtoi(values_table[(grep("dev_x",
                                             values_table[, "variable"])), "value"])
          if (!(length(dev_x) == 0 || is.na(dev_x))) tcltk::tclvalue(dev_x.var) <- dev_x
          
          dev_y <- strtoi(values_table[(grep("dev_y",
                                             values_table[, "variable"])), "value"])
          if (!(length(dev_y) == 0 || is.na(dev_y))) tcltk::tclvalue(dev_y.var) <- dev_y
          
          context <- strtoi(values_table[(grep("context",
                                               values_table[, "variable"])), "value"])
          if (!(length(context) == 0 || is.na(context))) tcltk::tclvalue(context.var) <- context
          
          write.tab.doc.file <- as.logical(values_table[(grep("write.tab.doc.file",
                                                              values_table[, "variable"])), "value"])
          if (!(length(write.tab.doc.file) == 0 ||
                is.na(write.tab.doc.file))) tcltk::tclvalue(write.tab.doc.file.var) <- as.character(write.tab.doc.file)
          
          exp.nondetc.tabs <- as.logical(values_table[(grep("exp.nondetc.tabs",
                                                            values_table[, "variable"])), "value"])
          if (!(length(exp.nondetc.tabs) == 0 ||
                is.na(exp.nondetc.tabs))) tcltk::tclvalue(exp.nondetc.tabs.var) <- as.character(exp.nondetc.tabs)
          
          write.txt.doc.file <- as.logical(values_table[(grep("write.txt.doc.file",
                                                              values_table[, "variable"])), "value"])
          if (!(length(write.txt.doc.file) == 0 ||
                is.na(write.txt.doc.file))) tcltk::tclvalue(write.txt.doc.file.var) <- as.character(write.txt.doc.file)
          
          exp.nondetc.tabs <- as.logical(values_table[(grep("exp.nondetc.tabs",
                                                            values_table[, "variable"])), "value"])
          if (!(length(exp.nondetc.tabs) == 0 ||
                is.na(exp.nondetc.tabs))) tcltk::tclvalue(exp.nondetc.tabs.var) <- as.character(exp.nondetc.tabs)
          
          cpy_mv <- as.character(values_table[(grep("cpy_mv",
                                                  values_table[, "variable"])), "value"])
          if (!(length(cpy_mv) == 0 || is.na(cpy_mv))) tcltk::tclvalue(copy_move_pdfs.var) <- as.character(cpy_mv)
          
          delete <- as.logical(values_table[(grep("delete",
                                                  values_table[, "variable"])), "value"])
          if (!(length(delete) == 0 || is.na(delete))) tcltk::tclvalue(delete.var) <- as.character(delete)
        } ## if wrong TSV was selected
      }
      
      tcltk::tkfocus(start.pause.but)
      tcltk::tkraise(PDE.globals$ttanalyzer)
    }
    l1.load.tsv.but <- tcltk2::tk2button(l1, text = "Load form from TSV",
                                         command = load.tsv)
    
    ## save current variables into tsv to fill in all
    ## the boxes
    save.tsv <- function() {
      todays.date <- format(Sys.Date(), "%Y-%m-%d")
      if (tcltk::tclvalue(tsv_location.var) == ""){
        PDE_parameters_filename <- paste0(todays.date, 
                                          "_", "PDE_parameters_v1.4.tsv")
      } else {
        PDE_parameters_filename <-  basename(tcltk::tclvalue(tsv_location.var))
      }
      tcltk::tclvalue(tsv_location.var) <- tcltk::tkgetSaveFile(initialdir = dirname(tcltk::tclvalue(tsv_location.var)),
                                                                initialfile = PDE_parameters_filename,
                                                                defaultextension = ".tsv", 
                                                                filetypes = "{ {TSV Files} {.tsv} } { {All Files} * }")
      
      values_table <- data.frame(matrix(ncol = 2,
                                        nrow = 25))
      x <- c("variable", "value")
      colnames(values_table) <- x
      values_table$variable <- c("whattoextr", "pdfs",
                                 "outputfolder", "table.heading.words",
                                 "ignore.case.th", "filter.words", "regex.fw", "ignore.case.fw",
                                 "filter.word.times", "search.words", "regex.sw", "ignore.case.sw",
                                 "eval.abbrevs",
                                 "write.table.locations", "cpy_mv", "delete", "exp.nondetc.tabs",
                                 "out.table.format", "dev_x", "dev_y", "context", "write.tab.doc.file",
                                 "write.txt.doc.file", "l15.filter.words.rbValue",
                                 "l19.rbValue")
      
      values_table[grep("whattoextr", values_table[,
                                                   "variable"]), "value"] <- tcltk::tclvalue(whattoextr.var)
      values_table[grep("pdfs", values_table[, "variable"]),
                   "value"] <- tcltk::tclvalue(pdfs.var)
      values_table[grep("outputfolder", values_table[,
                                                     "variable"]), "value"] <- tcltk::tclvalue(outputfolder.var)
      values_table[grep("table.heading.words", values_table[,
                                                            "variable"]), "value"] <- tcltk::tclvalue(table.heading.words.var)
      values_table[grep("ignore.case.th", values_table[,
                                                       "variable"]), "value"] <- tcltk::tclvalue(ignore.case.th.var)
      values_table[grep("filter.words", values_table[,
                                                     "variable"]), "value"] <- tcltk::tclvalue(filter.words.var)
      values_table[grep("ignore.case.fw", values_table[,
                                                       "variable"]), "value"] <- tcltk::tclvalue(ignore.case.fw.var)
      if (tcltk::tclvalue(regex.fw.var) == "1"){
        regex.fw <- TRUE
      } else {
        regex.fw <- FALSE
      }
      values_table[grep("regex.fw", values_table[,
                                                 "variable"]), "value"] <- regex.fw
      values_table[grep("filter.word.times", values_table[,
                                                          "variable"]), "value"] <- tcltk::tclvalue(filter.word.times.var)
      values_table[grep("search.words", values_table[,
                                                     "variable"]), "value"] <- tcltk::tclvalue(search.words.var)
      if (tcltk::tclvalue(regex.sw.var) == "1"){
        regex.sw <- TRUE
      } else {
        regex.sw <- FALSE
      }
      values_table[grep("regex.sw", values_table[,
                                                 "variable"]), "value"] <- regex.sw
      values_table[grep("ignore.case.sw", values_table[,
                                                       "variable"]), "value"] <- tcltk::tclvalue(ignore.case.sw.var)
      values_table[grep("eval.abbrevs", values_table[,
                                                     "variable"]), "value"] <- tcltk::tclvalue(eval.abbrevs.var)
      values_table[grep("write.table.locations", values_table[,
                                                              "variable"]), "value"] <- tcltk::tclvalue(write.table.locations.var)
      values_table[grep("cpy_mv", values_table[,
                                               "variable"]), "value"] = tcltk::tclvalue(copy_move_pdfs.var)
      values_table[grep("delete", values_table[,
                                               "variable"]), "value"] = tcltk::tclvalue(delete.var)
      values_table[grep("exp.nondetc.tabs", values_table[,
                                                         "variable"]), "value"] <- tcltk::tclvalue(exp.nondetc.tabs.var)
      values_table[grep("out.table.format", values_table[,
                                                         "variable"]), "value"] <- tcltk::tclvalue(out.table.format.var)
      values_table[grep("dev_x", values_table[, "variable"]),
                   "value"] <- tcltk::tclvalue(dev_x.var)
      values_table[grep("dev_y", values_table[, "variable"]),
                   "value"] <- tcltk::tclvalue(dev_y.var)
      values_table[grep("context", values_table[,
                                                "variable"]), "value"] <- tcltk::tclvalue(context.var)
      values_table[grep("write.tab.doc.file", values_table[,
                                                           "variable"]), "value"] <- tcltk::tclvalue(write.tab.doc.file.var)
      values_table[grep("write.txt.doc.file", values_table[,
                                                           "variable"]), "value"] <- tcltk::tclvalue(write.txt.doc.file.var)
      
      ## additional variable for interactive user form
      values_table[grep("l15.filter.words.rbValue",
                        values_table[, "variable"]), "value"] <- tcltk::tclvalue(l15.filter.words.rbValue)
      values_table[grep("l19.rbValue", values_table[,
                                                    "variable"]), "value"] <- tcltk::tclvalue(l19.rbValue)
      
      if (tcltk::tclvalue(tsv_location.var) != "") utils::write.table(values_table, file = tcltk::tclvalue(tsv_location.var),
                                                                      row.names = FALSE, sep = "\t", quote = FALSE)
      
      tcltk::tkfocus(PDE.globals$ttanalyzer)
      tcltk::tkraise(PDE.globals$ttanalyzer)
    }
    
    l1.save.tsv.but <- tcltk2::tk2button(l1, text = "Save form as TSV",
                                         command = save.tsv)
    
    reset <- function() {
      ## in_output
      tcltk::tclvalue(pdfs.var) <- ""
      tcltk::tclvalue(outputfolder.var) <- ""
      tcltk::tclvalue(out.table.format.var) <- ".csv (WINDOWS-1252)"
      ## search words
      tcltk::tclvalue(whattoextr.var) <- "tab"
      l8.ontab.click()
      tcltk::tclvalue(l19.rbValue) <- "all"
      l19.onall.click()
      tcltk::tclvalue(search.words.var) <- ""
      tcltk::tclvalue(regex.sw.var) <- "0"
      tcltk::tclvalue(ignore.case.sw.var) <- "FALSE"
      tcltk::tclvalue(eval.abbrevs.var) <- "TRUE"
      tcltk::tclvalue(context.var) <- "0"
      ## filter words
      tcltk::tclvalue(l15.filter.words.rbValue) <- "no"
      l15.onno.click()
      tcltk::tclvalue(filter.words.var) <- ""
      tcltk::tclvalue(regex.fw.var) <- "0"
      tcltk::tclvalue(filter.word.times.var) <- "0.2%"
      tcltk::tclvalue(percentage.fw.var) = "1"
      tcltk::tclvalue(ignore.case.fw.var) <- "FALSE"
      tcltk::tclvalue(copy_move_pdfs.var) <- "nocpymv"
      ## paras
      tcltk::tclvalue(table.heading.words.var) <- ""
      tcltk::tclvalue(ignore.case.th.var) <- "FALSE"
      tcltk::tclvalue(dev_x.var) <- "20"
      tcltk::tclvalue(dev_y.var) <- "9999"
      ## docus
      tcltk::tclvalue(write.table.locations.var) <- "FALSE"
      tcltk::tclvalue(exp.nondetc.tabs.var) <- "TRUE"
      tcltk::tclvalue(write.tab.doc.file.var) <- "TRUE"
      tcltk::tclvalue(write.txt.doc.file.var) <- "TRUE"
      tcltk::tclvalue(delete.var) <- "TRUE"
      tcltk::tkconfigure(le.progress.bar.pb, value = 0)
      tcltk::tclvalue(proc.pdf) <- ""
      ## add progress info
      tcltk::tkconfigure(PDE.globals$le.progress.textbox,values = c("",""))
      tcltk::tkconfigure(PDE.globals$le.progress.textbox,textvariable = tcltk::tclVar(""))
      ## reset table
      tcltk::tclvalue(table.location.var) <- ""
      tcltk::tkconfigure(l37.jumptotable.cb, values = "")
      out_table_list <- ""
      tcltk::tclvalue(wrap.var) <- "1"
      tcltk::tkconfigure(l37.wrap.rb, state = "disabled")
      tcltk::tkconfigure(l37.no.wrap.rb, state = "disabled")
      tcltk::tkdelete(l38.table, 0, "end")
      tcltk::tkconfigure(l38.table, columns = "")
    }
    l1.reset.but <- tcltk2::tk2button(l1, text = "Reset form",
                                      command = reset)
    
    ## in_output
    
    ## line 7 ######### open the PDF file location
    select.pdffolder <- function() {
      
      default.pdffolder <- unlist(strsplit(tcltk::tclvalue(pdfs.var),";"))[1]
      if (is.na(default.pdffolder)) default.pdffolder <- ""
      if (!dir.exists(default.pdffolder)) default.pdffolder <- dirname(default.pdffolder)
      pdfs <- tcltk::tk_choose.dir(default = default.pdffolder, caption = "Choose folder with PDF files")
      if (!is.na(pdfs)){
        tcltk::tclvalue(pdfs.var) <- pdfs
      }
      tcltk::tkfocus(PDE.globals$ttanalyzer)
      tcltk::tkraise(PDE.globals$ttanalyzer)
      
    }
    l7.2.select.pdffolder.but <- tcltk2::tk2button(l7.2, text = "Select folder",
                                                   command = select.pdffolder)
    
    load.pdffiles <- function() {
      default.pdffolder <- unlist(strsplit(tcltk::tclvalue(pdfs.var),";"))[1]
      if (is.na(default.pdffolder)) default.pdffolder <- ""
      if (!dir.exists(default.pdffolder)) default.pdffolder <- dirname(default.pdffolder)
      pdfs <- tcltk::tk_choose.files(default = default.pdffolder,
                                     caption = "Choose all PDF files",
                                     multi = TRUE)
      if (length(pdfs) > 0){
        ## sometimes tk_choose.files produces additional paths that don't exists which are removed below
        for (pdffile in pdfs){
          if (!file.exists(pdffile)) pdfs <- pdfs[!(pdfs %in% pdffile)]
        }
        
        tcltk::tclvalue(pdfs.var) <- paste(pdfs, collapse = ";")
      }
      tcltk::tkfocus(PDE.globals$ttanalyzer)
      tcltk::tkraise(PDE.globals$ttanalyzer)
    }
    l7.2.load.pdffiles.but <- tcltk2::tk2button(l7.2, text = "Load files",
                                                command = load.pdffiles)
    
    ## line9
    open.outputfolder <- function() {
      if (dir.exists(tcltk::tclvalue(outputfolder.var))){
        system(paste0("open ", "\"", tcltk::tclvalue(outputfolder.var),
                      "\""))
      } else {
        tcltk::tkmessageBox(title = "Error", type = "ok",
                            icon = "error", message = paste0("Please select an existing output folder."))
      }
    }
    l9.2.open.outputfolder.but <- tcltk2::tk2button(l9.2, text = "Open output folder",
                                                    command = open.outputfolder)
    
    ## line 9 open the functions file location
    select.outputfolder <- function() {
      outputfolder <- tcltk::tk_choose.dir(default = tcltk::tclvalue(outputfolder.var),
                                           caption = "Choose folder with PDF files")
      if (is.na(outputfolder)) outputfolder <- ""
      tcltk::tclvalue(outputfolder.var) <- outputfolder
      tcltk::tkfocus(PDE.globals$ttanalyzer)
      tcltk::tkraise(PDE.globals$ttanalyzer)
    }
    l9.2.select.outputfolder.but <- tcltk2::tk2button(l9.2, text = "Select folder",
                                                      command = select.outputfolder)
    
    start.pause <- function() {
      if (tcltk::tclvalue(start.pause.but.label.var) == "Start analysis"){
        tcltk::tclvalue(start.pause.but.label.var) <- "Pause analysis"
        tcltk::tclvalue(close.stop.but.label.var) <- "Stop analysis"
        tcltk::tkconfigure(start.pause.but, text = tcltk::tclvalue(start.pause.but.label.var))
        tcltk::tkconfigure(quit.but, text = tcltk::tclvalue(close.stop.but.label.var))
        ## this grid gets added at the end
        save.res <- as.character(tcltk::tkmessageBox(title = "Warning",
                                                     type = "yesnocancel", icon = "question",
                                                     message = "Do you want to save the form before starting the analysis?"))
        
        ## Write the entries of the form
        if (save.res == "yes") {
          save.tsv()
        }
        
        if (!save.res == "cancel") {
          start.analysis <- NULL
          ## check input boxes for correct format -------
          
          ######### pdfs #############
          if (any(dir.exists(tcltk::tclvalue(pdfs.var)),
                  file.exists(tcltk::tclvalue(pdfs.var)),
                  all(file.exists(unlist(strsplit(tcltk::tclvalue(pdfs.var), ";")))))) {
          } else {
            start.analysis <- c(start.analysis,
                                "input PDF file folder")
          }
          
          ######### filterwords? #############
          if (tcltk::tclvalue(l15.filter.words.rbValue) == "no") {
            filter.for <- ""
          } else {
            if (length(grep(";", tcltk::tclvalue(filter.words.var))) > 0) {
              filter.for <- strsplit(tcltk::tclvalue(filter.words.var), ";")[[1]]
            } else {
              filter.for <- tcltk::tclvalue(filter.words.var)
            }
          }
          
          ######### search words? #############
          if (tcltk::tclvalue(l19.rbValue) == "l19.rbValue") {
            search.for <- ""
            search.word.categories <- NULL
          } else {
            ## if there is %:category:%
            if (length(grep(";", tcltk::tclvalue(search.words.var))) > 0) {
              search.for <- strsplit(tcltk::tclvalue(search.words.var), ";")[[1]]
            } else {
              search.for <- tcltk::tclvalue(search.words.var)
            }
            search.word.categories <- NULL
            ## if categories are in the search word list
            if (grepl("%:(.)+:%",tcltk::tclvalue(search.words.var))){
              category <- NULL
              for (sf in 1:length(search.for)){
                if (grepl("%:(.)+:%",search.for[sf])){
                  category <- gsub("%:(.+):%.*", "\\1", search.for[sf])
                  search.for[sf] <- sub(paste0("%:",category,":%"),"",search.for[sf])
                  search.word.categories <- c(search.word.categories,category)
                } else {
                  if (is.null(category)){
                    tcltk::tkmessageBox(title = "Warning",
                                        type = "ok", icon = "warning",
                                        message = paste0("Choose the correct formating",
                                                         " for categories ",
                                                         "(e.g. %:category:%first search word)"))
                    stop(paste0("Choose the correct formating",
                                " for categories ",
                                "(e.g. %:category:%first search word)"))
                  }
                  search.word.categories <- c(search.word.categories,category)
                }
              }
            }
          }
          
          ## set all variable for executing analysis ---------
          whattoextr <- tcltk::tclvalue(whattoextr.var)
          pdfs <- tcltk::tclvalue(pdfs.var)
          outputfolder <- tcltk::tclvalue(outputfolder.var)
          out.table.format <- tcltk::tclvalue(out.table.format.var)
          if (length(grep(";", tcltk::tclvalue(table.heading.words.var))) > 0) {
            table.heading.for <- strsplit(tcltk::tclvalue(table.heading.words.var), ";")[[1]]
          } else {
            table.heading.for <- tcltk::tclvalue(table.heading.words.var)
          }
          if (tcltk::tclvalue(regex.sw.var) == "1"){
            regex.sw <- TRUE
          } else {
            regex.sw <- FALSE
          }
          if (tcltk::tclvalue(regex.fw.var) == "1"){
            regex.fw <- TRUE
          } else {
            regex.fw <- FALSE
          }
          ic.th <- tcltk::tclvalue(ignore.case.th.var)
          ic.th <- tcltk::tclvalue(ignore.case.th.var)
          ic.fw <- tcltk::tclvalue(ignore.case.fw.var)
          ic.sw <- tcltk::tclvalue(ignore.case.sw.var)
          eval.abbrevs <- tcltk::tclvalue(eval.abbrevs.var)
          filter.word.times <- tcltk::tclvalue(filter.word.times.var)
          context <- as.numeric(tcltk::tclvalue(context.var))
          dev_x <- as.numeric(tcltk::tclvalue(dev_x.var))
          dev_y <- as.numeric(tcltk::tclvalue(dev_y.var))
          wtl <- tcltk::tclvalue(write.table.locations.var)
          exp.nondetc.tabs <- tcltk::tclvalue(exp.nondetc.tabs.var)
          write.tab.doc.file <- tcltk::tclvalue(write.tab.doc.file.var)
          write.txt.doc.file <- tcltk::tclvalue(write.txt.doc.file.var)
          cpy_mv <- tcltk::tclvalue(copy_move_pdfs.var)
          delete <- tcltk::tclvalue(delete.var)
          
          if (length(start.analysis) == 0) {
            
            # source(tcltk::tclvalue(functionsfile.var))
            
            
            ## if it is a directory
            if (dir.exists(pdfs)) {
              pdf.files <- list.files(pdfs, pattern = "*.pdf",
                                      full.names = TRUE, recursive = TRUE)
              ## if it is a single file
            } else if (file.exists(pdfs)) {
              pdf.files <- pdfs
            } else if (all(file.exists(unlist(strsplit(pdfs,
                                                       ";"))))) {
              pdf.files <- unlist(strsplit(pdfs,
                                           ";"))
            }
            
            output <- NULL
            todays.date_time <- paste0(format(Sys.Date(), "%Y-%m-%d"),format(Sys.time(), "-%Hh-%Mm"))
            
            for (pdf in pdf.files) {
              
              # wait if pause is pressed
              # counter <- 0
              while (tcltk::tclvalue(start.pause.but.label.var) == "Resume analysis"){
                Sys.sleep(1)
                # counter <-  counter + 1
                # if (counter == 144000) break
                ## if the execution was stopped
                if (tcltk::tclvalue(start.pause.but.label.var) == "Start analysis"){
                  break
                }
              }
              
              ## if the execution was stopped
              if (tcltk::tclvalue(start.pause.but.label.var) == "Start analysis"){
                break
              }
              
              ## update progress bar
              tcltk::tclvalue(progress) <- as.character(round(((grep(pdf,
                                                                     pdf.files, fixed = TRUE) - 1)/length(pdf.files) *
                                                                 100), digits = 0))
              tcltk::tkconfigure(le.progress.bar.pb,
                                 value = as.numeric(tcltk::tclvalue(progress)))
              ## update progress bar label
              tcltk::tclvalue(proc.pdf) <- paste0(tcltk::tclvalue(progress),
                                                  "%, ", basename(pdf))
              ## add progress info 1
              progress_info_length <- length(tcltk2::tk2list.get(PDE.globals$le.progress.textbox))
              if (progress_info_length > 3) {
                new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)[!grepl("^$",
                                                                                        tcltk2::tk2list.get(PDE.globals$le.progress.textbox))]
              } else {
                new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)
              }
              tcltk::tkconfigure(PDE.globals$le.progress.textbox,values = c(new_list,tcltk::tclvalue(proc.pdf)))
              tcltk::tkconfigure(PDE.globals$le.progress.textbox,textvariable = proc.pdf)
              tcltk::tcl("update")
              tcltk::tkfocus(le)
              
              tablelines <- .PDE_extr_data_from_pdf(pdf = pdf,
                                                    whattoextr = whattoextr,
                                                    out = outputfolder, context = context,
                                                    dev_x = dev_x, dev_y = dev_y, filter.words = filter.for,
                                                    regex.fw = regex.fw, ignore.case.fw = ic.fw,
                                                    filter.word.times = filter.word.times,
                                                    table.heading.words = table.heading.for,
                                                    ignore.case.th = ic.th, search.words = search.for,
                                                    search.word.categories = search.word.categories,
                                                    regex.sw = regex.sw,
                                                    ignore.case.sw = ic.sw, eval.abbrevs = eval.abbrevs,
                                                    write.table.locations = wtl,
                                                    write.tab.doc.file = write.tab.doc.file,
                                                    write.txt.doc.file = write.txt.doc.file,
                                                    exp.nondetc.tabs = exp.nondetc.tabs,
                                                    out.table.format = out.table.format,
                                                    delete = delete, cpy_mv = cpy_mv, verbose = verbose)
              
              tcltk::tcl("update")
              tcltk::tkfocus(le)
              
              ## if the algorithm was not run in table detection
              ## setting tablelines is NULL
              if (length(tablelines) > 0) {
                
                # ## add new tablelines to output
                # output[[length(output) + 1]] <- tablelines
                
                ##make stat_output mastertable
                if (is.null(output)){
                  output <- tablelines$stat_output
                } else {
                  output[setdiff(names(tablelines$stat_output), names(output))] <- NA
                  tablelines$stat_output[setdiff(names(output), names(tablelines$stat_output))] <- NA
                  output <- rbind(output,tablelines$stat_output)
                }
              }
              ## write the PDE_analyzer_word_stats table
              
              if (grepl("csv", out.table.format)) {
                out.table.separator <- ","
                out.table.ext <- ".csv"
              }
              if (grepl("tsv", out.table.format)) {
                out.table.separator <- "\t"
                out.table.ext <- ".tsv"
              }
              output_table <- cbind(pdf_file_name = rownames(output), output)
              utils::write.table(output_table, file = paste0(outputfolder,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                             out.table.ext),
                                 sep = out.table.separator,
                                 row.names = FALSE)
            }
            
            ## write the PDE_analyzer_word_stats table
            if ("pdf_searchword_total" %in% colnames(output)){
              if ("pdf_filterword_total" %in% colnames(output)){
                output <- output[order(output$pdf_searchword_total,
                                     output$pdf_filterword_total, decreasing = TRUE),,drop=FALSE]
              } else {
                output <- output[order(output$pdf_searchword_total, decreasing = TRUE),,drop=FALSE]
              }
            }

            if (grepl("csv", out.table.format)) {
              out.table.separator <- ","
              out.table.ext <- ".csv"
            }
            if (grepl("tsv", out.table.format)) {
              out.table.separator <- "\t"
              out.table.ext <- ".tsv"
            }
            output_table <- cbind(pdf_file_name = rownames(output), output)
            utils::write.table(output_table, file = paste0(outputfolder,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                     out.table.ext),
                               sep = out.table.separator,
                               row.names = FALSE)
            
            ## test for extracted tables
            out_table_list_full <- dir(path = paste0(outputfolder, "/tables/"), pattern = "\\.csv$|\\.tsv$",
                                       all.files=FALSE, full.names = TRUE)
            out_table_list <- dir(path = paste0(outputfolder, "/tables/"), pattern = "\\.csv$|\\.tsv$",
                                  all.files=FALSE, full.names = FALSE)
            if (length(out_table_list) != 0){
              ## test if first time table extracted
              if (tcltk::tclvalue(table.location.var) == ""){
                tcltk::tkpack(l37.jumptotable.cb,
                              side="left",fill="x",expand = TRUE,  pady = 2)
                tcltk::tkpack(l37.wrap.label, l37.wrap.rb, l37.nowrap.label, l37.no.wrap.rb,
                              side="left", pady = 2)
                
                tcltk::tkpack(scroll.y, side = "left", fill = "y", pady = 2)
                tcltk::tkpack(l38.table, side = "left", 
                              padx = 5, expand = TRUE, fill = "x")
                tcltk::tkpack(scroll.x, side = "left", expand = TRUE, fill = "x", pady = 2)
                tcltk::tkpack(l37,l38, l39, side = "top",
                              fill = "x")
              }
              
              file_dates <- file.info(out_table_list_full)
              last_created_file <- rownames(file_dates)[which.max(file_dates$mtime)]
              ## load the table display
              tcltk::tclvalue(table.location.var) <- basename(last_created_file)
              load_extracted_table(last_created_file)
              ##update the list of tables
              tcltk::tkconfigure(l37.jumptotable.cb, values = out_table_list)
            }
            
            ## add completion  info
            progress_info_length <- length(tcltk2::tk2list.get(PDE.globals$le.progress.textbox))
            if (progress_info_length > 3) {
              new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)[!grepl("^$",
                                                                                      tcltk2::tk2list.get(PDE.globals$le.progress.textbox))]
            } else {
              new_list <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)
            }
            out_msg <- c(out_msg, "Analyses are complete.")
            if (verbose) cat(utils::tail(out_msg,1), sep="\n")
            
            tcltk::tkconfigure(PDE.globals$le.progress.textbox,values = c(new_list,"complete"))
            tcltk::tkconfigure(PDE.globals$le.progress.textbox,textvariable = tcltk::tclVar("complete"))
            tcltk::tkconfigure(le.progress.bar.pb, value = 100)
            tcltk::tcl("update")
            tcltk::tkfocus(PDE.globals$ttanalyzer)
            tcltk::tkraise(PDE.globals$ttanalyzer)
            
          } else if (length(start.analysis) == 1) {
            tcltk::tkmessageBox(title = "Error", type = "ok",
                                icon = "error", message = paste0("Analysis not started. Check the ",
                                                                 start.analysis[1], " and ", start.analysis[2],
                                                                 " for corrrect path and formating before analysis."))
          } else {
            tcltk::tkmessageBox(title = "Error", type = "ok",
                                icon = "error", message = paste0("Analysis not started. Check the ",
                                                                 paste(start.analysis[-length(start.analysis)],
                                                                       collapse = ", "), " and ",
                                                                 start.analysis[length(start.analysis)],
                                                                 " for corrrect path and formating before analysis."))
          }
        }  ## end if cancel was not pressed
        tcltk::tclvalue(start.pause.but.label.var) <- "Start analysis"
        tcltk::tclvalue(close.stop.but.label.var) <- "Close session"
        tcltk::tkconfigure(start.pause.but, text = tcltk::tclvalue(start.pause.but.label.var))
        tcltk::tkconfigure(quit.but, text = tcltk::tclvalue(close.stop.but.label.var))
      } else if (tcltk::tclvalue(start.pause.but.label.var) == "Pause analysis") { ## end if Start analysis was pressed
        tcltk::tclvalue(start.pause.but.label.var) <- "Resume analysis"
        tcltk::tclvalue(close.stop.but.label.var) <- "Stop analysis"
        tcltk::tkconfigure(start.pause.but, text = tcltk::tclvalue(start.pause.but.label.var))
        tcltk::tkconfigure(quit.but, text = tcltk::tclvalue(close.stop.but.label.var))
      } else if (tcltk::tclvalue(start.pause.but.label.var) == "Resume analysis") { ## end if Start analysis was pressed
        tcltk::tclvalue(start.pause.but.label.var) <- "Pause analysis"
        tcltk::tclvalue(close.stop.but.label.var) <- "Stop analysis"
        tcltk::tkconfigure(start.pause.but, text = tcltk::tclvalue(start.pause.but.label.var))
        tcltk::tkconfigure(quit.but, text = tcltk::tclvalue(close.stop.but.label.var))
      }
    }
    
    start.pause.but <- tcltk2::tk2button(e, text = tcltk::tclvalue(start.pause.but.label.var),
                                         command = start.pause)
    
    close.stop_session <- function() {
      if (tcltk::tclvalue(close.stop.but.label.var) == "Close session"){
        ## this grid gets added at the end
        quit.res <- as.character(tcltk::tkmessageBox(title = "Warning",
                                                     type = "yesnocancel", icon = "warning",
                                                     message = "Do you want to save the form before closing?"))
        if (quit.res == "yes") {
          save.tsv
          tcltk::tkdestroy(PDE.globals$ttanalyzer)
        } else if (quit.res == "no") {
          tcltk::tkdestroy(PDE.globals$ttanalyzer)
        }
      } else {
        tcltk::tclvalue(start.pause.but.label.var) <- "Start analysis"
        tcltk::tclvalue(close.stop.but.label.var) <- "Close session"
        tcltk::tkconfigure(start.pause.but, text = tcltk::tclvalue(start.pause.but.label.var))
        tcltk::tkconfigure(quit.but, text = tcltk::tclvalue(close.stop.but.label.var))
      }
    }
    quit.but <- tcltk2::tk2button(e, text = tcltk::tclvalue(close.stop.but.label.var),
                                  command = close.stop_session)
    
    
    save.progress_info <- function() {
      todays.date_time <- paste0(format(Sys.Date(), "%Y-%m-%d"),format(Sys.time(), "-%Hh-%Mm"))
      PDE_parameters_filename <- paste0(todays.date_time,
                                        "_", "progress_info")
      txt_location <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile = PDE_parameters_filename,
                                                           defaultextension = ".txt", 
                                                           filetypes = "{ {TXT Files} {.txt} } { {All Files} * }"))
      
      progress_info <- tcltk2::tk2list.get(PDE.globals$le.progress.textbox)
      progress_info <- gsub(" file path maybe too long.",
                            " might be too long of a file path to be read by some programs. Consider using a shorter output path.",
                            progress_info)
      output_progress_info <- paste(progress_info, collapse = "\n")
      if (txt_location != "") write(output_progress_info, file = txt_location)
      
      tcltk::tkfocus(PDE.globals$ttanalyzer)
      tcltk::tkraise(PDE.globals$ttanalyzer)
    }
    le.save.txt.but <- tcltk2::tk2button(le, text = "^ save ^", width = 8,
                                         command = save.progress_info)
    
    ## the output table
    scroll.y <- tcltk2::tk2scrollbar(l38, orient = "vertical",
                                     command = function(...) tcltk::tkyview(l38.table,
                                                                            ...))  ## command that performs the scrolling
    scroll.x <- tcltk2::tk2scrollbar(l39, orient = "horizontal",
                                     command = function(...) tcltk::tkxview(l38.table,
                                                                            ...))  ## command that performs the scrolling
    
    l38.table <- tcltk2::tk2tablelist(l38, selectmode = "browse", 
                                      exportselection = 0, stripebackground = "lightgrey",
                                      stretch = "anchor", selecttype = "cell", 
                                      yscrollcommand = function(...) tcltk::tkset(scroll.y,...),
                                      xscrollcommand = function(...) tcltk::tkset(scroll.x,...))
    
    ##tooltip labels ----------------
    l7.hint_choosepdf <- tcltk2::tk2label(l7, text = "?",
                                          font = os.font.ten.underlined,
                                          background = "#05F2C7",
                                          foreground="blue")
    l9.hint_chooseoutput <- tcltk2::tk2label(l9, text = "?",
                                             font = os.font.ten.underlined,
                                             background = "#05F2C7",
                                             foreground="blue")
    l10.hint_outputformat <- tcltk2::tk2label(l10, text = "?",
                                              font = os.font.ten.underlined,
                                              background = "#05F2C7",
                                              foreground="blue")
    l8.hint_whattoextr <- tcltk2::tk2label(l8, text = "?",
                                           font = os.font.ten.underlined,
                                           background = "#05F2C7",
                                           foreground="blue")
    l19.hint_extractallorsent <- tcltk2::tk2label(l19, text = "?",
                                                  font = os.font.ten.underlined,
                                                  background = "#05F2C7",
                                                  foreground="blue")
    l20.hint_choose_sw <- tcltk2::tk2label(l20, text = "?",
                                           font = os.font.ten.underlined,
                                           background = "#05F2C7",
                                           foreground="blue")
    l21.hint_ignorecase_sw <- tcltk2::tk2label(l21, text = "?",
                                               font = os.font.ten.underlined,
                                               background = "#05F2C7",
                                               foreground="blue")
    l22.hint_context <- tcltk2::tk2label(l22, text = "?",
                                         font = os.font.ten.underlined,
                                         background = "#05F2C7",
                                         foreground="blue")
    l23.hint_evalabrev <- tcltk2::tk2label(l23, text = "?",
                                           font = os.font.ten.underlined,
                                           background = "#05F2C7",
                                           foreground="blue")
    l15.hint_fw_yesno <- tcltk2::tk2label(l15, text = "?",
                                          font = os.font.ten.underlined,
                                          background = "#05F2C7",
                                          foreground="blue")
    l16.hint_choose_fw <- tcltk2::tk2label(l16, text = "?",
                                           font = os.font.ten.underlined,
                                           background = "#05F2C7",
                                           foreground="blue")
    l17.hint_igrnorecase_fw <- tcltk2::tk2label(l17, text = "?",
                                                font = os.font.ten.underlined,
                                                background = "#05F2C7",
                                                foreground="blue")
    l18.1.hint_fwtimes <- tcltk2::tk2label(l18.1, text = "?",
                                         font = os.font.ten.underlined,
                                         background = "#05F2C7",
                                         foreground="blue")
    l12.hint_tableheading <- tcltk2::tk2label(l12, text = "?",
                                              font = os.font.ten.underlined,
                                              background = "#05F2C7",
                                              foreground="blue")
    l13.hint_ignorecase_th <- tcltk2::tk2label(l13, text = "?",
                                               font = os.font.ten.underlined,
                                               background = "#05F2C7",
                                               foreground="blue")
    l14.1.hint_devx <- tcltk2::tk2label(l14.1, text = "?",
                                        font = os.font.ten.underlined,
                                        background = "#05F2C7",
                                        foreground="blue")
    l14.2.hint_devy <- tcltk2::tk2label(l14.2, text = "?",
                                        font = os.font.ten.underlined,
                                        background = "#05F2C7",
                                        foreground="blue")
    l25.hint_wtl <- tcltk2::tk2label(l25, text = "?",
                                     font = os.font.ten.underlined,
                                     background = "#05F2C7",
                                     foreground="blue")
    l26.hint_exportnondetc <- tcltk2::tk2label(l26, text = "?",
                                               font = os.font.ten.underlined,
                                               background = "#05F2C7",
                                               foreground="blue")
    l27.hint_writetabdoc <- tcltk2::tk2label(l27, text = "?",
                                             font = os.font.ten.underlined,
                                             background = "#05F2C7",
                                             foreground="blue")
    l28.hint_writetxtdoc <- tcltk2::tk2label(l28, text = "?",
                                             font = os.font.ten.underlined,
                                             background = "#05F2C7",
                                             foreground="blue")
    l29.hint_delete <- tcltk2::tk2label(l29, text = "?",
                                        font = os.font.ten.underlined,
                                        background = "#05F2C7",
                                        foreground="blue")
    
    ## load the table -------
    load_extracted_table <- function(table.location) {
      # A.1) test if tables folder exists
      if (table.location != ""){
        loadfile <- table.location
        
        if (Sys.info()["sysname"] == "Windows") {
          native.encoding <- "WINDOWS-1252"
        } else if (Sys.info()["sysname"] == "Darwin") {
          native.encoding <- "macintosh"
        } else {
          native.encoding <- "UTF-8"
        }
        
        all_content <- readLines(loadfile,
                                 encoding = native.encoding)
        if (grepl("The following table was detected but not processable",all_content[1])){
          ## clear table
          tcltk::tkdelete(l38.table, 0, "end")
          tcltk::tkconfigure(l38.table, columns = paste0("1 \"",
                                                         "\""))
          tcltk::tkinsert(l38.table, "end",
                          all_content[1])
        } else {
          ## enable wrap checkboxes
          tcltk::tkconfigure(l37.wrap.rb, state = "normal")
          tcltk::tkconfigure(l37.no.wrap.rb, state = "normal")
          
          if (Sys.info()["sysname"] == "Windows") {
            native.encoding <- "WINDOWS-1252"
          } else if (Sys.info()["sysname"] == "Darwin") {
            native.encoding <- "macintosh"
          } else {
            native.encoding <- "UTF-8"
          }
          loadfile <- table.location
          if (grepl(".csv$",loadfile)){
            separator <- ","
          } else {
            separator <- "\t"
          }
          all_content <- readLines(loadfile,
                                   encoding = native.encoding)
          out_table <- utils::read.table(file = loadfile, sep = separator, header = FALSE,
                                         encoding = native.encoding, stringsAsFactors=FALSE)
          ## test which lines have title
          for (i in 1:length(all_content)){
            ## delete everything before first comma
            rest_of_line <- sub(paste0(".*?\"",separator),",",all_content[i])
            if (!grepl(paste0("^",separator,"+$"), rest_of_line)){
              break
            }
          }
          
          ## for display convert from macintosh to UTF-8
          if (native.encoding == "macintosh"){
            out_table_new <- out_table
            for (r in 1:nrow(out_table)) {
              for (c in 1:ncol(out_table)) {
                out_table_new[r,c] <- iconv(out_table[r,c], from = "macintosh", to = "UTF-8")
              }
            }
            out_table <- out_table_new
          }
          
          ## go one back
          i = i - 1
          title <-  all_content[1:i]
          ##cleanup title
          title <- gsub("\",.*","",sub("\"","",title))
          title <- sub("^(,)+","",title)
          
          ## remove rows
          rest_content <- out_table[(i+2):nrow(out_table),]
          colnames(rest_content) <- out_table[i+1,]
          ##Replace NAs
          colnames(rest_content)[is.na(colnames(rest_content))] <- ""
          table_title <- paste(title, collapse = " ")
          table_content <- rest_content
          
          ## clear table
          tcltk::tkdelete(l38.table, 0, "end")
          
          ## fill the header
          columnhead <- NULL
          for (c in 2:(ncol(table_content))) {
            columnhead <- paste0(columnhead, "\" 0 \"",
                                 colnames(table_content)[c])
          }
          tcltk::tclvalue(columnnumber.var) <- as.character(ncol(table_content))
          tcltk::tkconfigure(l38.table, columns = paste0("1 \"",
                                                         colnames(table)[1], columnhead, "\""))
          
          ## make columns editable and wrap
          for (c in 0:(ncol(table_content) - 1)) {
            tcltk::tcl(l38.table, "columnconfigure",
                       c, editable = 1)
            tcltk::tcl(l38.table, "columnconfigure",
                       c, wrap = tcltk::tclvalue(wrap.var))
          }
          
          ## fill table
          for (r in 1:nrow(table_content)) {
            rowlist <- NULL
            for (c in 1:ncol(table_content)) {
              rowlist <- c(rowlist, as.character(table_content[r,
                                                               c]))
            }
            tcltk::tkinsert(l38.table, "end",
                            rowlist)
          }
          
        }
      }
    } ## end load_extracted_table
    
    out_table_list <- ""
    l37.jumptotable.cb <- tcltk2::tk2combobox(l37, textvariable = table.location.var,
                                              values = out_table_list, width = 30, state = "readonly")
    
    ## onchange combobox
    tcltk::tkbind(l37.jumptotable.cb, "<<ComboboxSelected>>",
                  function() {
                    load_extracted_table(paste0(tcltk::tclvalue(outputfolder.var),"/tables/",
                                                tcltk::tclvalue(table.location.var)))
                  })
    
    
    ## the scroll buttons -----------------------------------------------------
    
    # down <- function() {
    #   if (tcltk::tclvalue(scrollpos) == "0") {
    #     tcltk::tkpack.forget(l1)
    #     tcltk::tclvalue(scrollpos) <- "-1"
    #   }  else if (tcltk::tclvalue(scrollpos) == "-1") {
    #     tcltk::tkpack.forget(in_output)
    #     tcltk::tclvalue(scrollpos) <- "-2"
    #   } else if (tcltk::tclvalue(scrollpos) == "-2") {
    #     tcltk::tkpack.forget(paras)
    #     tcltk::tclvalue(scrollpos) <- "-3"
    #   } else if (tcltk::tclvalue(scrollpos) == "-3") {
    #     tcltk::tkpack.forget(docus)
    #     tcltk::tclvalue(scrollpos) <- "-4"
    #   }
    # }
    # 
    # up <- function() {
    #   if (tcltk::tclvalue(scrollpos) == "0") {
    #   } else if (tcltk::tclvalue(scrollpos) == "-1") {
    #     tcltk::tkpack(l1, side = "top", fill = "x", before = in_output)
    #     tcltk::tclvalue(scrollpos) <- "0"
    #   } else if (tcltk::tclvalue(scrollpos) == "-2") {
    #     tcltk::tkpack(in_output, side = "top", fill = "x",
    #                   before = paras)
    #     tcltk::tclvalue(scrollpos) <- "-1"
    #   } else if (tcltk::tclvalue(scrollpos) == "-3") {
    #     tcltk::tkpack(paras, side = "top", fill = "x",
    #                   before = docus)
    #     tcltk::tclvalue(scrollpos) <- "-2"
    #   } else if (tcltk::tclvalue(scrollpos) == "-4") {
    #     tcltk::tkpack(docus, side = "top", fill = "x",
    #                   before = e)
    #     tcltk::tclvalue(scrollpos) <- "-3"
    #   }
    # }
    # 
    # Up.but <- tcltk2::tk2button(r, text = "/\\", width = 2,
    #                             command = up)
    # Down.but <- tcltk2::tk2button(r, text = "\\/", width = 2,
    #                               command = down)
    
    ## How the form looks --------------------------------------
    
    ## the scroll buttons --------------------------------------
    
    # tcltk::tkpack(Up.but, expand = TRUE, side = "top", fill = "both")
    # tcltk::tkpack(Down.but, expand = TRUE, side = "top", fill = "both")
    # tcltk::tkpack(r, side = "right", anchor = "e", fill = "y")
    
    ## line 1 (buttons) ---------------------------------------
    tcltk::tkpack(l1.load.tsv.but, l1.save.tsv.but, l1.reset.but,
                  side = "left", expand = TRUE, pady = 2, padx = 5)
    tcltk::tkpack(l1, side = "top", anchor = "nw", fill = "x")
    
    
    ## in_output ---------------------------------------------
    ## line 7
    tcltk::tkpack(l7.step1.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l7.pdfs.label, l7.hint_choosepdf, side = "left", pady = 2,
                  padx = 0)
    tcltk::tkpack(l7.2.pdfs.entry, side = "left", expand = TRUE,
                  fill = "x", pady = 2, padx = 5)
    tcltk::tkpack(l7.2.select.pdffolder.but, l7.2.load.pdffiles.but,
                  side = "left", pady = 2)
    ## line 9
    tcltk::tkpack(l9.step2.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l9.outputfolder.label,l9.hint_chooseoutput, side = "left", pady = 2,
                  padx = 0)
    tcltk::tkpack(l9.2.outputfolder.entry, side = "left", expand = TRUE,
                  fill = "x", pady = 2, padx = 5)
    tcltk::tkpack(l9.2.select.outputfolder.but, l9.2.open.outputfolder.but, side = "left",
                  pady = 2, padx = 5)
    ## line 10
    tcltk::tkpack(l10.step3.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l10.out.table.format.label,l10.hint_outputformat, side = "left",
                  pady = 2, padx = 0)
    tcltk::tkpack(l10.2.out.table.format.combo, side = "left",
                  pady = 2, padx = 5)
    ## line 11
    tcltk::tkpack(l11.step4.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l11.step4_2.label, side = "left",
                  pady = 2, padx = 0)
    tcltk::tkpack(l11.2.step5.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l11.2.step5_2.label, side = "left",
                  pady = 2, padx = 0)
    
    tcltk::tkpack(l7,l7.2, l9,l9.2, l10,l10.2,l11,l11.2, side = "top", fill = "x")
    tcltk::tkpack(in_output_tab, side = "top", fill = "x")
    
    ## Search words ---------------------------------------------
    ## line 8
    tcltk::tkpack(l8.whattoextr.label, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l8.whattoextr.sentences.label, l8.whattoextr.txt.rb,
                  l8.whattoextr.tables.label, l8.whattoextr.tab.rb,
                  l8.whattoextr.both.label, l8.whattoextr.txtandtab.rb,l8.hint_whattoextr,
                  side = "left", pady = 2)
    ## line 19
    tcltk::tkpack(l19.extract.all.tables.label,  side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l19.extract.all.tables.all.label, l19.extract.all.tables.all.rb,
                  l19.extract.all.tables.searchwords.label, l19.extract.all.tables.searchwords.rb,
                  l19.hint_extractallorsent,
                  side = "left", pady = 2)
    ## line 20
    tcltk::tkpack(l20.search.words.label, l20.hint_choose_sw, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l20.2.search.words.entry, side = "left", expand = TRUE,
                  fill = "x", pady = 2, padx = 5)
    tcltk::tkpack(l20.2.regex.sw.cbtn, side = "left", pady = 2, padx = 5)
    ## line 21
    tcltk::tkpack(l21.ignore.case.sw.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l21.ignore.case.sw.yes.label, l21.ignore.case.sw.yes.rb,
                  l21.ignore.case.sw.no.label, l21.ignore.case.sw.no.rb,l21.hint_ignorecase_sw,
                  side = "left", pady = 2)
    ## line 22
    tcltk::tkpack(l22.context.label, l22.context.entry,
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l22.context.slider, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l22.hint_context,
                  side = "left", pady = 2, padx = 5)
    ## line 23
    tcltk::tkpack(l23.eval.abbrevs.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l23.eval.abbrevs.yes.label, l23.eval.abbrevs.yes.rb,
                  l23.eval.abbrevs.no.label, l23.eval.abbrevs.no.rb,
                  l23.hint_evalabrev, 
                  side = "left", pady = 2)
    tcltk::tkpack(l8, l19, l20,l20.2,l21, l22, l23, side = "top", fill = "x")
    tcltk::tkpack(searchwords_tab, side = "top", fill = "x")
    
    
    ## Filter words ---------------------------------------------
    ## line 15
    tcltk::tkpack(l15.filter.words.yesno.label,side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l15.filter.words.yes.label, l15.filter.words.yes.rb,
                  l15.filter.words.no.label, l15.filter.words.no.rb,l15.hint_fw_yesno, 
                  side = "left", pady = 2)
    ## line 16
    tcltk::tkpack(l16.filter.words.label, l16.hint_choose_fw, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l16.2.filter.words.entry, side = "left", expand = TRUE,
                  fill = "x", pady = 2, padx = 5)
    tcltk::tkpack(l16.2.regex.fw.cbtn, side = "left", pady = 2, padx = 5)
    ## line 17
    tcltk::tkpack(l17.ignore.case.fw.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l17.ignore.case.fw.yes.label, l17.ignore.case.fw.yes.rb,
                  l17.ignore.case.fw.no.label, l17.ignore.case.fw.no.rb, l17.hint_igrnorecase_fw, 
                  side = "left", pady = 2)
    ## line 18.1
    tcltk::tkpack(l18.1.filter.word.times.label, l18.1.filter.word.times.entry,
                  l18.1.percentage.fw.cbtn,
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l18.1.filter.word.times.slider, expand = TRUE,
                  fill = "x", side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l18.1.hint_fwtimes,
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l16.2.regex.fw.cbtn, side = "left", pady = 2, padx = 5)
    ## line 18.2
    tcltk::tkpack(l18.2.fw.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l18.2.fw.nocpymv.label, l18.2.fw.nocpymv.rb,
                  l18.2.fw.cpy.label,l18.2.fw.cpy.rb,
                  l18.2.fw.mv.label, l18.2.fw.mv.rb, 
                  side = "left", pady = 2)
    
    tcltk::tkpack(l15,l16,l16.2,l17,l18.1,l18.2, side = "top", fill = "x")
    tcltk::tkpack(filterwords_tab, side = "top", fill = "x")
    
    
    ## Parameters ---------------------------------------------
    
    ## line 12
    tcltk::tkpack(l12.table.heading.words.label, l12.hint_tableheading, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l12.2.table.heading.words.entry, side = "left",
                  expand = TRUE, fill = "x", pady = 2, padx = 5)
    ## line 13
    tcltk::tkpack(l13.ignore.case.th.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l13.ignore.case.th.yes.label, l13.ignore.case.th.yes.rb,
                  l13.ignore.case.th.no.label, l13.ignore.case.th.no.rb,l13.hint_ignorecase_th,
                  side = "left", pady = 2)
    ## line 14
    tcltk::tkpack(l14.1.dev_x.label, l14.1.dev_x.entry, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l14.1.dev_x.slider, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l14.1.hint_devx, side = "left",
                  pady = 2, padx = 5)
    ## line 14
    tcltk::tkpack(l14.2.dev_y.label, l14.2.dynamic.dev_y.cbtn, l14.2.dev_y.entry, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l14.2.dev_y.slider, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l14.2.hint_devy, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l12,l12.2,l13,l14.1, l14.2, side = "top", fill = "x")
    tcltk::tkpack(paras_tab, side = "top", fill = "x")
    
    ## Documentation -----------------------------------------
    ## line 25
    tcltk::tkpack(l25.write.table.locations.label,  side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l25.write.table.locations.yes.label, l25.write.table.locations.yes.rb,
                  l25.write.table.locations.no.label, l25.write.table.locations.no.rb,l25.hint_wtl,
                  side = "left", pady = 2)
    ## line 26
    tcltk::tkpack(l26.exp.nondetc.tabs.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l26.exp.nondetc.tabs.yes.label, l26.exp.nondetc.tabs.yes.rb,
                  l26.exp.nondetc.tabs.no.label, l26.exp.nondetc.tabs.no.rb,l26.hint_exportnondetc, 
                  side = "left", pady = 2)
    # line 27
    tcltk::tkpack(l27.write.tab.doc.file.label, side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l27.write.tab.doc.file.yes.label, l27.write.tab.doc.file.yes.rb,
                  l27.write.tab.doc.file.no.label, l27.write.tab.doc.file.no.rb,l27.hint_writetabdoc, 
                  side = "left", pady = 2)
    ## line 28
    tcltk::tkpack(l28.write.txt.doc.file.label,  side = "left",
                  pady = 2, padx = 5)
    tcltk::tkpack(l28.write.txt.doc.file.yes.label, l28.write.txt.doc.file.yes.rb,
                  l28.write.txt.doc.file.no.label, l28.write.txt.doc.file.no.rb,l28.hint_writetxtdoc,
                  side = "left", pady = 2)
    # line 29
    tcltk::tkpack(l29.delete.label, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l29.delete.yes.label, l29.delete.yes.rb,
                  l29.delete.no.label, l29.delete.no.rb, l29.hint_delete, side = "left",
                  pady = 2)
    
    tcltk::tkpack(l25, l26, l27, l28, l29, side = "top",
                  fill = "x")
    tcltk::tkpack(docus_tab, side = "top", fill = "x")
    
    ## No output tab ----------
    tcltk::tkpack(l30.no.tables.label, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l31.no.tables.one.label, side = "left", pady = 2,
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l32.no.tables.two.label, side = "left", pady = 2,
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l33.no.tables.three.label, side = "left", pady = 2,
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l34.no.tables.four.label, side = "left", pady = 2,
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l35.no.tables.five.label, side = "left",
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l36.url.label, side = "left",
                  padx = 5, expand = TRUE, fill = "x")
    tcltk::tkpack(l30,l31,l32,l33,l34,l35,l36, side = "top",
                  fill = "x")
    tcltk::tkpack(no.output_tab, side = "top", fill = "x")
    
    tkadd(notebook,in_output_tab,text="Input/Output")   ### tabid=0
    tkadd(notebook,searchwords_tab,text="Search Words")   ### tabid=1
    tkadd(notebook,filterwords_tab,text="Filter Words")   ### tabid=2
    tkadd(notebook,paras_tab,text="Parameters") ### tabid=3
    tkadd(notebook,docus_tab,text="Documentation") ### tabid=4
    tkadd(notebook,no.output_tab,text="No Output?") ### tabid=5
    
    
    tcltk::tkpack(notebook, side = "top", fill = "x")
    
    ## end buttons -----------------------------------------------
    
    ## Tooltips -------------------
    tcltk::tkbind(l7.hint_choosepdf, "<Enter>", 
                  expression(tooltip(paste0("Choose a folder with PDF files or ",
                                            "select one or more PDF files for analysis ",
                                            "(use Ctrl and/or Shift to select multiple). ",
                                            "All PDF files in the chosen folder and ",
                                            "subfolders will be analyzed. ",
                                            "Multiple PDF files will be separated by \";\" without a space."), 
                                     l7.hint_choosepdf)))
    tcltk::tkbind(l9.hint_chooseoutput, "<Enter>", 
                  expression(tooltip(paste0("All analysis files will be created inside of this folder; ",
                                            "therefore, choose an empty folder or create a new one as ",
                                            "output directory, since analyses create at least a number ",
                                            "of files equal to the amount of PDF files analyzed. If no ",
                                            "output folder is chosen, the results will be saved in the R",
                                            " working directory."), 
                                     l9.hint_chooseoutput)))
    tcltk::tkbind(l10.hint_outputformat, "<Enter>", 
                  expression(tooltip(paste0("The resulting analyses files can either be generated as ",
                                            "comma-separated values files (.csv) or tab-separated values ",
                                            "files (.tsv), with the former being easier to open and save ",
                                            "in Microsoft Excel, while the later leads to less errors ",
                                            "when opening in Microsoft Excel (as tabs are rare in texts).",
                                            " Depending on the operating system the output files are ",
                                            "opened in, it is recommended to choose the Microsoft Windows",
                                            " (WINDOWS-1252), Mac (macintosh) or Linux (UTF-8) encoding."), 
                                     l10.hint_outputformat)))
    tcltk::tkbind(l8.hint_whattoextr, "<Enter>", 
                  expression(tooltip(paste0("The PDE_analyzer has 2 main functions 1) PDF2TXT (extract ",
                                            "sentences from PDF files) 2) PDF2TABLE (table of PDF to ",
                                            "Microsoft Excel file) which can be combined or executed ",
                                            "separately. Accoridngly, the algorithm can either extract",
                                            " \"sentences\", \"tables\", ",
                                            "or \"both\" with one of the search words ",
                                            "present. Each function can be combined with filters ",
                                            "and search words. "), 
                                     l8.hint_whattoextr)))
    tcltk::tkbind(l19.hint_extractallorsent, "<Enter>", 
                  expression(tooltip(paste0("If the \"tables\" only analysis was chosen, the ",
                                            "algorithm can also extract all tables detected in the paper."), 
                                     l19.hint_extractallorsent)))
    tcltk::tkbind(l20.hint_choose_sw, "<Enter>", 
                  expression(tooltip(paste0("Type in the list of search words separated by \";\" without",
                                            " spaces in between. The list of search words includes all ",
                                            "aliases. Additionally, search word categories can be added",
                                            " by including the category name before the first search word,",
                                            " of each category surrounded by \"%:\" and \":%\", e.g., ",
                                            "%:category:%first search word. For each category word counts ",
                                            "will be summarized in the PDE_analyzer_word_stats.csv file."), 
                                     l20.hint_choose_sw)))
    tcltk::tkbind(l21.hint_ignorecase_sw, "<Enter>", 
                  expression(tooltip(paste0("Case-sensitivity (capitalization matters) can be disabled, ",
                                            "e.g., for \"Word\", if \"no\"",
                                            "was chosen then \"word\", \"WORD\", \"Word\", etc., will be ",
                                            "detected, if \"yes\" was chosen only \"Word\" will be detected."), 
                                     l21.hint_ignorecase_sw)))
    tcltk::tkbind(l22.hint_context, "<Enter>", 
                  expression(tooltip(paste0("When 0 is chosen, only the sentence with the search word is ",
                                            "extracted. If any number n is chosen, n number of sentences ",
                                            "before and n number of sentences after the sentence with the ",
                                            "search word will be extracted. A sentence is currently defined ",
                                            "by starting and ending with a \". \" (period with a ",
                                            "subsequent space)."), 
                                     l22.hint_context)))
    tcltk::tkbind(l23.hint_evalabrev, "<Enter>", 
                  expression(tooltip(paste0("If \"yes\" was chosen, all abbreviations that were used in ",
                                            "the PDF documents for the search words will be saved and then ",
                                            "replaced by the abbreviation (search word)$*, e.g., MTX will ",
                                            "be replaced by MTX (Methotrexate)$*. In addition plural ",
                                            "versions of the abbreviations, i.e., the abbreviation with ",
                                            "an \"s\" at the end will be replaced accordingly as well."), 
                                     l23.hint_evalabrev)))
    tcltk::tkbind(l15.hint_fw_yesno, "<Enter>", 
                  expression(tooltip(paste0("In some cases, only articles of a certain topic should be ",
                                            "analyzed. Filter words provide a way to analyze only articles",
                                            " which carry words from a list at least n times."), 
                                     l15.hint_fw_yesno)))
    tcltk::tkbind(l16.hint_choose_fw, "<Enter>", 
                  expression(tooltip(paste0("Type in the list of filter words separated by \";\" without",
                                            " spaces in between. A hit will be counted every time a word ",
                                            "from the list is detected in the article."), 
                                     l16.hint_choose_fw)))
    tcltk::tkbind(l17.hint_igrnorecase_fw, "<Enter>", 
                  expression(tooltip(paste0("Case-sensitivity (capitalization matters) can be disabled, ",
                                            "e.g., for \"Word\", if \"no\"",
                                            "was chosen then \"word\", \"WORD\", \"Word\", etc., will be ",
                                            "detected, if \"yes\" was chosen only \"Word\" will be detected."), 
                                     l17.hint_igrnorecase_fw)))
    tcltk::tkbind(l18.1.hint_fwtimes, "<Enter>", 
                  expression(tooltip(paste0("This represents the minimum number of hits described above ",
                                            "which has to be detected for a paper to be further analyzed. ",
                                            "If the threshold is not met, a documentation file can be ",
                                            "exported if selected in the documentation section. ",
                                            "If \"5\" is selected the number of hits is divided ",
                                            "by the total number of words"), 
                                     l18.1.hint_fwtimes)))
    tcltk::tkbind(l12.hint_tableheading, "<Enter>", 
                  expression(tooltip(paste0("Standard scientific articles have their tables labeled with ",
                                            "\"TABLE\", \"TAB\", \"Table\" or \"table\" plus number ",
                                            "and are detected accordingly. If a table is expected to ",
                                            "have a different heading, it should be typed in this ",
                                            "field. For multiple different heading use \";\" without",
                                            " extra spaces. For most scientific papers, this option ",
                                            "is not necessary to be populated as it is of greater use",
                                            " in extracting tables from non-journal articles."), 
                                     l12.hint_tableheading)))
    tcltk::tkbind(l13.hint_ignorecase_th, "<Enter>", 
                  expression(tooltip(paste0("Case-sensitivity (capitalization matters) can be disabled, ",
                                            "e.g., for \"HEADING\", if \"no\"",
                                            "was chosen then \"HEADING\", \"heading\", \"Heading\", etc., will be ",
                                            "detected, if \"yes\" was chosen only \"HEADING\" will be detected."), 
                                     l13.hint_ignorecase_th)))
    tcltk::tkbind(l14.1.hint_devx, "<Enter>", 
                  expression(tooltip(paste0("For some tables the heading is slightly indented which would ",
                                            "make the algorithm assume it was a separated column. With ",
                                            "the column pixel deviation the size of indention which would",
                                            " be considered the same column can be adjusted."), 
                                     l14.1.hint_devx)))
    tcltk::tkbind(l14.2.hint_devy, "<Enter>", 
                  expression(tooltip(paste0("For some tables elements even though in the same row can ",
                                            "have slightly different vertical coordiates. With the row ",
                                            "pixel deviation the variation of vertical coordinates which",
                                            " would be considered the same row can be adjusted. It can ",
                                            "be either a number or set to dynamic detection [9999], in ",
                                            "which case the font size is used to detect which words are ",
                                            "in the same row."), 
                                     l14.2.hint_devy)))
    tcltk::tkbind(l25.hint_wtl, "<Enter>", 
                  expression(tooltip(paste0("This option is commonly not necessary to be selected! ",
                                            "When \"tables\" detection/export is chosen, this option ",
                                            "will be relevant. For \textbf{yes}, a separate file with ",
                                            "the headings of all tables, their relative location in the",
                                            " generated HTML and TXT files, as well as information if ",
                                            "search words were found will be generated. The files will ",
                                            "start with \"htmltablelines\", \"txttablelines\", ",
                                            "\"keeplayouttablelines\" followed by the PDF file name and",
                                            " can be found in html.docu, txt.docu, keeptxt.docu subfolders.",
                                            "Nonetheless, it helps to identify if the PDE detects the ",
                                            "tables and, if yes, if they are exported. When comparing ",
                                            "the files starting with the PDF file name followed by ",
                                            "\"htmltablelines\", \"txttablelines\", \"keeplayouttablelines\"",
                                            ", it can be observed that all detected tables contained ",
                                            "at least one of the search words."), 
                                     l25.hint_wtl)))
    tcltk::tkbind(l26.hint_exportnondetc, "<Enter>", 
                  expression(tooltip(paste0("For \"yes\", if a table was detected in a PDF file ",
                                            "but is an image or cannot be read, the page with the ",
                                            "table will be exported as a portable network graphics ",
                                            "(PNG) file. The documentation file will have the name ",
                                            "format: [PDF file name]page[page number]w.table-[page number].png",
                                            "This is recommended to capture all tables, even if the program ",
                                            "cannot detect the table content. This applies especially, for ",
                                            "older articles with scanned tables."), 
                                     l26.hint_exportnondetc)))
    tcltk::tkbind(l27.hint_writetabdoc, "<Enter>", 
                  expression(tooltip(paste0("For \"yes\", if search words are used for table detection ",
                                            "and no search words were found in the tables of a PDF file,",
                                            " a file will be created with the PDF file name followed by ",
                                            "\"no.table.w.search.words\" in the folder with the name ",
                                            "no_tab_w_sw."), 
                                     l27.hint_writetabdoc)))
    tcltk::tkbind(l28.hint_writetxtdoc, "<Enter>", 
                  expression(tooltip(paste0("For \"yes\", if no search words were found in the sentences",
                                            " of a PDF file, a file will be created with the PDF file ",
                                            "name followed by \"no.txt.w.search.words\" in the ",
                                            "no_txt_w_sw folder. If the PDF file is empty, a file will ",
                                            "be created with the PDF file name followed by \"non-readable\"",
                                            " in the nr folder. Files that were filtered out using ",
                                            "the filterwords will lead to the creation of a file with ",
                                            "the PDF file name followed by \"no.txt.w.filter.words\" in ",
                                            "the excl_by_fw folder. This option does not influence ",
                                            "the creation of the [id]_is_secured.txt file in the ",
                                            "\"secured folder\"."), 
                                     l28.hint_writetxtdoc)))
    tcltk::tkbind(l29.hint_delete, "<Enter>", 
                  expression(tooltip(paste0("This option is primarily for debugging! ",
                                            "The program generates a txt, keeplayouttxt and HTML ",
                                            "copy of the PDF file, which will be deleted if ",
                                            "intermediate files deletion is chosen. In case, this ",
                                            "option was chosen accidentally, the user has two ",
                                            "options to delete the .txt and .html file. 1) Slow & ",
                                            "easy option: Rerun the analysis with this option being ",
                                            "yes. 2) Quick and slightly more complicated option: ",
                                            "Open the file explorer and search for *.txt and *.html ",
                                            "in the PDF folder. Then select all files and folders of ",
                                            "the search result and press delete. Having access to the .txt ",
                                            "and .html files will allow the identification of ",
                                            "undetected tables/sentences or conversion issues."), 
                                     l29.hint_delete)))
    
    # tcltk::tkbind(l7.hint_choosepdf, "<Button-1>",
    #               function() {
    #                 utils::browseURL("https://cran.r-project.org/web/packages/PDE/index.html")
    #               })
    
    tcltk::tkpack(PDE.globals$le.progress.textbox, le.save.txt.but, side="left",fill="x", pady= 2)
    tcltk::tkpack(le.progress.bar.pb, PDE.globals$le.progress.textbox, side="top",fill="x", pady= 2)
    tcltk::tkpack(start.pause.but, quit.but, side="left",pady= 2, padx= 5)
    tcltk::tkpack(le,expand=TRUE,fill="x", side="left",pady= 2, padx= 5)
    tcltk::tkpack(e, side="top",fill="x")
    
    tcltk::tkfocus(PDE.globals$ttanalyzer)
    tcltk::tkraise(PDE.globals$ttanalyzer)
  } ## end tcltk check
}

#'Extracting data from PDF (Portable Document Format) files
#'
#'The \code{PDE_analyzer} allows the sentence and table extraction from multiple
#'PDF files.
#'
#'@param PDE_parameters_file_path String. This file includes all parameters to
#'  run \code{\link{PDE_extr_data_from_pdfs}} on multiple PDF files. If
#'  \code{PDE_parameters_file_path} does not exist or is \code{NA} a dialog box
#'  is opened prompting the user to select the parameter file.
#'@param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#'@section Details: The parameter file (also referred to as .tsv file) can
#'  either manually or with the help of the \code{\link{PDE_analyzer_i}}
#'  interface be filled.
#'
#'@section Note: A detailed description of the parameters in the TSV file can be
#'  found in the markdown file (README_PDE.md) and in the description of
#'  \code{\link{PDE_extr_data_from_pdfs}}.
#'
#'@return If tables were extracted from the PDF file the function returns a list of
#'  following tables/items: 1) \strong{htmltablelines}, 2)
#'  \strong{txttablelines}, 3) \strong{keeplayouttxttablelines}, 4) \strong{id},
#'  5) \strong{out_msg}.
#'  The \strong{tablelines} are tables that provide the heading and position of
#'  the detected tables. The \strong{id} provide the name of the PDF file. The
#'  \strong{out_msg} includes all messages printed to the console or the suppressed
#'  messages if \code{verbose=FALSE}.
#'
#' @examples
#'  if(PDE_check_Xpdf_install() == TRUE){
#'    PDE_analyzer(paste0(system.file(package = "PDE"),
#'    "/examples/tsvs/PDE_parameters_v1.4_all_files+-0.tsv"))
#'  }
#'
#' \dontrun{
#'  ## requires user file choice:
#'  PDE_analyzer()
#' }
#'  
#'@seealso \code{\link{PDE_extr_data_from_pdfs}}
#'
#'@export
PDE_analyzer <- function(PDE_parameters_file_path = NA, verbose=TRUE){
  
  ## give folder and automate analysis ----------------------------------------------
  if (!is.na(PDE_parameters_file_path)) {
    ### read in the content
    values_table <- utils::read.table(PDE_parameters_file_path,
                                      sep = "\t", header = TRUE, quote = "\"")
  } else {
    tsv_location <- file.choose()
    if (file.exists(tsv_location)) {
      values_table <- utils::read.table(tsv_location, sep = "\t",
                                        header = TRUE, quote = "\"")
    } else {
      tcltk::tkmessageBox(title = "Warning",
                          type = "ok", icon = "warning",
                          message = paste0(tsv_location," does not exist"))
      stop(paste0(tsv_location," does not exist"))
    }
  }
  
  
  
  ## the mandatory variables for running
  search.wds <- as.character(values_table[(grep("search.words",
                                                values_table[, "variable"])), "value"])
  if (length(grep(";", search.wds)) > 0) {
    search.for <- strsplit(search.wds, ";")[[1]]
  } else {
    search.for <- search.wds
  }
  search.word.categories <- NULL  
  ## if categories are in the search word list
  if (grepl("%:(.)+:%",search.wds)){
    category <- NULL
    for (sf in 1:length(search.for)){
      if (grepl("%:(.)+:%",search.for[sf])){
        category <- gsub("%:(.+):%.*", "\\1", search.for[sf])
        search.for[sf] <- sub(paste0("%:",category,":%"),"",search.for[sf])
        search.word.categories <- c(search.word.categories,category)
      } else {
        if (is.null(category)){
          tcltk::tkmessageBox(title = "Warning",
                              type = "ok", icon = "warning",
                              message = paste0("Choose the correct formating",
                                               " for categories ",
                                               "(e.g. %:category:%first search word)"))
          stop(paste0("Choose the correct formating",
                      " for categories ",
                      "(e.g. %:category:%first search word)"))
        }
        search.word.categories <- c(search.word.categories,category)
      }
    }
  }
  
  ## reg.sw
  regex.sw <- as.logical(values_table[(grep("regex.sw",
                                            values_table[, "variable"])), "value"])
  
  ## ignore.case.sw
  ic.sw <- as.logical(values_table[(grep("ignore.case.sw",
                                         values_table[, "variable"])), "value"])
  if (length(ic.sw) == 0 || is.na(ic.sw)) ic.sw <- FALSE
  
  ## write.table.locations (generates 3 tables with
  ## the locations of the tables within the txt/html
  ## files)
  
  whattoextr <- sub(" ", "", as.character(values_table[(grep("whattoextr",
                                                             values_table[, "variable"])), "value"]))
  wtl <- as.logical(values_table[(grep("write.table.locations",
                                       values_table[, "variable"])), "value"])
  
  outputfolder <- as.character(values_table[(grep("outputfolder",
                                                  values_table[, "variable"])), "value"])
  
  if (!(length(outputfolder) == 0 || is.na(outputfolder))) {
    if (grepl("^/examples/MTX", outputfolder) || grepl("^examples/MTX", outputfolder)){
      outputfolder <- paste0(system.file(package = "PDE"),"/",outputfolder)
    } else {
      ## outputfolder stays the same
    }
  } else {
    outputfolder <- ""
  }
  
  
  pdfs <- as.character(values_table[(grep("pdfs",
                                          values_table[, "variable"])), "value"])
  if (length(pdfs) == 0 || is.na(pdfs)) {
    pdfs <- as.character(values_table[(grep("pdffolder",
                                            values_table[, "variable"])), "value"])
  }
  
  if (length(pdfs) == 0 || is.na(pdfs)) {
    pdfs <- ""
  }
  
  ## if it is a directory
  ## if it is a single file
  ## if example working directory
  if (dir.exists(pdfs) || 
      file.exists(pdfs) ||
      all(file.exists(unlist(strsplit(pdfs,
                                      ";"))))) {
    pdfs <- pdfs
    ## if it is a directory
    ## if it is a single file
  } else if (dir.exists(paste0(system.file(package = "PDE"),"/",pdfs)) ||
             file.exists(paste0(system.file(package = "PDE"),"/",pdfs))) {
    pdfs <- paste0(system.file(package = "PDE"),"/",pdfs)
  } else if (all(file.exists(paste0(system.file(package = "PDE"),"/",unlist(strsplit(pdfs,
                                                                                     ";")))))) {
    pdfs <- paste0(paste0(system.file(package = "PDE"),"/",unlist(strsplit(pdfs,
                                                                           ";"))), collapse = ";")
  }
  
  
  ## test if whattoextr files exist
  if (!(whattoextr == "txtandtab" ||
        whattoextr == "tabandtxt" ||
        whattoextr == "tab" ||
        whattoextr == "table" ||
        whattoextr == "txt")) {
    tcltk::tkmessageBox(title = "Warning",
                        type = "ok", icon = "warning",
                        message = "Please check your PDE_parameters_v1.4_all_files+-0.tsv file for the correct whattoextr")
    stop("Please check your PDE_parameters_v1.4_all_files+-0.tsv file for the correct whattoextr")
  }
  
  
  ## preset the additional parameters filter words
  filter.wds <- as.character(values_table[(grep("^filter.words$",
                                                values_table[, "variable"])), "value"])
  ## reg.fw
  regex.fw <- as.logical(values_table[(grep("regex.fw",
                                            values_table[, "variable"])), "value"])
  if (length(filter.wds) == 0 || is.na(filter.wds)) filter.wds <- ""
  if (length(grep(";", filter.wds)) > 0) {
    filter.for <- strsplit(filter.wds, ";")[[1]]
  } else {
    filter.for <- filter.wds
  }
  
  ## ignore.case.fw
  ic.fw <- as.logical(values_table[(grep("ignore.case.fw",
                                         values_table[, "variable"])), "value"])
  if (length(ic.fw) == 0 || is.na(ic.fw)) ic.fw <- FALSE
  
  ## filter.word.times
  filter.word.times <- strtoi(values_table[(grep("filter.word.times",
                                                 values_table[, "variable"])), "value"])
  if (length(filter.word.times) == 0 || is.na(filter.word.times)) filter.word.times <- 1
  
  ## table headings
  table.heading.wds <- as.character(values_table[(grep("table.heading.words",
                                                       values_table[, "variable"])), "value"])
  if (length(table.heading.wds) == 0 || is.na(table.heading.wds)) table.heading.wds <- ""
  if (length(grep(";", table.heading.wds)) > 0) {
    table.heading.for <- strsplit(table.heading.wds, ";")[[1]]
  } else {
    table.heading.for <- table.heading.wds
  }
  ## ignore.case.th
  ic.th <- as.logical(values_table[(grep("ignore.case.th",
                                         values_table[, "variable"])), "value"])
  if (length(ic.th) == 0 || is.na(ic.th)) ic.th <- FALSE
  
  id <- as.character(values_table[(grep("id", values_table[,
                                                           "variable"])), "value"])
  if (length(id) == 0 || is.na(id)) id <- NA
  
  write.table.locations <- as.logical(values_table[(grep("write.table.locations",
                                                         values_table[, "variable"])), "value"])
  if (length(write.table.locations) == 0 || is.na(write.table.locations)) write.table.locations <- FALSE
  
  out.table.format <- as.character(values_table[(grep("out.table.format",
                                                      values_table[, "variable"])), "value"])
  if (length(out.table.format) == 0 || is.na(out.table.format)) out.table.format <- ".csv (WINDOWS-1252)"
  
  dev_x <- strtoi(values_table[(grep("dev_x", values_table[,
                                                           "variable"])), "value"])
  if (length(dev_x) == 0 || is.na(dev_x)) dev_x <- 20
  
  dev_y <- strtoi(values_table[(grep("dev_y", values_table[,
                                                           "variable"])), "value"])
  if (length(dev_y) == 0 || is.na(dev_y)) dev_y <- 9999
  
  context <- strtoi(values_table[(grep("context", values_table[,
                                                               "variable"])), "value"])
  if (length(context) == 0 || is.na(context)) context <- 2
  
  eval.abbrevs <- strtoi(values_table[(grep("eval.abbrevs", values_table[,
                                                                         "variable"])), "value"])
  if (length(eval.abbrevs) == 0 || is.na(eval.abbrevs)) eval.abbrevs <- TRUE
  
  write.tab.doc.file <- as.logical(values_table[(grep("write.tab.doc.file",
                                                      values_table[, "variable"])), "value"])
  if (length(write.tab.doc.file) == 0 || is.na(write.tab.doc.file)) write.tab.doc.file <- FALSE
  
  write.txt.doc.file <- as.logical(values_table[(grep("write.txt.doc.file",
                                                      values_table[, "variable"])), "value"])
  if (length(write.txt.doc.file) == 0 || is.na(write.txt.doc.file)) write.txt.doc.file <- FALSE
  
  exp.nondetc.tabs <- as.logical(values_table[(grep("exp.nondetc.tabs",
                                                    values_table[, "variable"])), "value"])
  if (length(exp.nondetc.tabs) == 0 || is.na(exp.nondetc.tabs)) exp.nondetc.tabs <- TRUE
  
  cpy_mv <- as.character(values_table[(grep("cpy_mv", values_table[,
                                                                 "variable"])), "value"])
  if (length(cpy_mv) == 0 || is.na(cpy_mv)) cpy_mv <- "nocpymv"
  
  delete <- as.logical(values_table[(grep("delete", values_table[,
                                                                 "variable"])), "value"])
  if (length(delete) == 0 || is.na(delete)) delete <- TRUE
  
  ## as control show content of variables --------------------------------------------------
  if (verbose){
    cat(c("whattoextr", dQuote(whattoextr)),
        sep = "\n")
    cat(c("filter.words:", filter.for, "regex.fw:", 
          regex.fw, "ignore.case.fw:",
          ic.fw, "filter.word.times:", filter.word.times,
          "table.heading.words:", table.heading.for, "ignore.case.th:",
          ic.th), sep = "\n")
    cat(c("search.words:", search.for, "regex.sw:", 
          regex.sw,"ignore.case.sw:",
          ic.sw, "eval.abbrevs:",
          eval.abbrevs, "write.table.locations:", wtl, "out.table.format:",
          dQuote(out.table.format)), sep = "\n")
    cat(c("dev_x:", dev_x, "dev_y:", dev_y, "context:", context, "write.tab.doc.file:",
          write.tab.doc.file, "write.txt.doc.file:", write.txt.doc.file,
          "cpy_mv", cpy_mv, "delete", delete, "exp.nondetc.tabs:", exp.nondetc.tabs),
        sep = "\n")
  }
  
  ## load pdf(s) --------------------------------------------------------------------
  ## if it is a directory
  if (dir.exists(pdfs)) {
    pdf.files <- list.files(pdfs, pattern = "*.pdf",
                            full.names = TRUE, recursive = TRUE)
    ## if it is a single file
  } else if (file.exists(pdfs)) {
    pdf.files <- pdfs
  } else if (all(file.exists(unlist(strsplit(pdfs, ";"))))) {
    pdf.files <- unlist(strsplit(pdfs, ";"))
  }
  
  output <- NULL
  
  ## run the analysis ----------------------------------------------------------------
  for (pdf in pdf.files) {
    
    tablelines <- .PDE_extr_data_from_pdf(pdf = pdf,
                                          whattoextr = whattoextr, 
                                          out = outputfolder, context = context,
                                          dev_x = dev_x, dev_y = dev_y, filter.words = filter.for,
                                          regex.fw = regex.fw,
                                          ignore.case.fw = ic.fw, filter.word.times = filter.word.times,
                                          table.heading.words = table.heading.for,
                                          ignore.case.th = ic.th, search.words = search.for,
                                          search.word.categories = search.word.categories,
                                          regex.sw = regex.sw,
                                          ignore.case.sw = ic.sw, eval.abbrevs = eval.abbrevs,
                                          write.table.locations = wtl,
                                          write.tab.doc.file = write.tab.doc.file,
                                          write.txt.doc.file = write.txt.doc.file,
                                          exp.nondetc.tabs = exp.nondetc.tabs,
                                          out.table.format = out.table.format,
                                          delete = delete, cpy_mv = cpy_mv, verbose = verbose)
    
    ## if the algorithm was not run in table detection
    ## setting tablelines is NULL
    if (length(tablelines) > 0) {
      
      # ## add new tablelines to output
      # output[[length(output) + 1]] <- tablelines
      ##make stat_output mastertable
      if (is.null(output)){
        output <- tablelines$stat_output
      } else {
        output[setdiff(names(tablelines$stat_output), names(output))] <- NA
        tablelines$stat_output[setdiff(names(output), names(tablelines$stat_output))] <- NA
        output <- rbind(output,tablelines$stat_output)
      }
    }
    
  }  ## end for each pdf
  
  ## write the PDE_analyzer_word_stats table
  if ("pdf_searchword_total" %in% colnames(output)){
    if ("pdf_filterword_total" %in% colnames(output)){
      output <- output[order(output$pdf_searchword_total,
                             output$pdf_filterword_total, decreasing = TRUE),,drop=FALSE]
    } else {
      output <- output[order(output$pdf_searchword_total, decreasing = TRUE),,drop=FALSE]
    }
  }
  todays.date_time <- paste0(format(Sys.Date(), "%Y-%m-%d"),format(Sys.time(), "-%Hh-%Mm"))
  if (grepl("csv", out.table.format)) {
    out.table.separator <- ","
    out.table.ext <- ".csv"
  }
  if (grepl("tsv", out.table.format)) {
    out.table.separator <- "\t"
    out.table.ext <- ".tsv"
  }
  output_table <- cbind(pdf_file_name = rownames(output), output)
  utils::write.table(output_table, file = paste0(outputfolder,"/",todays.date_time,"_PDE_analyzer_word_stats",
                                                 out.table.ext),
                     sep = out.table.separator,
                     row.names = FALSE)
  
  return(output)
}

## Start the PDE reader ------------------------------------------
#' Browsing the PDE (PDF Data Extractor) analyzer results.
#'
#' The \code{PDE_reader_i} allows the user-friendly visualization and quick-processing of the obtained results.
#'
#' @param verbose Logical. Indicates whether messages will be printed in the console. Default: \code{TRUE}.
#'
#' @section Note:
#' A detailed description of the elements in the user interface can be found in the markdown file (README_PDE.md)
#'
#' @import tcltk tcltk2
#' 
#' @examples
#'  
#'  PDE_reader_i()
#'  
#' @export
PDE_reader_i <- function(verbose=TRUE) {
  
  ## general functions ------------------------------------------
  read.problemtable <- function(filelocation, header,
                                quote, sep) {
    if (file.exists(filelocation)){
      text <- readLines(filelocation, warn = FALSE, encoding = "UTF-8")
      colnumbheader <- ncol(utils::read.table(text = text[1],
                                              quote = quote, sep = sep, header = FALSE, stringsAsFactors = FALSE,
                                              encoding = "UTF-8"))
      for (l in 2:length(text)) {
        ## 1)try to build a table
        res <- try(utils::read.table(text = text[l], quote = quote,
                                     sep = sep, header = FALSE, stringsAsFactors = FALSE,
                                     encoding = "UTF-8"),
                   silent = TRUE)
        if (inherits(res,"try-error")) {
          quotnumb <- unlist(gregexpr("\"", text[l]))
          ## if the number of quotations is odd
          if ((length(quotnumb) %% 2) != 0) {
            ## test for multibyte string
            res_mbs <- try(substr(text[l], quotnumb[2], quotnumb[2]),
                           silent = TRUE)
            if (inherits(res_mbs,"try-error")) {
              text[l] = iconv(text[l], 'UTF-8', 'latin1', 'bit')
            }
            quotnumb <- unlist(gregexpr("\"", text[l]))
            substr(text[l], quotnumb[2], quotnumb[2]) <- "'"
          }
        }
        ## test if the column number is right
        colnumbtext <- ncol(utils::read.table(text = text[l],
                                              quote = quote, sep = sep, header = FALSE,
                                              stringsAsFactors = FALSE,
                                              encoding = "UTF-8"))
        max <- 20
        while (colnumbheader != colnumbtext) {
          max <- max - 1
          quotnumb <- unlist(gregexpr("\"", text[l]))
          ## if the number of quotations is odd
          if ((length(quotnumb) %% 2) != 0) {
            ## test for multibyte string
            res_mbs <- try(substr(text[l], quotnumb[2], quotnumb[2]),
                           silent = TRUE)
            if (inherits(res_mbs,"try-error")) {
              text[l] = iconv(text[l], 'UTF-8', 'latin1', 'bit')
            }
            substr(text[l], quotnumb[2], quotnumb[2]) <- "'"
          } else {
            ## test for multibyte string
            res_mbs1 <- try(substr(text[l], quotnumb[2], quotnumb[2]),
                            silent = TRUE)
            res_mbs2 <- try(substr(text[l], quotnumb[3], quotnumb[3]),
                            silent = TRUE)
            if (inherits(res_mbs1,"try-error") || inherits(res_mbs2,"try-error")) {
              text[l] = iconv(text[l], 'UTF-8', 'latin1', 'bit')
            }
            quotnumb <- unlist(gregexpr("\"", text[l]))
            substr(text[l], quotnumb[2], quotnumb[2]) <- "'"
            substr(text[l], quotnumb[3], quotnumb[3]) <- "'"
          }
          if (max != 0 || length(quotnumb) == 4) {
            colnumbtext <- ncol(utils::read.table(text = text[l],
                                                  quote = quote, sep = sep, header = FALSE,
                                                  stringsAsFactors = FALSE,
                                                  encoding = "UTF-8"))
          } else {
            colnumbtext <- colnumbheader
          }
          
        }
        
      }
      
      table <- utils::read.table(text = text, quote = quote,
                                 sep = sep, fill = TRUE, header = header, stringsAsFactors = FALSE,
                                 encoding = "UTF-8")
      ## if file does not exist
    } else {
      table <- ""
    }
    return(table)
  }
  
  ## set the variables ----------------------------------------------------------------
  out_msg <- NULL
  filename.var <- tcltk::tclVar("")
  pdffolder.var <- tcltk::tclVar("")
  fullpdfname.var <- tcltk::tclVar("")
  pdfname.var <- tcltk::tclVar("")
  pdfnumber.var <- tcltk::tclVar("")
  PDE.globals$jumpto.list <- ""
  jumpto.var <- tcltk::tclVar("")
  PDE.globals$mark.list <- ""
  mark.var <- tcltk::tclVar("")
  l6.analysis.file.table <- NULL
  columnnumber.var <- tcltk::tclVar("0")
  PDE.globals$tables.masterlist <- NULL
  analysis.folder_location.var <- tcltk::tclVar("")
  wrap.var <- tcltk::tclVar("1")
  txtcontent.only.var <- tcltk::tclVar("0")
  collapse.abbrev.var <- tcltk::tclVar("0")
  progress.var <- tcltk::tclVar("0")
  tsvfile.var <- tcltk::tclVar("")
  tsv.onoff.var <- tcltk::tclVar("off")
  search.words.var <- tcltk::tclVar("")
  ignore.case.sw.var <- tcltk::tclVar("")
  hotkey.mode.var <- tcltk::tclVar("standard")
  sent.count.indicator.var <- tcltk::tclVar("0")
  
  ## find os.font
  if (Sys.info()["sysname"] == "Windows") {
    os.font <- "Arial"
    os.font.ten.bold <- "Arial 10 bold"
    os.font.twelve.bold <- "Arial 12 bold"
  } else if (Sys.info()["sysname"] == "Darwin") {
    os.font <- "Helvetica"
    os.font.ten.bold <- "Helvetica 10 bold"
    os.font.twelve.bold <- "Helvetica 12 bold"
  } else if (Sys.info()["sysname"] == "Linux" || Sys.info()["sysname"] == "SunOS") {
    os.font <- grDevices::X11Fonts()$sans
    os.font.ten.bold <- paste0(os.font, " 10 bold")
    os.font.twelve.bold <- paste0(os.font, " 12 bold")
  } else {
    os.font <- "Arial"
    os.font.ten.bold <- "Arial 10 bold"
    os.font.twelve.bold <- "Arial 12 bold"
  }
  
  ## components of the form ----------------------------------------------------
  tcltk.test <- try(test <- tcltk::tktoplevel(bg = "#05F2DB"), silent = TRUE)
  tcltk.test2 <- try(tcltk::tkdestroy(test), silent = TRUE)
  if (inherits(tcltk.test,"try-error")){
    out_msg <- c(out_msg, paste("Package tcltk is not working properly.",
                                "If you are on a Mac try installing the latest version of xquartz to use tcltk correctly."))
    if (verbose) cat(utils::tail(out_msg,1), sep="\n")
  } else {
    ## if tcltk works continue
    ttreader <- tcltk::tktoplevel(bg = "#05F2DB")
    tcltk::tkwm.geometry(ttreader, "+0+0")
    tcltk::tkwm.title(ttreader, "PDE reader")
    
    ## style ---------------------------------------------------------------------
    themes <- try(tcltk2::tk2theme.list(), silent = TRUE)
    if (!inherits(themes, "try-error")) {
      if ("vista" %in% themes) {
        # This must be aquaTk on a Mac
        try(tcltk2::tk2theme("vista"), silent = TRUE)
      } else if ("clam" %in% themes) {
        try(tcltk2::tk2theme("clam"), silent = TRUE)
      } else if ("aquaTk" %in% themes) {
        # This must be Vista or Win 7
        try(tcltk2::tk2theme("aquaTk"), silent = TRUE)
      }
    }
    
    ## min size so that no buttons disappear vertically
    tcltk::tkwm.minsize(ttreader, 750, 0)
    
    
    ## frames ---------------------------------------------------
    
    ## right frame
    top <- tcltk::tkframe(ttreader, bg = "#05F2DB")
    ## line 1 with buttons
    l1 <- tcltk::tkframe(top, bg = "#05F2DB")
    l2 <- tcltk::tkframe(top, bg = "#05F2DB")
    l3 <- tcltk::tkframe(top, bg = "#05F2DB")
    l4 <- tcltk::tkframe(top, bg = "#05F2DB")
    l5 <- tcltk::tkframe(top, bg = "#05F2DB")
    mid <- tcltk::tkframe(ttreader, borderwidth = 1, relief = "solid",
                          bg = "#05F2C7")
    l6 <- tcltk::tkframe(mid, bg = "#05F2C7")
    l7 <- tcltk::tkframe(mid, bg = "#05F2C7")
    l8 <- tcltk::tkframe(mid, bg = "#05F2C7")
    bot <- tcltk::tkframe(ttreader, bg = "#05F2DB")
    l9 <- tcltk::tkframe(bot, bg = "#05F2DB")
    
    ## test os.font ------------------------------------------------------------------------
    res <- try(tcltk2::tk2label(l1, text = "test", font = os.font.ten.bold),silent = TRUE)
    if (inherits(res[1],"try-error")) {
      out_msg <- c(out_msg, "There is an error with the allocation of tcl system fonts.")
      if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      os.font <- ""
      os.font.ten.bold <- ""
      os.font.twelve.bold <- ""
    }
    
    
    ## labels -----------------------------------------------------------------
    l1.progress.label <- tcltk2::tk2label(l1, text = tcltk::tclvalue(progress.var),
                                          background = "#05F2DB")
    l5.jumpto.label <- tcltk2::tk2label(l5, text = "Jump to file:",
                                        background = "#05F2DB")
    l6.caption.label <- tcltk2::tk2label(l6, text = "Table:",
                                         font = os.font.twelve.bold, background = "#05F2C7")
    l6.font.label <- tcltk2::tk2label(l6, text = "font size",
                                      background = "#05F2C7")
    l6.hotkey.mode.label <- tcltk2::tk2label(l6, text = "hotkey mode:",
                                             background = "#05F2C7")
    l6.wrap.label <- tcltk2::tk2label(l6, text = "wrap", background = "#05F2C7")
    l6.nowrap.label <- tcltk2::tk2label(l6, text = "don't wrap (for window resizing)",
                                        background = "#05F2C7")
    l7.sentence.number.label <- tcltk2::tk2label(l7, text = "          sentence number:",
                                                 background = "#05F2C7")
    l9.caption.label <- tcltk2::tk2label(l9, text = "Mark:",
                                         background = "#05F2DB")
    
    ## entry boxes -----------------------------------------------------
    l3.pdffolder.entry <- tcltk2::tk2entry(l3, textvariable = pdffolder.var,
                                           width = 3)
    l2.loadtsv.entry <- tcltk2::tk2entry(l2, textvariable = tsvfile.var,
                                         width = 3)
    l4.pdfname.entry <- tcltk2::tk2entry(l4, textvariable = pdfname.var,
                                         width = 10, state = "readonly")
    
    
    ## progress bar ---------------------------------------------------
    ## e
    l1.progress.bar.pb <- tcltk2::tk2progress(l1, value = 0,
                                              maximum = 100, length = 50)
    
    ## scroll bar ----------------------------------------------------
    scroll.y <- tcltk2::tk2scrollbar(l8, orient = "vertical",
                                     command = function(...) tcltk::tkyview(l8.analysis.file.table,
                                                                            ...))  ## command that performs the scrolling
    l8.analysis.file.table <- tcltk2::tk2tablelist(l8, selectmode = "extended",
                                                   exportselection = 0, stripebackground = "lightgrey",
                                                   stretch = "anchor", selecttype = "cell", 
                                                   yscrollcommand = function(...) tcltk::tkset(scroll.y,...))
    
    
    ## buttons 1 -----------------------------------------------------------
    ## Here what happens when the buttons are pressed
    ## buttons 1
    
    renamefile <- function(completefilename, checkfor,
                           replacewith) {
      filebase <- basename(completefilename)
      filedir <- dirname(completefilename)
      newfilename <- filebase
      for (i in 1:length(checkfor)) {
        if (substr(newfilename, 1, 2) == checkfor[i])
          newfilename <- substr(newfilename, 3,
                                nchar(newfilename))
      }
      if (!substr(newfilename, 1, 2) == replacewith)
        newfilename <- paste0(replacewith, newfilename)
      file.rename(from = completefilename, to = paste(filedir,
                                                      newfilename, sep = "/"))
      ## rename in PDE.globals$tables.masterlist
      completefilename <- gsub("\\\\", "/", completefilename)
      numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
      pos <- as.numeric(numb)
      PDE.globals$tables.masterlist[["analysis.file_location"]][pos] <- list(paste(filedir,
                                                                                   newfilename, sep = "/"))
      return(newfilename)
    }
    
    flagfile <- function() {
      if (tcltk::tclvalue(mark.var) == "Mark analysis file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename analysisfile
        filename <- tcltk::tclvalue(filename.var)
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        tcltk::tclvalue(filename.var) <- renamefile(completefilename = fullfilename,
                                                    checkfor = c("x_"), replacewith = "!_")
        ## change filename also in jumpto.var
        numb <- gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        PDE.globals$jumpto.list[as.numeric(numb)] <- paste0(numb, "-",
                                                            tcltk::tclvalue(filename.var))
        tcltk::tclvalue(jumpto.var) <- paste0(numb, "-",
                                              tcltk::tclvalue(filename.var))
        if (length(PDE.globals$jumpto.list) > 1) {
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
        } else {
          tcltk::tkconfigure(l5.jumpto.cb, values = tcltk::as.tclObj(PDE.globals$jumpto.list,
                                                                     drop = FALSE))
        }
        
      }
      if (tcltk::tclvalue(mark.var) == "Mark PDF file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename PDF file
        if (!tcltk::tclvalue(pdfname.var) == "") {
          completefilename <- list.files(tcltk::tclvalue(pdffolder.var),
                                         pattern = tcltk::tclvalue(pdfname.var),
                                         full.names = TRUE, recursive = TRUE)[1]
          tcltk::tclvalue(pdfname.var) <- renamefile(completefilename = completefilename,
                                                     checkfor = c("x_"), replacewith = "!_")
          tcltk::tclvalue(fullpdfname.var) <- paste(dirname(completefilename),
                                                    tcltk::tclvalue(pdfname.var), sep = "/")
        }
      }
    }
    l9.flagfile.but <- tcltk2::tk2button(l9, text = "Flag file",
                                         width = 9, command = flagfile, state = "disabled",
                                         underline = "0")
    
    xmarkfile <- function() {
      if (tcltk::tclvalue(mark.var) == "Mark analysis file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename analysisfile
        filename <- tcltk::tclvalue(filename.var)
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        tcltk::tclvalue(filename.var) <- renamefile(completefilename = fullfilename,
                                                    checkfor = c("!_"), replacewith = "x_")
        ## change filename also in jumpto.var
        numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        PDE.globals$jumpto.list[as.numeric(numb)] <- paste0(numb, "-",
                                                            tcltk::tclvalue(filename.var))
        tcltk::tclvalue(jumpto.var) <- paste0(numb, "-",
                                              tcltk::tclvalue(filename.var))
        if (length(PDE.globals$jumpto.list) > 1) {
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
        } else {
          tcltk::tkconfigure(l5.jumpto.cb, values = tcltk::as.tclObj(PDE.globals$jumpto.list,
                                                                     drop = FALSE))
        }
      }
      if (tcltk::tclvalue(mark.var) == "Mark PDF file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename PDF file
        if (!tcltk::tclvalue(pdfname.var) == "") {
          completefilename <- list.files(tcltk::tclvalue(pdffolder.var),
                                         pattern = tcltk::tclvalue(pdfname.var),
                                         full.names = TRUE, recursive = TRUE)[1]
          tcltk::tclvalue(pdfname.var) <- renamefile(completefilename = completefilename,
                                                     checkfor = c("!_"), replacewith = "x_")
          tcltk::tclvalue(fullpdfname.var) <- paste(dirname(completefilename),
                                                    tcltk::tclvalue(pdfname.var), sep = "/")
        }
      }
    }
    l9.xmarkfile.but <- tcltk2::tk2button(l9, text = "x mark file",
                                          width = 11, command = xmarkfile, state = "disabled",
                                          underline = "0")
    
    unmarkfile <- function() {
      if (tcltk::tclvalue(mark.var) == "Mark analysis file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename analysisfile
        filename <- tcltk::tclvalue(filename.var)
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        tcltk::tclvalue(filename.var) <- renamefile(completefilename = fullfilename,
                                                    checkfor = c("!_", "x_"), replacewith = "")
        ## change filename also in jumpto.var
        numb <- gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        PDE.globals$jumpto.list[as.numeric(numb)] <- paste0(numb, "-",
                                                            tcltk::tclvalue(filename.var))
        tcltk::tclvalue(jumpto.var) <- paste0(numb, "-",
                                              tcltk::tclvalue(filename.var))
        if (length(PDE.globals$jumpto.list) > 1) {
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
        } else {
          tcltk::tkconfigure(l5.jumpto.cb, values = tcltk::as.tclObj(PDE.globals$jumpto.list,
                                                                     drop = FALSE))
        }
        
      }
      if (tcltk::tclvalue(mark.var) == "Mark PDF file only" ||
          tcltk::tclvalue(mark.var) == "Mark analysis file & PDF") {
        ## Rename PDF file
        if (!tcltk::tclvalue(pdfname.var) == "") {
          completefilename <- list.files(tcltk::tclvalue(pdffolder.var),
                                         pattern = tcltk::tclvalue(pdfname.var),
                                         full.names = TRUE, recursive = TRUE)[1]
          tcltk::tclvalue(pdfname.var) <- renamefile(completefilename = completefilename,
                                                     checkfor = c("x_", "!_"), replacewith = "")
          tcltk::tclvalue(fullpdfname.var) <- paste(dirname(completefilename),
                                                    tcltk::tclvalue(pdfname.var), sep = "/")
        }
      }
    }
    l9.unmarkfile.but <- tcltk2::tk2button(l9, text = "Unmark file",
                                           width = 11, command = unmarkfile, state = "disabled",
                                           underline = "0")
    
    ## functions --------------------------------------------------
    fill.table <- function(table, analysis.file_location) {
      included.columns <- !(colnames(table) %in% "search.word.loc_total")
      if (tcltk::tclvalue(txtcontent.only.var) == "1") {
        included.columns <- (colnames(table) %in% "txtcontent")
      }
      table <- table[, included.columns]
      ## clear table
      tcltk::tkdelete(l8.analysis.file.table, 0, "end")
      
      ## change the header
      tcltk::tclvalue(filename.var) <- as.character(basename(analysis.file_location))
      tcltk::tkwm.title(ttreader, paste0("PDE reader - ",tcltk::tclvalue(filename.var)))
      
      if (tcltk::tclvalue(txtcontent.only.var) == "0") {
        ## fill the header
        columnhead <- NULL
        for (c in 2:(ncol(table))) {
          columnhead <- paste0(columnhead, "\" 0 \"",
                               colnames(table)[c])
        }
        tcltk::tkconfigure(l8.analysis.file.table, columns = paste0("1 \"",
                                                                    colnames(table)[1], columnhead, "\""))
        
        ## make columns editable and wrap
        for (c in 0:(ncol(table) - 1)) {
          tcltk::tcl(l8.analysis.file.table, "columnconfigure",
                     c, editable = 1)
          tcltk::tcl(l8.analysis.file.table, "columnconfigure",
                     c, wrap = tcltk::tclvalue(wrap.var))
        }
        
        ## fill table
        for (r in 1:nrow(table)) {
          rowlist <- NULL
          for (c in 1:ncol(table)) {
            rowlist <- c(rowlist, as.character(table[r,
                                                     c]))
          }
          tcltk::tkinsert(l8.analysis.file.table, "end",
                          rowlist)
        }
        
        tcltk::tclvalue(columnnumber.var) <- ncol(table)
        
      } else {
        ## fill the header
        tcltk::tkconfigure(l8.analysis.file.table, columns = paste0("1 \"",
                                                                    "txtcontent", "\""))
        ## make columns editable and wrap
        tcltk::tcl(l8.analysis.file.table, "columnconfigure",
                   0, editable = 1)
        tcltk::tcl(l8.analysis.file.table, "columnconfigure",
                   0, wrap = tcltk::tclvalue(wrap.var))
        
        ## fill table
        for (r in 1:length(table)) {
          tcltk::tkinsert(l8.analysis.file.table, "end",
                          as.character(c(table[r],"")))
        }
        tcltk::tclvalue(columnnumber.var) <- 1
      }
      
      ## add mark option to combobox
      if (PDE.globals$mark.list[1] == "") {
        if (!tcltk::tclvalue(pdffolder.var) == "") {
          PDE.globals$mark.list <- c("Mark analysis file only",
                                     "Mark PDF file only", "Mark analysis file & PDF")
        } else {
          PDE.globals$mark.list <- c("Mark analysis file only")
        }
        tcltk::tkconfigure(l9.mark.cb, values = tcltk::as.tclObj(PDE.globals$mark.list,
                                                                 drop = FALSE))
        tcltk::tclvalue(mark.var) = PDE.globals$mark.list[1]
        tcltk::tkconfigure(l9.flagfile.but, state = "normal")
        tcltk::tkconfigure(l9.xmarkfile.but, state = "normal")
        tcltk::tkconfigure(l9.unmarkfile.but, state = "normal")
      }
      
    }
    
    ## combobox list -------------------------------------------------------
    
    l5.jumpto.cb <- tcltk2::tk2combobox(l5, textvariable = jumpto.var,
                                        values = PDE.globals$jumpto.list, width = 30, state = "readonly")
    
    ## onchange combobox
    tcltk::tkbind(l5.jumpto.cb, "<<ComboboxSelected>>",
                  function() {
                    numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
                    pos <- as.numeric(numb)
                    loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
                    res <- try(PDE.globals$tables.masterlist[["values_table"]][[pos]], silent = TRUE)
                    if (!inherits(res,"try-error") && !is.null(res)) {
                      tab <- PDE.globals$tables.masterlist[["values_table"]][[pos]]
                      loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
                      ## highlight search words if highlighting is on
                      hightlighted.tab <- markwords(tab,
                                                    loc, pos)
                      ## update display of sentences
                      display.values_table <- adjust.sent.display(hightlighted.tab)
                      
                      fill.table(table = display.values_table,
                                 analysis.file_location = loc)
                      ## match PDF files if pdffolder was chosen
                      if (!tcltk::tclvalue(pdffolder.var) == "") {
                        load.pdffolder(tcltk::tclvalue(pdffolder.var))
                      }
                    } else {
                      filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
                      searchfilename <- gsub("+", "[+]", filename,
                                             fixed = TRUE)
                      ## find the file in the folder
                      fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                                 pattern = searchfilename, full.names = TRUE,
                                                 recursive = TRUE)[1]
                      fullfilename <- gsub("\\\\", "/", fullfilename)
                      open.analysis.file(analysis.file_location = fullfilename)
                    }
                  })
    
    l9.mark.cb <- tcltk2::tk2combobox(l9, textvariable = mark.var,
                                      values = PDE.globals$mark.list, width = 20, state = "readonly")
    
    
    fill.folder.infos <- function(table, analysis.file_location) {
      filename <- as.character(basename(analysis.file_location))
      
      ## add file to jumpto list if it is not already in there
      if (!(any(grepl(filename, PDE.globals$jumpto.list, fixed = TRUE)))) {
        if (PDE.globals$jumpto.list[1] == "") {
          PDE.globals$jumpto.list <- paste((length(PDE.globals$jumpto.list)),
                                           filename, sep = "-")
          analysis.file_location <- gsub("\\\\", "/", analysis.file_location)
          pos <- 1
          PDE.globals$tables.masterlist[["analysis.file_location"]][pos] <- list(analysis.file_location)
        } else {
          analysis.file_location <- gsub("\\\\", "/", analysis.file_location)
          pos <- length(PDE.globals$jumpto.list) + 1
          PDE.globals$tables.masterlist[["analysis.file_location"]][pos] <- list(analysis.file_location)
          PDE.globals$jumpto.list <- c(PDE.globals$jumpto.list, paste(pos, filename, sep = "-"))
        }
        if (length(PDE.globals$jumpto.list) > 1) {
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
        } else {
          tcltk::tkconfigure(l5.jumpto.cb, values = tcltk::as.tclObj(PDE.globals$jumpto.list,
                                                                     drop = FALSE))
        }
      } else {
        pos <- grep(filename, PDE.globals$jumpto.list, fixed = TRUE)[1]
      }
      
      if (!is.null(dim(table))) {
        ## add value_table to masterlist of tables if not
        ## already in there
        res <- try(PDE.globals$tables.masterlist[["values_table"]][[pos]], silent = TRUE)
        if (inherits(res,"try-error")) {
          PDE.globals$tables.masterlist[["values_table"]][pos] <- list(table)
          PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["tsvfile"]] <- list()
          PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["table"]] <- list()
          # PDE.globals$tables.masterlist[["woabbrev_values_table"]][pos] <- list()
          PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["tsvfile"]] <- list()
          PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["table"]] <- list()
        } else if (is.null(PDE.globals$tables.masterlist[["values_table"]][[pos]])) {
          PDE.globals$tables.masterlist[["values_table"]][pos] <- list(table)
          PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["tsvfile"]] <- list()
          PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["table"]] <- list()
          # PDE.globals$tables.masterlist[["woabbrev_values_table"]][pos] <- list()
          PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["tsvfile"]] <- list()
          PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["table"]] <- list()
        }
        ## make folder structure
      } else {
        # PDE.globals$tables.masterlist[["values_table"]][pos] <- list()
        PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["tsvfile"]] <- list()
        PDE.globals$tables.masterlist[["markedtables"]][pos][[1]][["table"]] <- list()
        # PDE.globals$tables.masterlist[["woabbrev_values_table"]][pos] <- list()
        PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["tsvfile"]] <- list()
        PDE.globals$tables.masterlist[["woabbrev_markedtables"]][pos][[1]][["table"]] <- list()
      }
      
    }
    
    searchfolder <- function(analysis.folder_location) {
      analysis.tsv.files <- list.files(analysis.folder_location,
                                       pattern = ".*(txt\\+-).*\\.tsv", full.names = TRUE,
                                       recursive = TRUE)
      analysis.csv.files <- list.files(analysis.folder_location,
                                       pattern = ".*(txt\\+-).*\\.csv$", full.names = TRUE,
                                       recursive = TRUE)
      analysis.files <- c(analysis.tsv.files, analysis.csv.files)
      return(analysis.files)
    }
    
    smaller.font <- function() {
      newfontsize <- as.integer(tcltk::tkfont.actual("TkDefaultFont",
                                                     "-size")) - 1
      tcltk::tkfont.configure("TkDefaultFont", size = newfontsize)
      tcltk::tclvalue(fontsize.var) = newfontsize
    }
    l6.smaller.font.but <- tcltk2::tk2button(l6, text = "-",
                                             command = smaller.font, width = 2)
    
    default.font <- function() {
      tcltk::tkfont.configure("TkDefaultFont", size = tcltk::tclvalue(defaultfontsize.var))
      tcltk::tclvalue(fontsize.var) <- tcltk::tclvalue(defaultfontsize.var)
    }
    l6.default.font.but <- tcltk2::tk2button(l6, text = "o",
                                             command = default.font, width = 2)
    
    larger.font <- function() {
      newfontsize <- as.integer(tcltk::tkfont.actual("TkDefaultFont",
                                                     "-size")) + 1
      tcltk::tkfont.configure("TkDefaultFont", size = newfontsize)
      tcltk::tclvalue(fontsize.var) <- newfontsize
    }
    l6.larger.font.but <- tcltk2::tk2button(l6, text = "+",
                                            command = larger.font, width = 2)
    
    adjust.sent.display <- function(input_table){
      output_table <- input_table
      if (as.numeric(tcltk::tclvalue(sent.count.indicator.var)) == 0){
        return(input_table)
      } else {
        for (r in 1:nrow(input_table)){
          ## evaluate sentence break positions
          search.word.pos <- as.numeric(gsub("_.*","",input_table[r,"search.word.loc_total"]))
          sent.number <- as.numeric(gsub(".*_","",input_table[r,"search.word.loc_total"]))
          sent.count.indicator <- as.numeric(tcltk::tclvalue(sent.count.indicator.var))
          period.pos <- c(1,gregexpr("\\. [0-9A-Z]",input_table[r,"txtcontent"])[[1]],
                          nchar(input_table[r,"txtcontent"]))
          
          ##adjust front of the sentences
          ## if there are enough sentences in the front to be removed
          ## --> (search.word.pos + sent.count.indicator) > 0
          if ((search.word.pos + sent.count.indicator) > 0){
            start.new.txt <- period.pos[search.word.pos + sent.count.indicator + 1]
            
            ## if all sentences in the front are to be removed
            ## --> (search.word.pos + sent.count.indicator) <= 0
          } else if ((search.word.pos + sent.count.indicator) <= 0){
            start.new.txt <- period.pos[search.word.pos]
          }
          
          ##adjust end of the sentences
          ## if there are enough sentences in the end to be removed
          ## --> (search.word.pos + sent.count.indicator) > 0
          if ((sent.number - search.word.pos + sent.count.indicator) > 0){
            end.new.txt <- period.pos[sent.number + sent.count.indicator + 1]
            
            ## if all sentences in the end are to be removed
            ## --> (search.word.pos + sent.count.indicator) <= 0
          } else if ((sent.number - search.word.pos + sent.count.indicator) <= 0){
            end.new.txt <- period.pos[search.word.pos + 1]
          }
          
          output_table[r,"txtcontent"] <- substr(input_table[r,"txtcontent"],(start.new.txt+2),end.new.txt)
        } ## end for (r in 1:nrow(input_table)){
        return(output_table)
      } ## end if removal counter == 0
    }
    
    ## radiobuttons -----------------------------------------------------
    changewrapping <- function() {
      if (!tcltk::tclvalue(columnnumber.var) == "0") {
        for (c in 0:(as.integer(tcltk::tclvalue(columnnumber.var)) - 1)) {
          tcltk::tcl(l8.analysis.file.table, "columnconfigure",
                     c, wrap = tcltk::tclvalue(wrap.var))
        }
      }
    }
    
    l6.wrap.rb <- tcltk::tkradiobutton(l6, variable = wrap.var,
                                       value = "1", background = "#05F2C7", command = changewrapping)
    l6.no.wrap.rb <- tcltk::tkradiobutton(l6, variable = wrap.var,
                                          value = "0", background = "#05F2C7", command = changewrapping)
    
    ## checkbuttons -----------------------------------------------------
    change.txtcontent.only <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## reload the table
      if (!(tcltk::tclvalue(jumpto.var) == "")) {
        filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        open.analysis.file(analysis.file_location = fullfilename)
      } else {
        markwords("", "", 0)
      }
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    
    l7.txtcontent.only.cbtn <- tcltk::tkcheckbutton(l7, variable = txtcontent.only.var, 
                                                    text = "show txtcontent only",
                                                    state = "normal", background = "#05F2C7", 
                                                    command = change.txtcontent.only)
    
    change.collapse.abbrev <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## reload the table
      if (!(tcltk::tclvalue(jumpto.var) == "")) {
        filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        open.analysis.file(analysis.file_location = fullfilename)
      } else {
        markwords("", "", 0)
      }
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    
    l7.collapse.abbrev.cb <- tcltk::tkcheckbutton(l7, variable = collapse.abbrev.var, 
                                                  text = "show original text (abbrev. collapsed)",
                                                  state = "normal", background = "#05F2C7", 
                                                  command = change.collapse.abbrev)
    
    ## buttons 2 -----------------------------------------------------------------
    
    save.memory <- function() {
      todays.date <- format(Sys.Date(), "%Y-%m-%d")
      PDE_parameters_filename <- paste0(todays.date,
                                        "_", "memory.RData")
      savefilelocation <- tcltk::tclvalue(tcltk::tkgetSaveFile(initialfile = PDE_parameters_filename,
                                                               defaultextension = ".RData", 
                                                               filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
      if (!savefilelocation == ""){
        jumpto.saved <-  tcltk::tclvalue(jumpto.var)
        pdffolder.saved <- tcltk::tclvalue(pdffolder.var)
        tsvfile.saved <- tcltk::tclvalue(tsvfile.var)
        analysis.folder_location.saved <- tcltk::tclvalue(analysis.folder_location.var)
        PDE.globals.jumpto.list <- PDE.globals$jumpto.list
        PDE.globals.mark.list <- PDE.globals$mark.list
        PDE.globals.tables.masterlist <- PDE.globals$tables.masterlist
        
        save("PDE.globals.jumpto.list", "PDE.globals.mark.list", "PDE.globals.tables.masterlist",
             "jumpto.saved", "pdffolder.saved", "tsvfile.saved", 
             "analysis.folder_location.saved", file = paste(savefilelocation,
                                                            collapse = " "))
      }
    }
    
    load.memory <- function() {
      ## reset loaded variables
      PDE.globals.jumpto.list <-NULL
      PDE.globals.mark.list <- NULL
      PDE.globals.tables.masterlist <- NULL
      tsvfile.saved <- NULL
      analysis.folder_location.saved <- NULL
      jumpto.saved <- NULL
      pdffolder.saved <- NULL
      
      todays.date <- format(Sys.Date(), "%Y-%m-%d")
      PDE_parameters_filename <- paste0(todays.date,
                                        "_", "memory.RData")
      openfilelocation <- tcltk::tclvalue(tcltk::tkgetOpenFile(initialfile = PDE_parameters_filename,
                                                               defaultextension = ".RData", 
                                                               filetypes = "{ {RData Files} {.RData} } { {All Files} * }"))
      if (!openfilelocation == ""){
        load(file = paste(openfilelocation, collapse = " "))
        
        ## test if PDE.globals was loaded
        if (is.null(PDE.globals.tables.masterlist)) {
          PDE.globals$jumpto.list <- PDE.globals.jumpto.list
          ## update marklist
          PDE.globals$mark.list <- PDE.globals.mark.list
          if (PDE.globals$mark.list != ""){
            tcltk::tkconfigure(l9.mark.cb, values = tcltk::as.tclObj(PDE.globals$mark.list,
                                                                     drop = FALSE))
            tcltk::tclvalue(mark.var) <- PDE.globals$mark.list[1]
            tcltk::tkconfigure(l9.flagfile.but, state = "normal")
            tcltk::tkconfigure(l9.xmarkfile.but, state = "normal")
            tcltk::tkconfigure(l9.unmarkfile.but, state = "normal")
          }
          ##
          PDE.globals$tables.masterlist <- PDE.globals.tables.masterlist
          
          ## fill in tsv file
          tcltk::tclvalue(tsvfile.var) <- tsvfile.saved
          load.loadtsv(loadfile = "not")
          
          ## set the analysis.folder_location
          tcltk::tclvalue(analysis.folder_location.var) <- analysis.folder_location.saved
          
          ## fill in jumpto.list
          tcltk::tclvalue(jumpto.var) <- jumpto.saved
          numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
          pos <- as.numeric(numb)
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
          if (is.na(pos)) pos <- 1
          ## load tables
          tab <- PDE.globals$tables.masterlist[["values_table"]][[pos]]
          loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
          ## highlight search words if highlighting is on
          hightlighted.tab <- markwords(tab,
                                        loc, pos)
          ## update display of sentences
          display.values_table <- adjust.sent.display(hightlighted.tab)
          
          fill.table(table = display.values_table,
                     analysis.file_location = loc)
          tcltk::tclvalue(pdffolder.var) <- pdffolder.saved
          ## match PDF files if pdffolder was chosen
          if (!tcltk::tclvalue(pdffolder.var) == "") {
            load.pdffolder(tcltk::tclvalue(pdffolder.var))
          }
        }
      }
    }
    
    l1.save.memory.but <- tcltk2::tk2button(l1, text = "Save memory to file",
                                            command = save.memory)
    l1.load.memory.but <- tcltk2::tk2button(l1, text = "Load memory from file",
                                            command = load.memory)
    
    markwords <- function(table, analysis.file_location, pos) {
      analysis.file_location <- gsub("\\\\", "/",
                                     analysis.file_location)
      ## mark only if table is present, mark is ON, and
      ## search words are present
      if (!is.null(dim(table)) &&
          tcltk::tclvalue(tsv.onoff.var) == "on" &&
          !(length(tcltk::tclvalue(search.words.var)) == 0)) {
        ## if abbreviations are not collapsed (regular)
        if (tcltk::tclvalue(collapse.abbrev.var) == "0"){
          ## check position of TSV file (integer(0) when not found)
          tsvpos <- grep(tcltk::tclvalue(tsvfile.var), 
                         PDE.globals$tables.masterlist[["markedtables"]][[pos]][["tsvfile"]],
                         fixed = TRUE)
          ## if TSV file is not found --> search for it
          if (length(tsvpos) == 0) {
            ## get a list of search words
            if (length(grep(";", tcltk::tclvalue(search.words.var))) > 0) {
              search.for <- strsplit(tcltk::tclvalue(search.words.var),
                                     ";")[[1]]
            } else {
              search.for <- tcltk::tclvalue(search.words.var)
            }
            newtable <- table
            ## for each search word
            for (w in 1:length(search.for)) {
              ## first separate by column to prevent faults in
              ## gsub then lapply
              for (c in 1:ncol(newtable)) {
                newtable[, c] <- list(lapply(newtable[, c], function(x) {
                  gsub(search.for[w],
                       paste0("\U2588\U25B6", regmatches(x, 
                                                         regexpr(search.for[w], x, 
                                                                 ignore.case = as.logical(tcltk::tclvalue(ignore.case.sw.var)))),
                              "\U25C0\U2588"), x, ignore.case = as.logical(tcltk::tclvalue(ignore.case.sw.var)))
                }))
              }
            }
            tsvnum <- length(PDE.globals$tables.masterlist[["markedtables"]][[pos]][["tsvfile"]])
            PDE.globals$tables.masterlist[["markedtables"]][[pos]][["tsvfile"]][(tsvnum + 1)] <- list(tcltk::tclvalue(tsvfile.var))
            PDE.globals$tables.masterlist[["markedtables"]][[pos]][["table"]][(tsvnum + 1)] <- list(newtable)
          } else {
            ## if marked table was found
            newtable <- PDE.globals$tables.masterlist[["markedtables"]][[pos]][["table"]][[tsvpos]]
          }
          return(newtable)
          ## if mark is on and abbreviations should be collapsed as well
        } else {
          ## check position of TSV file (integer(0) when not found)
          tsvpos <- grep(tcltk::tclvalue(tsvfile.var), 
                         PDE.globals$tables.masterlist[["woabbrev_markedtables"]][[pos]][["tsvfile"]],
                         fixed = TRUE)
          ## if TSV file is not found
          if (length(tsvpos) == 0) {
            ## get a list of search words
            if (length(grep(";", tcltk::tclvalue(search.words.var))) > 0) {
              search.for <- strsplit(tcltk::tclvalue(search.words.var),
                                     ";")[[1]]
            } else {
              search.for <- tcltk::tclvalue(search.words.var)
            }
            newtable <- table
            ## for each search word
            for (w in 1:length(search.for)) {
              ## first separate by column to prevent faults in
              ## gsub then lapply
              for (c in 1:ncol(newtable)) {
                newtable[, c] <- list(lapply(newtable[, c], function(x) {
                  gsub(search.for[w],
                       paste0("\U2588\U25B6", regmatches(x, 
                                                         regexpr(search.for[w], x, 
                                                                 ignore.case = as.logical(tcltk::tclvalue(ignore.case.sw.var)))),
                              "\U25C0\U2588"), x, ignore.case = as.logical(tcltk::tclvalue(ignore.case.sw.var)))
                }))
                ## replace abbreviations
                newtable[, c] <- list(lapply(newtable[, c], function(x) {
                  sub(" \\(","",paste0(gsub("^.*\\$\\*","",
                                            paste0(" (",strsplit(as.character(x)," \\(")[[1]])),collapse = ""))
                }))
              }
            }
            ## add the newtable to the PDE.globals$tables.masterlist
            ## check position of TSV file
            tsvnum <- length(PDE.globals$tables.masterlist[["woabbrev_markedtables"]][[pos]][["tsvfile"]])
            PDE.globals$tables.masterlist[["woabbrev_markedtables"]][[pos]][["tsvfile"]][(tsvnum + 1)] <- list(tcltk::tclvalue(tsvfile.var))
            PDE.globals$tables.masterlist[["woabbrev_markedtables"]][[pos]][["table"]][(tsvnum + 1)] <- list(newtable)
          } else {
            ## if marked table was found
            newtable <- PDE.globals$tables.masterlist[["woabbrev_markedtables"]][[pos]][["table"]][[tsvpos]]
          }
          return(newtable)
        }
        ## if mark is OFF but abbreviations should be collapsed
      } else if(!is.null(dim(table)) &&
                tcltk::tclvalue(tsv.onoff.var) == "off" &&
                !(length(tcltk::tclvalue(search.words.var)) == 0)) {
        ## if abbreviations should be collapsed
        if (tcltk::tclvalue(collapse.abbrev.var) == "1"){
          ## test if woabbrev_values_table already exists
          res <- try(PDE.globals$tables.masterlist[["woabbrev_values_table"]][[pos]], silent = TRUE)
          if (!inherits(res,"try-error") && !is.null(res)) {
            ## check position of TSV file (if table was skipped it becomes NA)
            newtable <- PDE.globals$tables.masterlist[["woabbrev_values_table"]][[pos]]
            if (is.null(newtable)) newtable <- NA
          } else {
            newtable <- NA
          }
          ## if newtable is empty (woabbrev_values_table does not exist)
          if (is.na(newtable)) {
            ## overwrite newtable with regular values table
            newtable <- table
            ## first separate by column to prevent faults in
            ## gsub then lapply
            for (c in 1:ncol(newtable)) {
              ## replace abbreviations
              newtable[, c] <- list(lapply(newtable[, c], function(x) {
                sub(" \\(","",paste0(gsub("^.*\\$\\*","",paste0(" (",strsplit(as.character(x)," \\(")[[1]])),collapse = ""))
              }))
            }
            PDE.globals$tables.masterlist[["woabbrev_values_table"]][pos] <- list(newtable)
          } else {
            ## return newtable
          }
          return(newtable)
          ## if neither mark was on nor abbreviations should be collapsed
        } else {
          return(table)
        }
        ## if table was not intact
      } else {
        return(table)
      }
    }
    
    
    load.loadtsv <- function(loadfile) {
      if (loadfile == "%loadfile") {
        tsvfile <- tcltk::tk_choose.files(default = tcltk::tclvalue(tsvfile.var),
                                          caption = "Choose the TSV file",
                                          multi = FALSE)
        if (length(tsvfile) > 0) {
          tcltk::tclvalue(tsvfile.var) <- tsvfile
        } 
      }
      
      ## enable extract table if TSV file is chosen
      if (!(tcltk::tclvalue(tsvfile.var) == "") && file.exists(tcltk::tclvalue(fullpdfname.var))) {
        tcltk::tkconfigure(l4.extract.table.but, state = "normal")
      }
      
      if (!(tcltk::tclvalue(tsvfile.var) == "")) {
        tsv_table <- utils::read.table(tcltk::tclvalue(tsvfile.var),
                                       sep = "\t", header = TRUE, quote = "\"",
                                       stringsAsFactors = FALSE)
        ## search word
        search.wds <- as.character(tsv_table[(grep("search.words",
                                                   tsv_table[, "variable"])), "value"])
        ## search word case sensitive
        ic.sw <- as.character(tsv_table[(grep("ignore.case.sw",
                                              tsv_table[, "variable"])), "value"])
        ## look if search words were found
        if (!(length(search.wds) == 0)) {
          tcltk::tclvalue(search.words.var) = search.wds
          tcltk::tkconfigure(l2.tsv.onoff.but, state = "normal")
          tcltk::tkconfigure(l2.tsv.loadall.but, state = "normal")
        } else {
          tcltk::tkmessageBox(title = "Error", type = "ok",
                              icon = "error", message = "Check for correct TSV file. No search words were found in file.")
          tcltk::tkconfigure(l2.tsv.onoff.but, state = "disabled")
          tcltk::tkconfigure(l2.tsv.loadall.but, state = "disabled")
        }
        ## look if search word ignore case
        if (!(length(ic.sw) == 0)) {
          tcltk::tclvalue(ignore.case.sw.var) <- ic.sw
        } else {
          tcltk::tclvalue(ignore.case.sw.var) <- "FALSE"
        }
        ## reload table (marking is in open.analysis.file
        ## script)
        if (!(tcltk::tclvalue(jumpto.var) == "")) {
          filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
          searchfilename <- gsub("+", "[+]", filename,
                                 fixed = TRUE)
          fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                     pattern = searchfilename, full.names = TRUE,
                                     recursive = TRUE)[1]
          open.analysis.file(analysis.file_location = fullfilename)
        } else {
          markwords("", "",0)
        }
        ##change from off to on for marking
        analysis.folder_location <- tcltk::tclvalue(analysis.folder_location.var)
        if (!is.na(analysis.folder_location) &&
            analysis.folder_location != "" &&
            tcltk::tclvalue(tsv.onoff.var) == "off" &&
            !(length(search.wds) == 0) &&
            !(loadfile == "not")){
          tsv.onoff()
        }
      } ## end if TSV file was chosen
    }
    
    tsv.onoff <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## reverse button
      if (tcltk::tclvalue(tsv.onoff.var) == "on") {
        tcltk::tclvalue(tsv.onoff.var) <- "off"
      } else {
        tcltk::tclvalue(tsv.onoff.var) <- "on"
      }
      tcltk::tkconfigure(l2.tsv.onoff.but, text = tcltk::tclvalue(tsv.onoff.var))
      ## start marking
      load.loadtsv(loadfile = "not")
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    
    tsv.loadall <- function() {
      analysis.files <- PDE.globals$tables.masterlist[["analysis.file_location"]]
      if (length(analysis.files) > 0) {
        analysis.files <- gsub("\\\\", "/", analysis.files)
        tcltk::tclvalue(tsv.onoff.var) <- "on"
        for (pos in 1:length(analysis.files)) {
          ## update progressbar
          tcltk::tclvalue(progress.var) <- as.character(round(((pos -
                                                                  1)/length(analysis.files) * 100),
                                                              digits = 0))
          tcltk::tkconfigure(l1.progress.bar.pb, value = as.numeric(tcltk::tclvalue(progress.var)))
          tcltk::tkconfigure(l1.progress.label, text = paste0(tcltk::tclvalue(progress.var),
                                                              "%"))
          tcltk::tcl("update")
          
          ## load values table
          res <- try(PDE.globals$tables.masterlist[["values_table"]][[pos]], silent = TRUE)
          if (!inherits(res,"try-error") && !is.null(res)) {
            values_table <- PDE.globals$tables.masterlist[["values_table"]][[pos]]
            if (is.null(values_table)) {
              if (grepl("\\.csv$", analysis.files[pos])) {
                values_table <- read.problemtable(analysis.files[pos],
                                                  header = TRUE, quote = "\"",
                                                  sep = ",")
              } else if (grepl("\\.tsv$", analysis.files[pos])) {
                values_table <- read.problemtable(analysis.files[pos],
                                                  sep = "\t", header = TRUE, quote = "\"")
              }
            }
          } else {
            if (grepl("\\.csv$", analysis.files[pos])) {
              values_table <- read.problemtable(analysis.files[pos],
                                                header = TRUE, quote = "\"",
                                                sep = ",")
            } else if (grepl("\\.tsv$", analysis.files[pos])) {
              values_table <- read.problemtable(analysis.files[pos],
                                                sep = "\t", header = TRUE, quote = "\"")
            }
          }
          ## fill infos for all
          fill.folder.infos(values_table, analysis.files[pos])
          highlighted.values_table <- markwords(values_table,
                                                analysis.files[pos], pos)
        }
        ## update progressbar
        tcltk::tkconfigure(l1.progress.bar.pb, value = 100)
        tcltk::tkconfigure(l1.progress.label, text = "complete")
        tcltk::tkfocus(ttreader)
        tcltk::tkraise(ttreader)
        tcltk::tcl("update")
        tcltk::tclvalue(tsv.onoff.var) <- "off"
      }
    }
    
    l2.loadtsv.but <- tcltk2::tk2button(l2, text = "Load TSV file (for search word highlighting)",
                                        command = load.loadtsv)
    
    l2.tsv.loadall.but <- tcltk2::tk2button(l2, text = "load all",
                                            command = tsv.loadall, state = "disabled",
                                            width = 8)
    
    l2.tsv.onoff.but <- tcltk2::tk2button(l2, text = tcltk::tclvalue(tsv.onoff.var),
                                          command = tsv.onoff, state = "disabled", width = 3)
    
    resetformnopdf <- function() {
      return("")
      tcltk::tkconfigure(l5.jumpto.cb, values = "")
      tcltk::tclvalue(filename.var) <- ""
      PDE.globals$jumpto.list <- ""
      tcltk::tclvalue(jumpto.var) <- ""
      l6.analysis.file.table <- NULL
      PDE.globals$tables.masterlist <- NULL
      tcltk::tclvalue(analysis.folder_location.var) <- ""
    }
    
    resetform <- function() {
      tcltk::tclvalue(filename.var) <- ""
      tcltk::tkwm.title(ttreader, "PDE reader")
      tcltk::tclvalue(pdffolder.var) <- ""
      tcltk::tclvalue(fullpdfname.var) <- ""
      tcltk::tclvalue(pdfname.var) <- ""
      tcltk::tclvalue(pdfnumber.var) <- ""
      tcltk::tclvalue(tsvfile.var) <- ""
      tcltk::tkconfigure(l4.openpdf.but, state = "disabled")
      tcltk::tkconfigure(l4.extract.table.but, state = "disabled")
      tcltk::tkconfigure(l2.tsv.onoff.but, state = "disabled")
      tcltk::tkconfigure(l2.tsv.loadall.but, state = "disabled")
      PDE.globals$jumpto.list <- ""
      tcltk::tclvalue(jumpto.var) <- ""
      tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
      PDE.globals$mark.list <- ""
      tcltk::tkconfigure(l9.mark.cb, values = PDE.globals$mark.list)
      tcltk::tkconfigure(l9.flagfile.but, state = "disabled")
      tcltk::tkconfigure(l9.xmarkfile.but, state = "disabled")
      tcltk::tkconfigure(l9.unmarkfile.but, state = "disabled")
      tcltk::tclvalue(mark.var) <- ""
      l6.analysis.file.table <- NULL
      tcltk::tclvalue(columnnumber.var) <- "0"
      PDE.globals$tables.masterlist <- NULL
      tcltk::tclvalue(analysis.folder_location.var) <- ""
      tcltk::tclvalue(wrap.var) <- "1"
      tcltk::tclvalue(progress.var) <- "0"
      tcltk::tkconfigure(l8.analysis.file.table, columns = "")
      tcltk::tkfont.configure("TkDefaultFont", size = tcltk::tclvalue(defaultfontsize.var))
      tcltk::tclvalue(hotkey.mode.var) <- "standard"
    }
    l1.reset.but <- tcltk2::tk2button(l1, text = "Reset form",
                                      command = resetform)
    
    
    ## line2
    load.pdffolder <- function(pdffolder) {
      if (length(pdffolder) == 0 || pdffolder == "%pdffolder") {
        tcltk::tclvalue(pdffolder.var) <- tcltk::tk_choose.dir(default = tcltk::tclvalue(pdffolder.var),
                                                               caption = "Choose folder with PDF files")
      }
      ## search pdf folder for matching PDF files
      if (!tcltk::tclvalue(jumpto.var) == "") {
        tsv.filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        txtstart <- regexpr("_txt+-", tsv.filename,
                            fixed = TRUE)
        if (substr(tsv.filename,1,2) == "!_" ||
            substr(tsv.filename,1,2) == "x_"){
          pdfnamestart <- 3
        } else {
          pdfnamestart <- 1
        }
        pdfname <- paste0(substr(tsv.filename,
                                 pdfnamestart, (txtstart - 1)),
                          ".pdf")
        
        ## find pdf
        pdfs <- list.files(tcltk::tclvalue(pdffolder.var),
                           pattern = "*.pdf", full.names = TRUE,
                           recursive = TRUE)
        if (any(grepl(pdfname, pdfs, fixed = TRUE))) {
          tcltk::tclvalue(pdfname.var) <- basename(grep(pdfname,
                                                        pdfs, fixed = TRUE, value = TRUE)[1])
          tcltk::tclvalue(fullpdfname.var) <- grep(pdfname,
                                                   pdfs, fixed = TRUE, value = TRUE)[1]
          tcltk::tkconfigure(l4.openpdf.but, state = "normal")
          ## enable extract table if TSV file is chosen
          if (!(tcltk::tclvalue(tsvfile.var) == "")) {
            tcltk::tkconfigure(l4.extract.table.but,
                               state = "normal")
          }
          if (!tcltk::tclvalue(jumpto.var) == "") {
            if (!length(PDE.globals$mark.list) == 3) {
              PDE.globals$mark.list <- c("Mark analysis file only",
                                         "Mark PDF file only", "Mark analysis file & PDF")
              tcltk::tkconfigure(l9.mark.cb, values = tcltk::as.tclObj(PDE.globals$mark.list,
                                                                       drop = FALSE))
              tcltk::tclvalue(mark.var) <- PDE.globals$mark.list[1]
            }
          }
        } else {
          tcltk::tkconfigure(l4.extract.table.but,
                             state = "disabled")
          tcltk::tclvalue(pdfname.var) <- ""
          tcltk::tclvalue(fullpdfname.var) <- ""
        }
      }
    }
    l3.loadpdffolder.but <- tcltk2::tk2button(l3, text = "Load PDF folder",
                                              command = load.pdffolder)
    
    ## line4
    openpdf <- function() {
      if (!tcltk::tclvalue(pdfname.var) == "")
        system(paste0("open ", "\"", tcltk::tclvalue(fullpdfname.var),
                      "\""))
    }
    l4.openpdf.but <- tcltk2::tk2button(l4, text = "Open current PDF",
                                        command = openpdf, state = "disabled", underline = "0")
    
    extract_table <- function() {
      if (!(tcltk::tclvalue(tsvfile.var) == "") && file.exists(tcltk::tclvalue(fullpdfname.var))) {
        tsv_table <- utils::read.table(tcltk::tclvalue(tsvfile.var),
                                       sep = "\t", header = TRUE, quote = "\"",
                                       stringsAsFactors = FALSE)
        
        ## set required variables
        pdf <- tcltk::tclvalue(fullpdfname.var)
        whattoextr <- "tab"
        ## choose folder
        outputdir.created <- FALSE
        outputdir <- paste0(tcltk::tclvalue(analysis.folder_location.var),
                            "/extracted_tables")
        if (!exists(outputdir)){
          dir.create(outputdir, showWarnings = FALSE)
          outputdir.created <- TRUE
        }
        outputfolder <- tcltk::tk_choose.dir(default = outputdir,
                                             caption = "Choose the outputfolder for the extracted tables.")
        if (outputdir != outputfolder && outputdir.created == TRUE) {
          unlink(outputdir, recursive = TRUE)
        }
        
        ##continue when correct output folder was chosen
        if (!is.na(outputfolder) && outputfolder != "") {
          if (!exists(outputfolder)){
            dir.create(outputfolder,showWarnings = FALSE)
          }
          context <- 0
          dev_x <- as.numeric(tsv_table[(grep("dev_x",
                                              tsv_table[, "variable"])), "value"])
          dev_y <- as.numeric(tsv_table[(grep("dev_y",
                                              tsv_table[, "variable"])), "value"])
          filter.for <- ""
          ic.fw <- TRUE
          filter.word.times <- 0
          table.heading.for <- ""
          ic.th <- TRUE
          search.for <- ""
          search.word.categories <- ""
          ic.sw <- TRUE
          eval.abbrevs <- FALSE
          wtl <- FALSE
          write.tab.doc.file <- TRUE
          write.txt.doc.file <- FALSE
          exp.nondetc.tabs <- TRUE
          cpy_mv <- "nocpymv"
          out.table.format <- as.character(tsv_table[(grep("out.table.format",
                                                           tsv_table[, "variable"])), "value"])
          delete <- TRUE
          tablelines <- .PDE_extr_data_from_pdf(pdf = pdf,
                                                whattoextr = whattoextr, 
                                                out = outputfolder, context = context,
                                                dev_x = dev_x, dev_y = dev_y, filter.words = filter.for,
                                                ignore.case.fw = ic.fw, filter.word.times = filter.word.times,
                                                table.heading.words = table.heading.for,
                                                ignore.case.th = ic.th, search.words = search.for,
                                                search.word.categories = search.word.categories,
                                                ignore.case.sw = ic.sw, eval.abbrevs = eval.abbrevs,
                                                write.table.locations = wtl,
                                                write.tab.doc.file = write.tab.doc.file,
                                                write.txt.doc.file = write.txt.doc.file,
                                                exp.nondetc.tabs = exp.nondetc.tabs,
                                                out.table.format = out.table.format,
                                                delete = delete, cpy_mv = cpy_mv, verbose = verbose)
          
        } else {
          out_msg <- c(out_msg, "Please choose a correct output folder.")
          if (verbose) cat(utils::tail(out_msg,1), sep="\n")
        }
      } else {
        out_msg <- c(out_msg, "Please choose a TSV file and the PDF folder.")
        if (verbose) cat(utils::tail(out_msg,1), sep="\n")
      }
    }
    l4.extract.table.but <- tcltk2::tk2button(l4, text = "Extract tables",
                                              command = extract_table, state = "disabled")
    
    
    ## line 1 open analysis file
    open.analysis.file <- function(analysis.file_location = NULL) {
      values_table <- NULL
      if (length(analysis.file_location) == 0 ||
          grepl("%", analysis.file_location)) {
        analysis.file_location <- paste(tcltk::tk_choose.files(default = tcltk::tclvalue(analysis.folder_location.var),
                                                               caption = "Choose the analysis file",
                                                               multi = FALSE), collapse = " ")
      }
      filename <- as.character(basename(analysis.file_location))
      ## if no file was selected do nothing
      if (length(analysis.file_location) == 0) {
        ## if correct file (with txt+-) was chosen
      } else if ((analysis.file_location != "") && (grepl("txt+-",
                                                          filename, fixed = TRUE))) {
        
        ## if a new folder is loaded, reset form
        if (!dirname(analysis.file_location) ==
            tcltk::tclvalue(analysis.folder_location.var)) {
          PDE.globals$jumpto.list <- resetformnopdf()
          
          ## fill infos for all files in folder
          tcltk::tclvalue(analysis.folder_location.var) <- dirname(analysis.file_location)
          
          analysis.files <- searchfolder(tcltk::tclvalue(analysis.folder_location.var))
          for (f in 1:length(analysis.files)) {
            fill.folder.infos(NA, analysis.files[f])
          }
          ## update the list
          tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
        }
        
        ## read in the table
        if (grepl("\\.csv$", analysis.file_location)) {
          values_table <- read.problemtable(analysis.file_location,
                                            header = TRUE, quote = "\"", sep = ",")
        } else if (grepl("\\.tsv$", analysis.file_location)) {
          values_table <- read.problemtable(analysis.file_location,
                                            sep = "\t", header = TRUE, quote = "\"")
        }
        
        ## fill form for chosen file
        fill.folder.infos(values_table, analysis.file_location)
        
        numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        pos <- as.numeric(numb)
        
        ## highlight search words if highlighting is on
        highlighted.values_table <- markwords(values_table,
                                              analysis.file_location, 
                                              pos)
        ## update display of sentences
        display.values_table <- adjust.sent.display(highlighted.values_table)
        
        fill.table(display.values_table, analysis.file_location)
        
        
        tcltk::tclvalue(jumpto.var) <- grep(filename, PDE.globals$jumpto.list,
                                            value = TRUE, fixed = TRUE)
        
        ## match PDF files if pdffolder was chosen
        if (!tcltk::tclvalue(pdffolder.var) == "") {
          load.pdffolder(tcltk::tclvalue(pdffolder.var))
        }
        
      } else {
        tcltk::tkmessageBox(title = "Error", type = "ok",
                            icon = "error", message = "No accepted analysis file was chosen.")
      }
      
      ## enable extract table if TSV file is chosen
      if (!(tcltk::tclvalue(tsvfile.var) == "") && file.exists(tcltk::tclvalue(fullpdfname.var))) {
        tcltk::tkconfigure(l4.extract.table.but, state = "normal")
      }
      
    }
    l1.open.analysis.file.but <- tcltk2::tk2button(l1, text = "Open analysis file",
                                                   command = open.analysis.file)
    
    ## open analysis folder
    load.analysis.folder <- function() {
      analysis.folder_location <- tcltk::tk_choose.dir(default = tcltk::tclvalue(analysis.folder_location.var),
                                                       caption = "Choose analysis folder with the results form the PDE analyzer")
      if (!is.na(analysis.folder_location) && analysis.folder_location !=
          "") {
        ## if a new folder is loaded, reset form
        if (!analysis.folder_location == tcltk::tclvalue(analysis.folder_location.var)) {
          PDE.globals$jumpto.list <- resetformnopdf()
        }
        tcltk::tclvalue(analysis.folder_location.var) <- analysis.folder_location
        
        analysis.files <- searchfolder(tcltk::tclvalue(analysis.folder_location.var))
        for (f in 1:length(analysis.files)) {
          ## update progressbar
          tcltk::tclvalue(progress.var) <- as.character(round(((f -
                                                                  1)/length(analysis.files) * 100),
                                                              digits = 0))
          tcltk::tkconfigure(l1.progress.bar.pb, value = as.numeric(tcltk::tclvalue(progress.var)))
          tcltk::tkconfigure(l1.progress.label, text = paste0(tcltk::tclvalue(progress.var),
                                                              "%"))
          tcltk::tcl("update")
          ## load files
          if (grepl("\\.csv$", analysis.files[f])) {
            values_table <- read.problemtable(analysis.files[f],
                                              header = TRUE, quote = "\"", sep = ",")
          } else if (grepl("\\.tsv$", analysis.files[f])) {
            values_table <- read.problemtable(analysis.files[f],
                                              sep = "\t", header = TRUE, quote = "\"")
          }
          ## fill infos for all
          fill.folder.infos(values_table, analysis.files[f])
          
          ## fill the form for the first file
          if (f == 1) {
            ## highlight search words if highlighting is on
            highlighted.values_table = markwords(values_table,
                                                 analysis.files[1], 1)
            ## update display of sentences
            display.values_table <- adjust.sent.display(highlighted.values_table)
            
            fill.table(display.values_table, analysis.files[1])
            
            tcltk::tclvalue(jumpto.var) <- grep(basename(analysis.files[1]),
                                                PDE.globals$jumpto.list, value = TRUE, fixed = TRUE)
            
            ## match PDF files if pdffolder was chosen
            if (!tcltk::tclvalue(pdffolder.var) == "") {
              load.pdffolder(tcltk::tclvalue(pdffolder.var))
            }
          }
        }
        ## update progressbar
        tcltk::tkconfigure(l1.progress.bar.pb, value = 100)
        tcltk::tkconfigure(l1.progress.label, text = "complete")
        tcltk::tcl("update")
        tcltk::tkfocus(ttreader)
        tcltk::tkraise(ttreader)
        
        ## update the list
        tcltk::tkconfigure(l5.jumpto.cb, values = PDE.globals$jumpto.list)
      } else {
        tcltk::tkmessageBox(title = "Error", type = "ok",
                            icon = "error", message = "No analysis folder was chosen.")
      }
      
      ## enable extract table if TSV file is chosen
      if (!(tcltk::tclvalue(tsvfile.var) == "") && file.exists(tcltk::tclvalue(fullpdfname.var))) {
        tcltk::tkconfigure(l4.extract.table.but, state = "normal")
      }
      
    }
    l1.load.analysis.folder.but <- tcltk2::tk2button(l1, text = "Load analysis folder",
                                                     command = load.analysis.folder)
    
    ## line 5
    previous.file <- function() {
      if (!tcltk::tclvalue(jumpto.var) == "") {
        numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        pos <- as.numeric(numb)
        ## if the pos is at least at position 2
        if (pos > 1){
          tcltk::tclvalue(jumpto.var) <- PDE.globals$jumpto.list[pos - 1]
        }
        ## if there are saved tables in the masterlist
        if (!is.null(PDE.globals$tables.masterlist)) {
          numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
          pos <- as.numeric(numb)
          ## test if table exists
          res <- try(PDE.globals$tables.masterlist[["values_table"]][[pos]],silent = TRUE)
          if (!inherits(res,"try-error") && !is.null(res)) {
            tab <- PDE.globals$tables.masterlist[["values_table"]][[pos]]
            loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
            ## highlight search words if highlighting is on
            hightlighted.tab <- markwords(tab,
                                          loc, pos)
            ## update display of sentences
            display.values_table <- adjust.sent.display(hightlighted.tab)
            
            fill.table(table = display.values_table,
                       analysis.file_location = loc)
            ## match PDF files if pdffolder was chosen
            if (!tcltk::tclvalue(pdffolder.var) == "") {
              load.pdffolder(tcltk::tclvalue(pdffolder.var))
            }
          } else {
            loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
            open.analysis.file(analysis.file_location = loc)
          }
        } else {
          filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
          searchfilename <- gsub("+", "[+]", filename,
                                 fixed = TRUE)
          ## find the file in the folder
          fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                     pattern = searchfilename, full.names = TRUE,
                                     recursive = TRUE)[1]
          fullfilename <- gsub("\\\\", "/", fullfilename)
          open.analysis.file(analysis.file_location = fullfilename)
        }
      }
    }
    l9.prev.but <- tcltk2::tk2button(l9, text = "Prev", width = 4,
                                     command = previous.file, underline = "0")
    
    next.file <- function() {
      
      if (!tcltk::tclvalue(jumpto.var) == "") {
        
        numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
        pos <- as.numeric(numb)
        ## if the pos smaller than the list
        if (pos < length(PDE.globals$jumpto.list)){
          tcltk::tclvalue(jumpto.var) <- PDE.globals$jumpto.list[(pos + 1)]
        }
        if (!is.null(PDE.globals$tables.masterlist)) {
          numb = gsub("-.*", "", tcltk::tclvalue(jumpto.var))
          pos <- as.numeric(numb)
          ## test if table exists
          res <- try(PDE.globals$tables.masterlist[["values_table"]][[pos]],silent = TRUE)
          if (!inherits(res,"try-error") && !is.null(res)) {
            tab <- PDE.globals$tables.masterlist[["values_table"]][[pos]]
            loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
            ## highlight search words if highlighting is on
            hightlighted.tab <- markwords(tab,
                                          loc, pos)
            ## update display of sentences
            display.values_table <- adjust.sent.display(hightlighted.tab)
            
            fill.table(table = display.values_table,
                       analysis.file_location = loc)
            ## match PDF files if pdffolder was chosen
            if (!tcltk::tclvalue(pdffolder.var) == "") {
              load.pdffolder(tcltk::tclvalue(pdffolder.var))
            }
          } else {
            loc <- PDE.globals$tables.masterlist[["analysis.file_location"]][[pos]]
            open.analysis.file(analysis.file_location = loc)
          }
        } else {
          filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
          searchfilename <- gsub("+", "[+]", filename,
                                 fixed = TRUE)
          ## find the file in the folder
          fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                     pattern = searchfilename, full.names = TRUE,
                                     recursive = TRUE)[1]
          fullfilename <- gsub("\\\\", "/", fullfilename)
          open.analysis.file(analysis.file_location = fullfilename)
          
        }
      }
    }
    l9.next.but <- tcltk2::tk2button(l9, text = "Next", width = 4,
                                     command = next.file, underline = "0")
    
    show.more.sentences <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## subtract a value
      if ((as.numeric(tcltk::tclvalue(sent.count.indicator.var)) + 1) > 0){
        tcltk::tclvalue(sent.count.indicator.var) <- "0"
        msg <- paste("Maximum number of sentences to be displayed is reached.",
                     "No more sentences are available in the analysis file for display")
        tcltk::tkmessageBox(title = "Sentence display",
                            type = "ok", icon = "warning",
                            message = msg)
      } else {
        tcltk::tclvalue(sent.count.indicator.var) <- as.numeric(tcltk::tclvalue(sent.count.indicator.var)) + 1
      }
      
      ## reload the table
      if (!(tcltk::tclvalue(jumpto.var) == "")) {
        filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        open.analysis.file(analysis.file_location = fullfilename)
      } else {
        markwords("", "", 0)
      }
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    l7.show.more.sentences.but  <- tcltk2::tk2button(l7, text = "+",
                                                     command = show.more.sentences, width = 2)
    
    default.sentences <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## subtract a value
      tcltk::tclvalue(sent.count.indicator.var) <- "0"
      
      ## reload the table
      if (!(tcltk::tclvalue(jumpto.var) == "")) {
        filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        open.analysis.file(analysis.file_location = fullfilename)
      } else {
        markwords("", "", 0)
      }
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    l7.default.sentences.but <- tcltk2::tk2button(l7, text = "o",
                                                  command = default.sentences, width = 2)
    
    show.less.sentences <- function() {
      ##save scroll position
      save.scroll.pos <- as.character(tcltk::tkyview(l8.analysis.file.table))[1]
      ## subtract a value
      tcltk::tclvalue(sent.count.indicator.var) <- as.numeric(tcltk::tclvalue(sent.count.indicator.var)) - 1
      
      ## reload the table
      if (!(tcltk::tclvalue(jumpto.var) == "")) {
        filename <- gsub("^[^-]*-", "", tcltk::tclvalue(jumpto.var))
        searchfilename <- gsub("+", "[+]", filename,
                               fixed = TRUE)
        fullfilename <- list.files(tcltk::tclvalue(analysis.folder_location.var),
                                   pattern = searchfilename, full.names = TRUE,
                                   recursive = TRUE)[1]
        open.analysis.file(analysis.file_location = fullfilename)
      } else {
        markwords("", "", 0)
      }
      ## reset scroll position
      tcltk::tkyview.moveto(l8.analysis.file.table,save.scroll.pos)
    }
    l7.show.less.sentences.but  <- tcltk2::tk2button(l7, text = "-",
                                                     command = show.less.sentences, width = 2)
    
    ## set hotkeys ---------------------------------------------------
    
    tcltk::tkbind(ttreader, "<p>", function() previous.file())
    tcltk::tkbind(ttreader, "<Left>", function() previous.file())
    tcltk::tkbind(ttreader, "<Up>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                     -1, "unit"))
    tcltk::tkbind(ttreader, "<n>", function() next.file())
    tcltk::tkbind(ttreader, "<Right>", function() next.file())
    tcltk::tkbind(ttreader, "<Down>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                       1, "unit"))
    tcltk::tkbind(ttreader, "<f>", function() {
      if (!tcltk::tclvalue(mark.var) == "") flagfile()
    })
    tcltk::tkbind(ttreader, "<space>", function() {
      if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
    })
    tcltk::tkbind(ttreader, "<x>", function() {
      if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
    })
    tcltk::tkbind(ttreader, "<u>", function() {
      if (!tcltk::tclvalue(mark.var) == "") unmarkfile()
    })
    tcltk::tkbind(ttreader, "<o>", function() {
      if (!tcltk::tclvalue(pdfname.var) == "") openpdf()
    })
    
    change.hotkey.mode <- function() {
      
      if (tcltk::tclvalue(hotkey.mode.var) == "standard") {
        ## change to onehand
        tcltk::tclvalue(hotkey.mode.var) <- "one hand"
        ## change underlines
        tcltk::tkconfigure(l9.prev.but, underline = NA)
        tcltk::tkconfigure(l9.xmarkfile.but, underline = NA)
        tcltk::tkconfigure(l9.next.but, underline = NA)
        ## set no hotkeys
        tcltk::tkbind(ttreader, "<p>", function() { })
        tcltk::tkbind(ttreader, "<Left>", function() { })
        tcltk::tkbind(ttreader, "<Up>", function() { })
        tcltk::tkbind(ttreader, "<n>", function() { })
        tcltk::tkbind(ttreader, "<Right>", function() { })
        tcltk::tkbind(ttreader, "<Down>", function() { })
        tcltk::tkbind(ttreader, "<x>", function() { })
        ## set one hand hotkeys right hand
        tcltk::tkbind(ttreader, "<j>", function() previous.file())
        tcltk::tkbind(ttreader, "<i>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        -1, "unit"))
        tcltk::tkbind(ttreader, "<l>", function() next.file())
        tcltk::tkbind(ttreader, "<k>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        1, "unit"))
        tcltk::tkbind(ttreader, "<space>", function() {
          if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
        })
        tcltk::tkbind(ttreader, "<h>", function() {
          if (!tcltk::tclvalue(mark.var) == "") flagfile()
        })
        tcltk::tkbind(ttreader, "<u>", function() {
          if (!tcltk::tclvalue(mark.var) == "") unmarkfile()
        })
        tcltk::tkbind(ttreader, "<o>", function() {
          if (!tcltk::tclvalue(pdfname.var) == "") openpdf()
        })
        ## set one hand hotkeys left hand
        tcltk::tkbind(ttreader, "<a>", function() previous.file())
        tcltk::tkbind(ttreader, "<w>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        -1, "unit"))
        tcltk::tkbind(ttreader, "<d>", function() next.file())
        tcltk::tkbind(ttreader, "<s>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        1, "unit"))
        tcltk::tkbind(ttreader, "<space>", function() {
          if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
        })
        tcltk::tkbind(ttreader, "<f>", function() {
          if (!tcltk::tclvalue(mark.var) == "") flagfile()
        })
        tcltk::tkbind(ttreader, "<e>", function() {
          if (!tcltk::tclvalue(mark.var) == "") unmarkfile()
        })
        tcltk::tkbind(ttreader, "<q>", function() {
          if (!tcltk::tclvalue(pdfname.var) == "") openpdf()
        })
      } else if (tcltk::tclvalue(hotkey.mode.var) == "one hand") {
        ## change to onehand
        tcltk::tclvalue(hotkey.mode.var) <- "oh + std"
        ## change underlines
        tcltk::tkconfigure(l4.openpdf.but, underline = "0")
        tcltk::tkconfigure(l9.prev.but, underline = "0")
        tcltk::tkconfigure(l9.flagfile.but, underline = "0")
        tcltk::tkconfigure(l9.xmarkfile.but, underline = "0")
        tcltk::tkconfigure(l9.unmarkfile.but, underline = "0")
        tcltk::tkconfigure(l9.next.but, underline = "0")
        ## set standard hotkeys
        tcltk::tkbind(ttreader, "<p>", function() previous.file())
        tcltk::tkbind(ttreader, "<Left>", function() previous.file())
        tcltk::tkbind(ttreader, "<Up>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                         -1, "unit"))
        tcltk::tkbind(ttreader, "<n>", function() next.file())
        tcltk::tkbind(ttreader, "<Right>", function() next.file())
        tcltk::tkbind(ttreader, "<Down>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                           1, "unit"))
        tcltk::tkbind(ttreader, "<x>", function() {
          if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
        })
        ## set one hand hotkeys right hand
        tcltk::tkbind(ttreader, "<j>", function() previous.file())
        tcltk::tkbind(ttreader, "<i>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        -1, "unit"))
        tcltk::tkbind(ttreader, "<l>", function() next.file())
        tcltk::tkbind(ttreader, "<k>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        1, "unit"))
        tcltk::tkbind(ttreader, "<space>", function() {
          if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
        })
        tcltk::tkbind(ttreader, "<h>", function() {
          if (!tcltk::tclvalue(mark.var) == "") flagfile()
        })
        tcltk::tkbind(ttreader, "<u>", function() {
          if (!tcltk::tclvalue(mark.var) == "") unmarkfile()
        })
        tcltk::tkbind(ttreader, "<o>", function() {
          if (!tcltk::tclvalue(pdfname.var) == "") openpdf()
        })
        ## set one hand hotkeys left hand
        tcltk::tkbind(ttreader, "<a>", function() previous.file())
        tcltk::tkbind(ttreader, "<w>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        -1, "unit"))
        tcltk::tkbind(ttreader, "<d>", function() next.file())
        tcltk::tkbind(ttreader, "<s>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                        1, "unit"))
        tcltk::tkbind(ttreader, "<space>", function() {
          if (!tcltk::tclvalue(mark.var) == "") xmarkfile()
        })
        tcltk::tkbind(ttreader, "<f>", function() {
          if (!tcltk::tclvalue(mark.var) == "") flagfile()
        })
        tcltk::tkbind(ttreader, "<e>", function() {
          if (!tcltk::tclvalue(mark.var) == "") unmarkfile()
        })
        tcltk::tkbind(ttreader, "<q>", function() {
          if (!tcltk::tclvalue(pdfname.var) == "") openpdf()
        })
      } else if (tcltk::tclvalue(hotkey.mode.var) == "oh + std") {
        ## change to onehand
        tcltk::tclvalue(hotkey.mode.var) <- "no hotkey"
        ## change underlines
        tcltk::tkconfigure(l4.openpdf.but, underline = NA)
        tcltk::tkconfigure(l9.prev.but, underline = NA)
        tcltk::tkconfigure(l9.flagfile.but, underline = NA)
        tcltk::tkconfigure(l9.xmarkfile.but, underline = NA)
        tcltk::tkconfigure(l9.unmarkfile.but, underline = NA)
        tcltk::tkconfigure(l9.next.but, underline = NA)
        ## set no hotkeys
        tcltk::tkbind(ttreader, "<p>", function() { })
        tcltk::tkbind(ttreader, "<Left>", function() { })
        tcltk::tkbind(ttreader, "<Up>", function() { })
        tcltk::tkbind(ttreader, "<n>", function() { })
        tcltk::tkbind(ttreader, "<Right>", function() { })
        tcltk::tkbind(ttreader, "<Down>", function() { })
        tcltk::tkbind(ttreader, "<f>", function() { })
        tcltk::tkbind(ttreader, "<space>", function() { })
        tcltk::tkbind(ttreader, "<x>", function() { })
        tcltk::tkbind(ttreader, "<u>", function() { })
        tcltk::tkbind(ttreader, "<o>", function() { })
        ## reset one hand hotkeys right hand
        tcltk::tkbind(ttreader, "<j>", function() { })
        tcltk::tkbind(ttreader, "<i>", function() { })
        tcltk::tkbind(ttreader, "<l>", function() { })
        tcltk::tkbind(ttreader, "<k>", function() { })
        tcltk::tkbind(ttreader, "<h>", function() { })
        ## reset one hand hotkeys left hand
        tcltk::tkbind(ttreader, "<a>", function() { })
        tcltk::tkbind(ttreader, "<w>", function() { })
        tcltk::tkbind(ttreader, "<d>", function() { })
        tcltk::tkbind(ttreader, "<s>", function() { })
        tcltk::tkbind(ttreader, "<e>", function() { })
        tcltk::tkbind(ttreader, "<q>", function() { })
      } else if (tcltk::tclvalue(hotkey.mode.var) == "no hotkey") {
        ## change to onehand
        tcltk::tclvalue(hotkey.mode.var) <- "standard"
        ## change underlines
        tcltk::tkconfigure(l4.openpdf.but, underline = "0")
        tcltk::tkconfigure(l9.prev.but, underline = "0")
        tcltk::tkconfigure(l9.flagfile.but, underline = "0")
        tcltk::tkconfigure(l9.xmarkfile.but, underline = "0")
        tcltk::tkconfigure(l9.unmarkfile.but, underline = "0")
        tcltk::tkconfigure(l9.next.but, underline = "0")
        ## set standard hotkeys
        tcltk::tkbind(ttreader, "<p>", function() previous.file())
        tcltk::tkbind(ttreader, "<Left>", function() previous.file())
        tcltk::tkbind(ttreader, "<Up>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                         -1, "unit"))
        tcltk::tkbind(ttreader, "<n>", function() next.file())
        tcltk::tkbind(ttreader, "<Right>", function() next.file())
        tcltk::tkbind(ttreader, "<Down>", function() tcltk::tkyview.scroll(l8.analysis.file.table,
                                                                           1, "unit"))
        tcltk::tkbind(ttreader, "<f>", function() {
          if (!tcltk::tclvalue(mark.var) == "")
            flagfile()
        })
        tcltk::tkbind(ttreader, "<space>", function() {
          if (!tcltk::tclvalue(mark.var) == "")
            xmarkfile()
        })
        tcltk::tkbind(ttreader, "<x>", function() {
          if (!tcltk::tclvalue(mark.var) == "")
            xmarkfile()
        })
        tcltk::tkbind(ttreader, "<u>", function() {
          if (!tcltk::tclvalue(mark.var) == "")
            unmarkfile()
        })
        tcltk::tkbind(ttreader, "<o>", function() {
          if (!tcltk::tclvalue(pdfname.var) == "")
            openpdf()
        })
      }
      ## update button caption
      tcltk::tkconfigure(l6.hotkey.but, text = tcltk::tclvalue(hotkey.mode.var))
    }
    
    l6.hotkey.but <- tcltk2::tk2button(l6, text = tcltk::tclvalue(hotkey.mode.var),
                                       width = 8, command = change.hotkey.mode)
    
    ## How the form looks --------------------------------------------
    
    ## top ----------------------------------------------------------
    tcltk::tkpack(l1.open.analysis.file.but, l1.load.analysis.folder.but,
                  l1.save.memory.but, l1.load.memory.but, side = "left",
                  pady = 2, padx = 2)
    tcltk::tkpack(l1.progress.bar.pb, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 2)
    tcltk::tkpack(l1.progress.label, l1.reset.but, side = "left",
                  pady = 2, padx = 2)
    tcltk::tkpack(l2.tsv.onoff.but, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l2.tsv.loadall.but, side = "left", pady = 2,
                  padx = 0)
    tcltk::tkpack(l2.loadtsv.entry, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l2.loadtsv.but, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l3.pdffolder.entry, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l3.loadpdffolder.but, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l4.pdfname.entry, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l4.openpdf.but, l4.extract.table.but,
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l5.jumpto.label, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l5.jumpto.cb, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l1, l2, l3, l4, l5, side = "top",
                  anchor = "nw", fill = "x")
    tcltk::tkpack(top, side = "top", fill = "x")
    
    ## mid -------------------------------------------------
    tcltk::tkpack(l6.caption.label, side = "left", pady = 2,
                  padx = 20, anchor = "nw")
    tcltk::tkpack(l6.font.label, l6.smaller.font.but, l6.default.font.but,
                  l6.larger.font.but, side = "left", pady = 2)
    tcltk::tkpack(l6.hotkey.mode.label, side = "left", padx = 5,
                  pady = 2)
    tcltk::tkpack(l6.hotkey.but, side = "left", pady = 2)
    tcltk::tkpack(l6.no.wrap.rb, l6.nowrap.label, l6.wrap.rb,
                  l6.wrap.label, side = "right", pady = 2)
    tcltk::tkpack(l7.sentence.number.label, l7.show.less.sentences.but,
                  l7.default.sentences.but, l7.show.more.sentences.but,
                  l7.txtcontent.only.cbtn,
                  l7.collapse.abbrev.cb,
                  side = "left", pady = 2)
    tcltk::tkpack(l8.analysis.file.table, side = "left", expand = TRUE,
                  fill = "both", pady = 2)
    tcltk::tkpack(scroll.y, side = "left", fill = "y", pady = 2)
    tcltk::tkpack(l6, l7, side = "top", fill = "both")
    tcltk::tkpack(l8, side = "top", expand = TRUE, fill = "both")
    tcltk::tkpack(mid, side = "top", expand = TRUE, fill = "both")
    
    ## bottom -------------------------------------------------
    tcltk::tkpack(l9.prev.but, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l9.mark.cb, l9.flagfile.but, l9.xmarkfile.but,
                  l9.unmarkfile.but, side = "left", pady = 2,
                  padx = 5)
    tcltk::tkpack(l9.next.but, expand = TRUE, fill = "x",
                  side = "left", pady = 2, padx = 5)
    tcltk::tkpack(l9, side = "top", anchor = "nw", fill = "x")
    
    tcltk::tkpack(bot, side = "top", fill = "x")
    
    tcltk::tkfocus(ttreader)
    tcltk::tkraise(ttreader)
    tcltk::tcl("update")
    
    ## default font size --------------------------------------
    current.font <- as.numeric(tcltk::tkfont.actual("TkDefaultFont",
                                                    "-size"))
    if (current.font < 2)
      current.font <- 10
    defaultfontsize.var <- tcltk::tclVar(current.font)
    fontsize.var <- tcltk::tclVar(current.font)
    tcltk::tkfocus(ttreader)
    tcltk::tkraise(ttreader)
  } ## end tcltk test
}
