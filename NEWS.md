# PDE 1.4.9
*Fixed bug with table display in PDE_analyzer_i()
*Combined continued tables

# PDE 1.4.8
* Fixed bug with PDE_check_Xpdf_install() function

# PDE 1.4.7
*fixed bug with save.tab.by.category variable
*fixed issue with intToUtf8(0xFB01) - intToUtf8(0xFB06) which produce merge letters

# PDE 1.4.6
*added save.tab.by.category variable to seperate tables according to search word found in

# PDE 1.4.5
*Fixed PDF recognition in PDE_reader_i
*Added ability to separate pdfs according to category

# PDE 1.4.4
*Added recognition of superscript and subscript in tables when extracted
*Fixed issue with international Windows version xpdf install

# PDE 1.4.3
*Added the ability to have search word categories
*Fixed bug with display of non-extracted tables
*Fixed bug with overlapping tables
*Fixed bug with absolute filter word threshold
*Fixed bug with very small tables towards the end of the page
*Fixed strtrim bug

# PDE 1.4.2
*Fixed problem with outputting to locations with spaces

# PDE 1.4.1
* Added message box that opens when using PDE_analyzer_i() and xpdf command line tools are not installed
* Fixed issue with correct numbering in PDE_analyzer_word_stat table
* Fixed issue when having special characters in PDF file names
* Fixed PDE_check_Xpdf_install() error when config fall was copied from another PC
* Fixed error when extracting small tables
* Fixed clearing of pdf field when cancel was selected
* Fixed issue with pdfs containing empty pages

# PDE 1.4.0
* Fixed bug with PDE_analyzer_i() trying to display .png files
* Fixed bug with greyed out searchwords entry box
* Added regex_sw and regex_fw parameters to turn on and off regex formating of search and filter words
* changed to shorter output file suffixes
  * _not.enough.txt.w.filter.words --> _too_few_fwds
  * _no.txt.w.search.words --> _no_txt_w_swds
  * no_table --> no_tab_found
  * _no.table.w.search.words --> _no_tab_w_swds
  * _no.txt.w.search.words --> _no_txt_w_swds
* removed full table heading from output file name of extracted tables
* PDE_analyzer function return a table with search word and filterw word statistics
* txt+- folder contains a txt file with the list of search words used
* PDE_analyzer_word_stat table is written in the outputfolder
* added the option to copy or move processed pdf files into output folder based on filter word presence
* corrected xpdf tools 4.2.0 version to xpdf tools 4.02

# PDE 1.3.0
* Updated link to xpdf tools 4.2.0
* Corrected multibyte error for PDE_reader_i()
* Added display of output table to PDE_analyzer_i()
* Added dev_x and dev_y (new tsv format!)

# PDE 1.2.1
* Removed LazyLoad: true due to new policies concerning non-existing data path
* changed https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf to https://www.rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf

# PDE 1.2.0
* Added tabs to easier navigate the PDE_analyzer_i() interface
* Removed PDE_install_XpdfReader4.02()
* Deprecated PDE_path() (use system.file(package = "PDE") instead)
* changed standard settings to table extraction without search words
* changed default exp.nondetc.tabs, write.tab.doc.file, and write.txt.doc.file to TRUE
* Added remove_backref(x) function to process tables that have strings identified as 
backreferences such \1 or \0 in their heading
* added export of indicator file in the table folder if table was detected but could not be processed
* fixed the interpretation of elements in HTML version of the PDF file, which do not have positional 
(left & top) information
* several other bug fixes

# PDE 1.1.2
* Changed whereis command for Solaris OS again
* Fixed tcltk issue for old mac versions

# PDE 1.1.1
* Changed whereis command for Solaris OS for PDE_check_Xpdf_install() function

# PDE 1.1.0
* Deprecated function PDE_install_XpdfReader4.02() for new function PDE_install_Xpdftools4.02()
* PDE_install_Xpdftools4.02() installs the Xpdf command line tools instead of the XpdfReader
* Updated PDE_check_Xpdf_install()
* Added notification to install latest version of xquartz if tcltk does not work right
* Dissolve requirement of changing global PATH variable incorporation of /bin/XPDF_DIR.config
* Fixed issue with the recognition of non-readable files

# PDE 1.0.2

* Fixed the install message for PDE_install_XpdfReader4.02().
* Fixed problem with generation of keeplayout.txt files.
* Added linebeaks to Console outputs.

# PDE 1.0.1

* msg error for PDE_check_Xpdf_install() on the Solaris operational system was fixed
* Recognition of the Solaris operational system as Linux was added

# PDE 1.0.0

* The package is ready for publication!