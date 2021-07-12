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