## Re-submission 2
This is a re-submission. In this version I have:

* Changed 'cat()' functions to 'message()' except for *Server.R files that 
  are using cat to communicate caught errors as a single character string 
  to a shiny ui component via a 'capture.output' method with type 'output'
* Removed use of 'installed.packages()' and replaced with 'require()'
* Added saving and restoring of users graphical parameters in functions 
  that alter them with the 'par()' function
* Incremented package version to 0.4.4 with minor updates listed in NEWS

## Re-submission 1

* Re-worded the Description and surrounded package names with single quotes
* Re-worded the Title changing 'Social Media Data' to 'Social Networks' 

## Test environments
* local MacOS X, R 3.6.0
* local Windows 10, R 3.6.1
* R-Devel r76784 Windows (Winbuilder) 

## R CMD check results
0 errors | 0 warnings
* checking CRAN incoming feasibility ... NOTE
New submission
