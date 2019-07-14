# VOSONDash

`VOSONDash` is an interactive [R Shiny](https://shiny.rstudio.com/) web application for the visualisation and analysis of social network data. The app has a dashboard layout with sections for visualising and manipulating network graphs, performing text analysis, displaying network metrics and the collection of network data using the [vosonSML](https://github.com/vosonlab/vosonSML) R package.

## Installation & Getting Started

`VOSONDash` is an R package and must be installed before the app can be run.

### Package Installation

Using the devtools package the latest version of VOSON Dashboard can be downloaded and installed directly from github.

```R
# install.packages("devtools")
devtools::install_github("vosonlab/VOSONDash")
```

Once the VOSON Dashboard package is installed the Shiny web application can be run from the RStudio console using the `runVOSONDash()` function.

```R
library(VOSONDash)
runVOSONDash()
```
### Running the app for the first time

When run the `VOSONDash` app will check that all of the R packages that are required to make it work are installed. If run for the first time it is likely that some packages will be missing and the app will print a message indicating the missing packages and a function command that can be used to install them.

For example:

```R
> runVOSONDash()
=================================================
VOSONDash v0.4.0 16June19 
27 Jun 2019 09:35

x86_64-apple-darwin15.6.0 
R version 3.6.0 (2019-04-26)
R shiny 1.3.2 

Home: /Users/voson_user
Encoding: native.enc 

Checking packages...
Required Packages Missing:
- visNetwork
- syuzhet
```

The missing `visNetwork` and `syuzhet` packages can be installed using the provided package install command.

```R
Please install required packages before using VOSONDash:

install.packages(c("visNetwork","syuzhet"))
```

After installing required packages and running again the `VOSONDash` Shiny app will open up in the default web browser.