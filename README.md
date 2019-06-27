# VOSONDash

`VOSONDash` is an interactive [R Shiny](https://shiny.rstudio.com/) web application for the visualisation and analysis 
of social network data. The app has a dashboard layout with sections for visualising and manipulating network graphs, performing text analysis, displaying network metrics and the collection of network data using the [vosonSML](https://github.com/vosonlab/vosonSML) R package.

## Installation & Getting Started

`VOSONDash` can either be installed as a package or downloaded, unzipped and run from a folder.

### Package Installation

Using the devtools package the latest version of VOSON Dashboard can be downloaded and installed directly from github.

```R
# install.packages("devtools")
devtools::install_github("mishoryu/VOSONDash", ref = "develop-package")
```

Once the VOSON Dashboard package is installed the Shiny web application can be run from the RStudio console using the `runVOSONDash()` function.

```R
library(VOSONDash)
runVOSONDash()
```

### Folder Installation 

The `VOSONDash` package zip file can be directly downloaded from github using the following link: https://github.com/mishoryu/VOSONDash/archive/develop-package.zip

The app can then be opened in [RStudio](https://www.rstudio.com/) by either creating a new project in the existing 
downloaded `VOSONDash` root folder or by navigating to the folder using the RStudio File browser tab. 

The app can be run using the following Shiny function:
```R
shiny::runApp(appDir = "./inst/vosondash")
```

### Running the app for the first time

When run the `VOSONDash` app will check that all of the R packages that are required to make it work are installed. If 
run for the first time it is likely that some packages will be missing and the app will print a message indicating the
missing packages and a function command that can be used to install them.

For example:

```R
> shiny::runApp('./inst/vosondash')
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

Please install required packages before using VOSONDash:

install.packages(c("visNetwork","syuzhet"))
```

The missing `visNetwork` and `syuzhet` packages can be installed using the provided package install command.

```R
install.packages(c("visNetwork","syuzhet"))
```

After installing required packages and running again the `VOSONDash` Shiny app will open up in the default web browser.

## Changes & Notes

#### Demonstration Data

The `Network Graphs` section now contains a select box for loading demonstration data as the data is installed as part 
of the package in the package library and can be difficult to locate or directly inaccessible. As such if `VOSONDash` 
is run from a folder instead of the package then the list of demonstration files wont be found and populated into the 
select box (unless the package is also installed). The files can be opened manually however by browsing to the `inst/extdata` sub-folder in the `VOSONDash` root.

<img src="/inst/images/VOSONDash-Demo-Data.png">

#### Changes to API Keys & Tokens

`VOSONDash` now loads and saves social media keys and tokens to files in the users home directory as specified by the environment variable `HOME`. This location can be found in the `VOSONDash` start up information, in the apps `API Keys` 
tab or by using the following R function in RStudio:

```R
> Sys.getenv("HOME")
[1] "/Users/voson_user"
```

This is a system environment variable and will likely be used by the OS and other software so it is highly inadvisable 
to change its value.

When API keys or tokens are saved they will be saved to the following files:

```R
# if HOME is /Users/voson_user
/Users/voson_user/vosondash_keys.rds
/Users/voson_user/vosondash_tokens.rds
```

A twitter web auth token can now be created and used for API access without the need for a Developer account. This still requires a twitter app and `Consumer` API keys to initially generate and authorize the token for a users twitter account but the keys are then no longer required once the token has been created.

Enter the app name and consumer keys and click on the `Create Web Auth Token` button.
<img src="/inst/images/VOSONDash-Twitter-Token-1.png">

A new browser tab will open asking you to Log into twitter with your user account and to authorize the twitter app.
<img src="/inst/images/VOSONDash-Twitter-Token-2.png">

When the authentication complete message appears the new browser tab can be closed.
<img src="/inst/images/VOSONDash-Twitter-Token-3.png">

If successful details of the created token will be displayed and the token should then be added to the twitter token select list by clicking on `Add Token to Select List` button.
<img src="/inst/images/VOSONDash-Twitter-Token-4.png">

The token can then be used for twitter collection by clicking on the 'Use Token' button. It should be saved for future use by clicking on the `Save Tokens` button.
<img src="/inst/images/VOSONDash-Twitter-Token-5.png">

The token being used for collection will then appear in the `Twitter` section of the app.
<img src="/inst/images/VOSONDash-Twitter-Token-6.png">
