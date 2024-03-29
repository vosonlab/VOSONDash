
# VOSONDash - Shiny Dashboard App<img src="https://vosonlab.github.io/VOSONDash/images/logo.png" width="140px" align="right" />

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/VOSONDash)](https://CRAN.R-project.org/package=VOSONDash)
[![CRAN_Monthly](https://cranlogs.r-pkg.org/badges/VOSONDash)](https://CRAN.R-project.org/package=VOSONDash)
[![CRAN_Total](https://cranlogs.r-pkg.org/badges/grand-total/VOSONDash)](https://CRAN.R-project.org/package=VOSONDash)
[![Github_Release](https://img.shields.io/github/release-pre/vosonlab/VOSONDash.svg?logo=github)](https://github.com/vosonlab/VOSONDash/releases)
[![Github_Dev](https://img.shields.io/static/v1?label=dev&message=v0.6.1&logo=github)](https://github.com/vosonlab/VOSONDash)
[![Last_Commit](https://img.shields.io/github/last-commit/vosonlab/VOSONDash.svg?&logo=github)](https://github.com/vosonlab/VOSONDash/commits/master)
[![Build_Status](https://github.com/vosonlab/VOSONDash/workflows/R-CMD-check/badge.svg)](https://github.com/vosonlab/VOSONDash/actions)

`VOSONDash` is an interactive [R Shiny](https://shiny.rstudio.com/) web
application for the visualisation and analysis of social network data.
The app has a dashboard layout with sections for visualising and
manipulating network graphs, performing text analysis, displaying
network metrics and the collection of network data using the
[vosonSML](https://github.com/vosonlab/vosonSML) R package.

## Installation Options

Install the most recent CRAN release:

``` r
install.packages("VOSONDash")
```

Install the most recent release tag via GitHub:

``` r
install.packages(
  "https://github.com/vosonlab/VOSONDash/releases/download/v0.5.11/VOSONDash-0.5.11.tar.gz",
  repo = NULL, type = "source")
```

Install the latest development version:

``` r
# library(remotes)
remotes::install_github("vosonlab/VOSONDash")
```

Once the VOSON Dashboard package is installed and loaded the Shiny web
application can be run from the RStudio console using the
`runVOSONDash()` function.

``` r
library(VOSONDash)
runVOSONDash()
```

### Running the app for the first time

When run the `VOSONDash` app will check that all of the R packages that
are required to make it work are installed. It is likely that some
packages will be missing and the app will print a message indicating the
missing packages and a command that can be used to install them.

For example:

``` r
runVOSONDash()
# =================================================
# VOSONDash v0.6.1
# ...
# Checking packages...
# 
# Error: Required packages missing.
# - shinyjs
# - visNetwork
# 
# Please install required packages before using VOSONDash:
# install.packages(c("shinyjs", "visNetwork"))
```

The missing packages can be installed using the above `install.packages`
command.

After installing required packages and running again the `VOSONDash`
Shiny app will open up in the default web browser.

## VOSON Dashboard

`VOSONDash` features an intuitive web interface with a section for
‘Analysis’ of graph data loaded from [igraph](https://igraph.org/r/)
objects and a section for the ‘Collection’ of social media data using
[vosonSML](https://github.com/vosonlab/vosonSML).

### Analysis

Network and text analysis of graph data.

- Network Graphs: Visualise and modify networks
- Network Metrics: Calculate node and network level metrics
- Text Analysis: Word frequency, word clouds and sentiment
- Assortativity: Calculate homogeneity and homophily indexes (if VOSON
  categorical node attributes present)

![VOSONDash Network Graphs
Analysis](https://vosonlab.github.io/VOSONDash/images/network-graphs-1420x847.jpg)

Figure 1. Environmental activist site hyperlink network loaded from a
`graphml` file and plotted by the `visNetwork` package.

### Collection and Network Creation

Graphical interfaces for collecting network data from social media
API’s.

- Collect: Twitter, youtube and reddit network data
- Create: different types of networks from the data such as activity,
  actor, twomode and semantic networks

![VOSONDash Twitter
Collection](https://vosonlab.github.io/VOSONDash/images/collection-twitter-1420x980.jpg)

Figure 2. Collection of recent `#auspol` tweets and generation of an
actor network with the `vosonSML` package.

![VOSONDash Twitter Network
Creation](https://vosonlab.github.io/VOSONDash/images/creation-twitter-1420x848.jpg)

Figure 3. Create an actor network and add tweet text to the network.

### API Keys and Tokens

- API Keys: Enter, save, load API keys and create access tokens

![VOSONDash Keys and Tokens
Interface](https://vosonlab.github.io/VOSONDash/images/api-twitter-1420x848.jpg)

Figure 4. Twitter API token creation and selection.

## Special thanks

This application would not be possible without key packages by other
authors in the R community, particularly the
[shiny](https://github.com/rstudio/shiny),
[shinydashboard](https://github.com/rstudio/shinydashboard),
[DT](https://github.com/rstudio/DT) and
[shinyjs](https://github.com/daattali/shinyjs) packages. Graph
visualisations created with [igraph](https://github.com/igraph/rigraph)
and [visNetwork](https://github.com/datastorm-open/visNetwork), and text
analysis with support from [tm](https://CRAN.R-project.org/package=tm),
[SnowballC](https://CRAN.R-project.org/package=SnowballC),
[wordcloud](https://CRAN.R-project.org/package=wordcloud) and
[syuzhet](https://CRAN.R-project.org/package=syuzhet) packages.

## Code of Conduct

Please note that the VOSON Lab projects are released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
