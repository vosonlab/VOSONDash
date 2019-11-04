# VOSONDash
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/VOSONDash)](https://CRAN.R-project.org/package=VOSONDash)
![Downloads](https://cranlogs.r-pkg.org/badges/VOSONDash)
![Total](https://cranlogs.r-pkg.org/badges/grand-total/VOSONDash)
![Github Release](https://img.shields.io/github/release-pre/vosonlab/VOSONDash.svg?logo=github&colorB=8065ac)
![Dev](https://img.shields.io/static/v1?label=dev&message=v0.5.0&color=orange&logo=github)
![Last Commit](https://img.shields.io/github/last-commit/vosonlab/VOSONDash.svg?logo=github)

`VOSONDash` is an interactive [R Shiny](https://shiny.rstudio.com/) web application for the visualisation and analysis of social network data. The app has a dashboard layout with sections for visualising and manipulating network graphs, performing text analysis, displaying network metrics and the collection of network data using the [vosonSML](https://github.com/vosonlab/vosonSML) R package.

## Installation

`VOSONDash` is an R package and must be installed before the app can be run.

Install the latest release via CRAN:
```R
install.packages("VOSONDash")
```

Install the latest release via GitHub:
```R
install.packages("https://github.com/vosonlab/VOSONDash/releases/download/v0.4.4/VOSONDash-0.4.4.tar.gz", 
  repo = NULL, type = "source")
```

Install the latest development version:
```R
# library(devtools)

devtools::install_github("vosonlab/VOSONDash")
```

Once the VOSON Dashboard package is installed and loaded the Shiny web application can be run from the RStudio console using the `runVOSONDash()` function.

```R
library(VOSONDash)

runVOSONDash()
```

### Running the app for the first time

When run the `VOSONDash` app will check that all of the R packages that are required to make it work are installed. It is likely that some packages will be missing and the app will print a message indicating the missing packages and a command that can be used to install them.

For example:

```R
> runVOSONDash()
=================================================
VOSONDash v0.4.4
01 Aug 2019 09:35

...

Checking packages...

Error: Required packages missing.
- visNetwork
- syuzhet
```

The missing packages can be installed using the provided package install command.

```R
Please install required packages before using VOSONDash:

install.packages(c("visNetwork","syuzhet"))
```

After installing required packages and running again the `VOSONDash` Shiny app will open up in the default web browser.

## VOSON Dashboard

`VOSONDash` features an intuitive web interface with a section for 'Analysis' of graph data loaded from [igraph](https://igraph.org/r/) objects and a section for the 'Collection' of social media data using [vosonSML](https://github.com/vosonlab/vosonSML).

### Analysis

Network and text analysis of graph data.

* Network Graphs: Visualise and modify networks
* Network Metrics: Calculate node and network level metrics
* Text Analysis: Word frequency, word clouds and sentiment
* Assortativity: Calculate homogeneity and homophily indexes (if VOSON categorical node attributes present)

![VOSONDash Network Graphs Analysis](man/figures/network-graphs-1420x880.jpg)

Fig 1. Environmental activist site hyperlink network loaded from a `graphml` file and plotted by the `visNetwork` package.

### Collection and Network Creation

Graphical interfaces for collecting network data from social media API's.

* Collect: Twitter, youtube and reddit network data
* Create: different types of networks from the data such as activity, actor, bimodal and semantic networks
* API Keys: Enter, save, load API keys and create access tokens

![VOSONDash Twitter Collection](man/figures/collection-twitter-1420x880.jpg)

Fig 2. Collection of recent `#auspol` tweets and generation of an actor network with the `vosonSML` package.

## Special thanks

This application would not be possible without key packages by other authors in the R community, particularly the [shiny](https://github.com/rstudio/shiny), [shinydashboard](https://github.com/rstudio/shinydashboard), [DT](https://github.com/rstudio/DT) and [shinyjs](https://github.com/daattali/shinyjs) packages. Graph visualisations created with [igraph](https://github.com/igraph/rigraph) and [visNetwork](https://github.com/datastorm-open/visNetwork), and text analysis with support from [tm](https://CRAN.R-project.org/package=tm), [SnowballC](https://CRAN.R-project.org/package=SnowballC), [wordcloud](https://CRAN.R-project.org/package=wordcloud) and [syuzhet](https://CRAN.R-project.org/package=syuzhet) packages.
