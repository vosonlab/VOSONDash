# VOSONDash
![Github Release](https://img.shields.io/github/release-pre/vosonlab/VOSONDash.svg?logo=github&colorB=yellow)
![Last Commit](https://img.shields.io/github/last-commit/vosonlab/VOSONDash.svg)

`VOSONDash` is an interactive [R Shiny](https://shiny.rstudio.com/) web application for the visualisation and analysis of social network data. The app has a dashboard layout with sections for visualising and manipulating network graphs, performing text analysis, displaying network metrics and the collection of network data using the [vosonSML](https://github.com/vosonlab/vosonSML) R package.

## Installation

Installation simply requires [downloading](https://github.com/vosonlab/VOSONDash/archive/master.zip) the `VOSONDash` files and unzipping them to a folder. The app is opened in [RStudio](https://www.rstudio.com/) by either creating a new project using the `VOSONDash` directory or navigating to the folder using the RStudio File browser tab.

The latest `VOSONDash` can be downloaded from the [VOSON lab](https://github.com/vosonlab) on github using the following link: 
https://github.com/vosonlab/VOSONDash/archive/master.zip

## Getting started

The app can be run by typing `runApp()` in the RStudio Console (optionally with a path `appDir = '~/my/voson/dash/folder/path/'`) or alternatively by clicking on the `Run App` button that appears in the top right of the RStudio file viewer frame when either the `ui.R` or `server.R` files are open.

| ![VOSONDash Interface](VOSONDash/www/VOSONDash-NetworkGraphs-1400x990.jpg)
|:--| 
| `VOSONDash` app interface. |
