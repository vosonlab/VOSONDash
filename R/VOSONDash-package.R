#' @title Interface for collection and interactive analysis of social networks
#'
#' @description VOSONDash provides functions and an interface in the form of an interactive R 
#' Shiny web application for the visualisation and analysis of network data. The app has 
#' sections for visualising and manipulating network graphs, performing text analysis, and 
#' displaying network metrics. It also has an interface for the collection of social network 
#' data using the vosonSML R package.
#' 
#' @name VOSONDash-package
#' @aliases VOSONDash-package VOSONDash
#' @docType package
#' @author Bryan Gertzel and Robert Ackland.
#' 
#' @importFrom data.table data.table
#' @importFrom graphics plot text title par barplot
#' @import httpuv
#' @importFrom httr parse_url
#' @importFrom igraph vertex_attr as_edgelist gsize delete_vertices components simplify vcount 
#' betweenness closeness set_graph_attr V 'V<-' E 'E<-' degree edge_attr_names vertex_attr_names
#' reciprocity transitivity mean_distance graph.density is.directed ecount count_components
#' centr_degree centr_betw centr_clo is.igraph
#' @importFrom lattice barchart
#' @importFrom magrittr '%>%' '%<>%'
#' @importFrom RColorBrewer brewer.pal
#' @import shiny
#' @import SnowballC
#' @importFrom systemfonts system_fonts
#' @importFrom syuzhet get_nrc_sentiment
#' @importFrom tm VCorpus VectorSource tm_map content_transformer DocumentTermMatrix
#' removeSparseTerms removeNumbers removePunctuation stopwords stemDocument
#' @importFrom textutils HTMLdecode
#' @importFrom utils packageVersion
#' @importFrom vosonSML Authenticate Collect Create
#' @importFrom wordcloud wordcloud
NULL
