#' @title Interface for collection and interactive analysis of social networks
#'
#' @description Description field.
#' 
#' @name VOSONDash-package
#' @aliases VOSONDash-package VOSONDash
#' @docType package
#' @author Robert Ackland and Bryan Gertzel
#' 
#' @import shiny
#' @import httpuv
#' @importFrom tm DocumentTermMatrix removeSparseTerms
#' @importFrom igraph vertex_attr as_edgelist gsize delete_vertices components simplify vcount 
#' betweenness closeness set_graph_attr V 'V<-' E 'E<-' degree edge_attr_names vertex_attr_names
#' @importFrom rtweet create_token
#' @importFrom magrittr '%>%' '%<>%'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom syuzhet get_nrc_sentiment
#' @importFrom wordcloud wordcloud
#' @importFrom httr parse_url
#' @importFrom vosonSML Authenticate Collect Create
#' @importFrom utils packageVersion
#' @importFrom graphics plot text title par barplot
#' @importFrom lattice barchart
NULL
