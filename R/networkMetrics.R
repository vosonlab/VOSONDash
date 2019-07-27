#' @title Get graph network metrics
#' 
#' @description Function creates a vector of calculated network metrics for a graph. 
#' 
#' @param g \pkg{igraph} graph object.
#' @param component_type Character string. Use strongly or weakly connected components by specifying \code{"strong"} or 
#' \code{"weak"}. Ignored for undirected graphs. Default is \code{"strong"}.
#' 
#' @return Network metrics as named vector.
#' 
#' @export
getNetworkMetrics <- function(g, component_type = "strong") {
  metrics <- list()
  
  metrics['directed'] <- ifelse(is.directed(g), TRUE, FALSE)
  metrics['nodes'] <- vcount(g)
  metrics['edges'] <- ecount(g)
  metrics['components_type'] <- ifelse(metrics['directed'], component_type, "not used")
  metrics['components'] <- count_components(g, mode = component_type)
  metrics['isolates'] <- length(which(degree(g) == 0))
  metrics['density'] <- graph.density(g)
  metrics['ave_geodesic_dist'] <- mean_distance(g)
  metrics['global_clust_coeff'] <- transitivity(g)
  metrics['reciprocity_def'] <- reciprocity(g, mode = "default")
  metrics['reciprocity_ratio'] <- reciprocity(g, mode = "ratio")
  metrics['degree'] <- centr_degree(g)$centralization
  metrics['indegree'] <- centr_degree(g, mode = "in")$centralization
  metrics['outdegree'] <- centr_degree(g, mode = "out")$centralization
  metrics['betweenness'] <- centr_betw(g)$centralization
  metrics['closeness'] <- suppressWarnings(centr_clo(g)$centralization)

  metrics  
}
