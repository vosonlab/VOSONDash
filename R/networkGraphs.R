#' Filter out graph vertices not in selected voson category attributes
#' 
#' @param g graph as graphml object
#' @param selected_category vertex category as character string 
#' @param selected_category_attr list of vertex category attributs to filter out as character string 
#' 
#' @return g as graphml object
#' @keywords internal
#' 
#' @export
applyCategoricalFilters <- function(g, selected_category, selected_category_attr) {
  
  if (selected_category == "All") {
    return(g)
  }
  
  # remove All from list
  selected_category_attr <- selected_category_attr[selected_category_attr != "All"]
  
  # filter out all vertices that are not in category attribute
  if (length(selected_category_attr) > 0) {
    vattr <- paste0('vosonCA_', selected_category)
    g <- igraph::delete_vertices(g, V(g)[!(vertex_attr(g, vattr) %in% selected_category_attr)])
  }

  g
}

#' Filter out graph vertices not in component size range
#' 
#' @param g as graphml object
#' @param component_type as character string  
#' @param component_range min and max values of range as vector
#' 
#' @return g as graphml object
#' @keywords internal
#' 
#' @export
applyComponentFilter <- function(g, component_type, component_range) {
  min_range <- component_range[1]
  max_range <- component_range[2]
  
  graph_clusters <- igraph::components(g, mode = component_type)
  
  min_cluster_size <- suppressWarnings(min(graph_clusters$csize)) # suppress no non-missing arguments to min;
  max_cluster_size <- suppressWarnings(max(graph_clusters$csize)) # returning Inf warning
  
  filter_nodes_under <- NULL
  filter_nodes_over <- NULL
  rm_nodes <- c()
  
  # remove vertices not part of components in component size range by name
  if (min_range > min_cluster_size) {
    filter_nodes_under <- names(which(table(graph_clusters$membership) < min_range))
    
    if (length(filter_nodes_under) > 0) {
      rm_nodes <- sapply(filter_nodes_under, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (max_range < max_cluster_size) {
    filter_nodes_over <- names(which(table(graph_clusters$membership) > max_range))
    
    if (length(filter_nodes_over) > 0) {
      rm_nodes <- sapply(filter_nodes_over, function(x) append(rm_nodes, names(which(graph_clusters$membership == x))))
    }
  }
  
  if (length(rm_nodes) > 0) {
    rm_nodes <- unlist(rm_nodes)
    g <- igraph::delete_vertices(g, rm_nodes)
  }
  
  g
}

#' Filter out graph vertices and edges from graph object that are isolates, multi edge or edge loops
#' 
#' @param g as graphml object
#' @param isolates include isolate vertices in graph as logical
#' @param multi_edge include multiple edges between vertices in graph as logical
#' @param loops_edge include vertex edge loops in graph as logical
#' 
#' @return g as graphml object
#' @keywords internal
#' 
#' @export
applyGraphFilters <- function(g, isolates, multi_edge, loops_edge) {
  
  # remove multiple edges and self loops
  if (multi_edge == FALSE || loops_edge == FALSE) {
    remove_multiple <- ifelse(multi_edge == FALSE, TRUE, FALSE)
    remove_loops <- ifelse(loops_edge == FALSE, TRUE, FALSE)
    g <- igraph::simplify(g, remove.multiple = remove_multiple, remove.loops = remove_loops)
  }
  
  # remove isolates
  if (isolates == FALSE) {
    g <- igraph::delete_vertices(g, degree(g) == 0)
  }
  
  g
}

#' Add degree, in-degree, out-degree, betweenness and closeness measures to graph as vertex attributes
#' 
#' @param g as graphml object
#'
#' @return g as graphml object
#' @keywords internal
#' 
#' @export
addAdditionalMeasures <- function(g) {
  # add degree
  V(g)$Degree <- igraph::degree(g, mode = "total")
  if (igraph::is_directed(g)) {
    V(g)$Indegree <- igraph::degree(g, mode = "in")
    V(g)$Outdegree <- igraph::degree(g, mode = "out")
  } else {
    V(g)$Indegree <- V(g)$Outdegree <- 0
  }
  
  # add centrality
  if (vcount(g) > 1) {
    V(g)$Betweenness <- as.numeric(sprintf("%.3f", igraph::betweenness(g)))
    # suppress disconnected graph warnings
    V(g)$Closeness <- as.numeric(sprintf("%.3f", suppressWarnings(igraph::closeness(g))))    
  } else {
    V(g)$Betweenness <- V(g)$Closeness <- 0
  }

  g
}
