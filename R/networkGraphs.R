#' Filter out graph vertices not in component size range
#' 
#' @param g as graphml object
#' @param component_type as character string
#' @param component_range min and max values of range as vector
#' 
#' @return g as graphml object
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
    V(g)$Betweenness <- as.numeric(sprintf(fmt = "%#.3f", igraph::betweenness(g)))
    # suppress disconnected graph warnings
    V(g)$Closeness <- as.numeric(sprintf(fmt = "%#.3f", suppressWarnings(igraph::closeness(g))))    
  } else {
    V(g)$Betweenness <- V(g)$Closeness <- 0
  }

  g
}

#' Get a list of vertex attribute names that match a category attribute prefix format
#' 
#' @param g as graphml object
#' @param cat_prefix character string of category attribute prefix format to match
#' 
#' @return graph_cats
#' 
#' @export
getVertexCategories <- function(g, cat_prefix = "vosonCA_") {
  graph_cats <- list()

  attr_v <- igraph::vertex_attr_names(g)
  attr_v <- attr_v[grep(paste0("^", cat_prefix), attr_v, perl = TRUE)]
  
  if (length(attr_v)) {
    for (i in attr_v) {
      graph_cats[[sapply(strsplit(i, "_"), `[`, 2)]] <- sort(unique(vertex_attr(g, i)))
    }
  }
  
  graph_cats
}

#' Check if graph object has vertex or edge voson text attributes
#' 
#' @param g as graphml graph object
#' 
#' @return has_text as logical
#' @keywords internal
#' 
#' @export
hasVosonTextData <- function(g) {
  attr_v <- vertex_attr_names(g)
  attr_v <- attr_v[grep("^vosonTxt_", attr_v, perl = TRUE)]
  attr_e <- edge_attr_names(g)
  attr_e <- attr_e[grep("^vosonTxt_", attr_e, perl = TRUE)]
  
  has_text <- FALSE
  if (length(attr_v)) {
    attr <- c(attr_v[1], 'vertex')
    has_text <- TRUE
  } else if (length(attr_e)) {
    i <- attr_e[1]
    attr <- c(attr_e[1], 'edge')
    has_text <- TRUE
  }
  
  has_text
}

#' Filter out graph vertices not in selected category sub-categories
#' 
#' @param g graph as graphml object
#' @param selected_cat selected vertex category without prefix as character string
#' @param selected_subcats list of selected sub-category values to filter as character string 
#' @param cat_prefix character string of category attribute prefix format to match
#' 
#' @return g as graphml object
#' 
#' @export
applyCategoricalFilters <- function(g, selected_cat, selected_subcats, cat_prefix = "vosonCA_") {
  
  if (selected_cat == "All") {
    return(g)
  }
  
  # re-create category vertex attribute name
  vattr <- paste0(cat_prefix, selected_cat)
  
  # remove All from sub-categories list
  selected_subcats <- selected_subcats[selected_subcats != "All"]
  
  # filter out all vertices that do not have a category value in sub-categories list
  if (length(selected_subcats) > 0) {
    g <- igraph::delete_vertices(g, V(g)[!(igraph::vertex_attr(g, vattr) %in% selected_subcats)])
  }
  
  g
}

#' Filter out list of vertices from graphml object using vertex id value
#'
#' @param g graph as graphml object
#' @param selected_prune_verts selected vertex ids to filter out as list
#' 
#' @return g as graphml object
#' 
#' @export
applyPruneFilterX <- function(g, selected_prune_verts) {
  if (length(selected_prune_verts) > 0) {
    verts <- which(V(g)$id %in% selected_prune_verts)
    g <- igraph::delete.vertices(g, verts)
  }
  
  g
}

