#' @title Filter out graph vertices not in component size range
#' 
#' @description This function removes any graph vertices that are in components that fall outside of the specified 
#' component size range.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param component_type Character string. Use strongly or weakly connected components by specifying \code{"strong"} or 
#' \code{"weak"}. Ignored for undirected graphs. Default is \code{"strong"}.
#' @param component_range Numeric vector. Min and max values or size range of component.
#' 
#' @return An igraph graph object.
#' 
#' @export
applyComponentFilter <- function(g, component_type = "strong", component_range) {
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

#' @title Filter out graph vertices and edges from graph object that are isolates, multi edge or edge loops
#' 
#' @description This function removes isolate vertices, multiple edges between vertices and or vertex edge loops from a 
#' graph.
#' 
#' @note Removing multiple edges or edge loops from a graph will simplify it and remove other edge attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param isolates Logical. Include isolate vertices in graph. Default is \code{TRUE}.
#' @param multi_edge Logical. Include multiple edges between vertices in graph. Default is \code{TRUE}.
#' @param loops_edge Logical. Include vertex edge loops in graph. Default is \code{TRUE}.
#' 
#' @return An igraph graph object.
#' 
#' @export
applyGraphFilters <- function(g, isolates = TRUE, multi_edge = TRUE, loops_edge = TRUE) {
  
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

#' @title Add additional measures to graph as vertex attributes
#' 
#' @description Adds degree, in-degree, out-degree, betweenness and closeness measures to graph as vertex attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#'
#' @return An igraph graph object.
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

#' @title Get a list of vertex category attribute names and values
#' 
#' @description This function returns a list of graph vertex attribute names that match a category attribute prefix 
#' format and their unique values.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param cat_prefix Character string. Category attribute prefix format to match. Default is \code{"vosonCA_"}.
#' 
#' @return A named list of vertex category attributes and values.
#' 
#' @examples
#' \dontrun{
#' # get a list of voson vertex categories and values
#' g <- loadPackageGraph("DividedTheyBlog_40Alist_release.graphml")
#' 
#' vcats <- getVertexCategories(g)
#' 
#' # vcats
#' # $Stance
#' # [1] "conservative" "liberal"  
#' }
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

#' @title Check if graph object has text attributes
#' 
#' @description This function checks if a graph has either vertex or edge text attributes.
#' 
#' @note Uses the \code{VOSON} vertex and edge text attribute prefix \code{"vosonTxt_"} to determine if attributes are
#' text attributes.
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' 
#' @return Result as logical.
#' 
#' @keywords internal
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

#' @title Filter out graph vertices not in selected category
#' 
#' @description This function removes vertices that are not in the selected categories values list or sub-categories.   
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param selected_cat Character string. Selected vertex category without prefix.
#' @param selected_subcats List. Selected sub-category values to include in graph. 
#' @param cat_prefix Character string. Category attribute prefix format to match. Default is \code{"vosonCA_"}.
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # return a graph containing only vertices that have the vertex category 
#' # attribute "vosonCA_Stance" value "liberal"
#' g <- loadPackageGraph("DividedTheyBlog_40Alist_release.graphml")
#' 
#' g <- applyCategoricalFilters(g, "Stance", c("liberal"))
#' }
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

#' @title Prune vertices from graph by vertex id
#'
#' @description This function removes a list of vertices from the graph object by vertex id value. 
#' 
#' @param g \pkg{igraph} \code{graph} object.
#' @param selected_prune_verts List. Selected vertex ids to remove.
#' 
#' @return An igraph graph object.
#' 
#' @export
applyPruneFilter <- function(g, selected_prune_verts) {
  if (length(selected_prune_verts) > 0) {
    verts <- which(V(g)$id %in% selected_prune_verts)
    g <- igraph::delete.vertices(g, verts)
  }
  
  g
}

#' @title Load package included network graph
#'
#' @description This function loads a network graph included in the \code{extdata} directory of the 
#' \code{VOSONDash} package by file name. 
#' 
#' @param fname Character string. Name of demonstration \code{graphml} file.
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Divided They Blog" package included network graph by file name
#' g <- loadPackageGraph("DividedTheyBlog_40Alist_release.graphml")
#' }
#' 
#' @export
loadPackageGraph <- function(fname) {
  tryCatch({
    f <- system.file("extdata", fname, package = "VOSONDash", mustWork = TRUE)
    g <- igraph::read_graph(f, format = c('graphml'))  
  }, error = function(e) {
    stop(e)
  })
  
  g
}

#' @title Load the package included "Divided They Blog" network graph
#'
#' @description This is a convenience function to load the "DividedTheyBlog_40Alist_release.graphml" graph. 
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Divided They Blog" network graph
#' g <- dtbGraph()
#' }
#' 
#' @keywords internal
#' @export
dtbGraph <- function() {
  loadPackageGraph("DividedTheyBlog_40Alist_release.graphml")
}

#' @title Load the package included "Enviro Activist Websites 2006" network graph
#'
#' @description This is a convenience function to load the "enviroActivistWebsites_2006.graphml" graph. 
#' 
#' @return An igraph graph object.
#' 
#' @examples
#' \dontrun{
#' # load the "Enviro Activist Websites 2006" network graph
#' g <- eawGraph()
#' }
#' 
#' @keywords internal
#' @export
eawGraph <- function() {
  loadPackageGraph("enviroActivistWebsites_2006.graphml")
}
