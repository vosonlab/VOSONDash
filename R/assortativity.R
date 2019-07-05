#' Create a mixing matrix
#' 
#' mixing matrix original function written by Gary Weissman
#' see: https://gist.github.com/gweissman/2402741, http://www.babelgraph.org/wp/?p=351
#'
#' @param g graph as graphml object
#' @param attrib vertex attribute
#' @param use.density use edge density
#' 
#' @return mixing matrix
#' 
#' @export
mixmat <- function(g, attrib, use.density = TRUE) {
  # get unique list of characteristics of the attribute
  attlist <- sort(unique(igraph::vertex_attr(g, attrib)))
  
  numatts <- length(attlist)
  
  # build an empty mixing matrix by attribute
  mm <- matrix(nrow = numatts, 
               ncol = numatts, 
               dimnames = list(attlist, attlist))
  
  # calculate edge density for each matrix entry by pairing type
  # lends itself to parallel if available
  el <- igraph::as_edgelist(g, names = FALSE)
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      mm[i, j] <- length(which(apply(el, 1, function(x) {
        igraph::vertex_attr(g, attrib, x[1] ) == attlist[i] &&
          igraph::vertex_attr(g, attrib, x[2] ) == attlist[j] } )))
    }
  }
  
  # convert to proportional mixing matrix if desired (ie by edge density)
  if (use.density) {
    return(mm / igraph::gsize(g))
  }
  
  mm
}