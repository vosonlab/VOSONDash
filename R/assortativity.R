#' @title Create a mixing matrix
#' 
#' @description Function creates a mixing matrix by graph vertex attribute. 
#' 
#' @note Mixing matrix original function written by Gary Weissman. See: https://gist.github.com/gweissman/2402741.
#'
#' @param g \pkg{igraph} graph object.
#' @param attrib Character string. Vertex attribute or category.
#' @param use_density Logical. Use edge density. Default is \code{TRUE}.
#' 
#' @return A mixing matrix.
#'
#' @examples
#' \dontrun{
#' # create a mixing matrix of the demonstration network based on vertex 
#' # categorical attribute for political stance "vosonCA_Stance"
#' g <- loadPackageGraph("DividedTheyBlog_40Alist_release.graphml")
#' 
#' mm <- mixmat(g, "vosonCA_Stance", use_density = FALSE)
#' }
#' 
#' @export
mixmat <- function(g, attrib, use_density = TRUE) {
  # get unique list of characteristics of the attribute
  attlist <- sort(unique(igraph::vertex_attr(g, attrib)))
  
  numatts <- length(attlist)
  
  # mixing matrix by attribute
  mm <- matrix(nrow = numatts, 
               ncol = numatts, 
               dimnames = list(attlist, attlist))
  
  el <- igraph::as_edgelist(g, names = FALSE)
  for (i in 1:numatts) {
    for (j in 1:numatts) {
      mm[i, j] <- length(which(apply(el, 1, function(x) {
        igraph::vertex_attr(g, attrib, x[1] ) == attlist[i] &&
          igraph::vertex_attr(g, attrib, x[2] ) == attlist[j] } )))
    }
  }
  
  # use density or raw number of edges
  if (use_density) {
    return(mm / igraph::gsize(g))
  }
  
  mm
}
