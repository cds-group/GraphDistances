#' Extract subnetworks using Eigen centrality scores of nodes
#'
#' @description A subnetwork is extracted by choosing top eigen central nodes
#' (based on the supplied cut-off) and also the nodes adjacent to them.
#' @param g graph in igraph format
#' @param eigenSortcutoff cutoff based on eigen centrality score (values >0 and
#'  <1) - default = 0.1
#' @param percentTotalnodes cutoff based on percentage of top eigen central
#' ranked nodes to be selected
#'
#' @return subnetwork
#' @export
#' @import igraph
#' @examples
#' \dontrun{
#' data(KidneyGraphs)
#' g <- KidneyGraphs[[1]]
#' getSubnetworksEigenadjacent(g, eigenSortcutoff = 0.1)}
getSubnetworksEigenadjacent <- function(g, eigenSortcutoff = 0.1,
                                        percentTotalnodes = NA){
  if (is_directed(g)){
    g_u <- as.undirected(g)
  } else{g_u <- g}
  # Find eigen centrality of all vertices and sort------------------------------
  graphCentrality <- eigen_centrality(g_u, weights = NA, directed = FALSE)
  graphCentralitySort <- data.frame(sorted_centrality = sort(
    (graphCentrality$vector), decreasing = TRUE))
  #-----------------------------------------------------------------------------

  # Find vertices with eigen centrality values > cutoff and their adjacent------
  # verticeS
  if (!is.na(percentTotalnodes)){
    nodestoKeep <- ceil(percentTotalnodes*vcount(g_u)/100)
  } else{
    nodestoKeep <- length(which(graphCentralitySort$sorted_centrality >
                                  eigenSortcutoff))
  }
  adj_vert <- adjacent_vertices(g_u, rownames(graphCentralitySort)
                                [1:nodestoKeep], mode = "all")
  topNodeswithNeighbors <- unlist(adj_vert)
  #-----------------------------------------------------------------------------

  # Keep top eigen nodes and their first degree neighbors
  g_out <- delete_vertices(g_u, V(g_u)[-topNodeswithNeighbors])

  if (is_directed(g)){
    keepList <- (vertex.attributes(g_out)$name)
    keepListnumeric <- as.numeric(V(g)[keepList])
    g_out <- delete_vertices(g, V(g)[-keepListnumeric])
  }

  return(g_out)
}
