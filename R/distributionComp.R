#' Bins for the node distance distribution (NDD)
#'
#'getBins finds the maximum diameter from the given graph list and returns the
#'bins needed for the  getNodeDistanceDistr function
#'
#' @param gList List of graphs in igraph format
#'
#' @return binList
#' @importFrom pracma ceil
#' @export
#' @examples
#' \dontrun{
#' data(KidneyGraphs)
#' binsG <- getBins(KidneyGraphs)}
getBins <- function(gList){
  # For a given list of graphs keep the bins the same, so that the NDD matrices
  # are all of the same size for subsequent comparison
  maxDiam <- max(sapply(gList, function(x) diameter(x)))
  binList <- c(0:ceil(maxDiam), Inf)
  return(binList)
}

#' Node Distance Distribution computation
#'
#' @param g graph in igraph format
#' @param binList bins for the distance distribution
#'
#' @return Node distance distribution
#' @import igraph
#' @importFrom pracma histc
#' @importFrom methods as
#' @import Matrix
#' @export
#'
#' @examples
#' \dontrun{
#' g <- igraph::make_tree(10, mode = "out")
#' getNodeDistanceDistr(g)}
getNodeDistanceDistr <- function(g, binList=NA){

  if (any(is.na(binList))) { binList <- c(0:diameter(g), Inf) }
  ifelse(is_directed(g), mode_g <- "out", mode_g <- "all")
  numNodes <- vcount(g)

  #Find nodewise distances and their histogram
  d <- distances(g, mode = mode_g)
  h_g <- t(sapply(1:numNodes, function(x) histc(d[x,], binList)$cnt))
  h_g[,1] <- h_g[,1] - 1

  #Normalize to obtain NDD
  distribMat <- h_g/(numNodes - 1)
  return(as(distribMat, "sparseMatrix"))

}

#' Transition Matrix computation
#'
#' @param g graph in igraph format
#' @param walk random walk distance (default=1)
#'
#' @return Transition matrix of specified walk
#' @import igraph
#' @import Matrix
#' @importFrom methods as
#' @export
#'
#' @examples
#' \dontrun{
#' g <- igraph::make_tree(10, mode = "out")
#' # calculate one walk transition probability matrix
#' getTransitionmatrix(g, walk = 1)}
getTransitionmatrix <- function(g, walk=1){

  if (is_directed(g)){
    mode_g <- "out"
  } else{mode_g <- "all"}

  if (walk==1){
    # find 1 walk transition matrix
    if (is_weighted(g) == TRUE){
      adj_g <- as_adjacency_matrix(g, sparse = TRUE, attr="weight")
    }else{
      adj_g <- as_adjacency_matrix(g, sparse = TRUE)
    }
    dw  <- rowSums(adj_g)
    dw[dw == 0] <- 1

    # Normalize adjacency matrix to get the one walk transition matrix
    distribMat <- adj_g/dw
  } else {
    # find Transition matrices > 1 walk
    d <- distances(g, mode = mode_g)
    ego_out <- ego(g, order = walk, nodes = V(g), mindist = walk, mode = mode_g)
    numNodes <- vcount(g)
    walk_distances <- matrix(0, nrow = numNodes, ncol = numNodes)

    # find vertices for the specified walk parameter by indexing the distances
    # matrix by using the ego indices (ego_out)
    for (i in 1:numNodes){
      walk_distances[i, ego_out[[i]]] <- d[i, ego_out[[i]]]
    }
    dw <- rowSums(walk_distances, na.rm = TRUE)
    dw[dw==0] <- 1
    # Normalize adjacency matrix to get the specified walk transition matrix
    distribMat <- as(walk_distances/dw, "sparseMatrix")
  }
  return(distribMat)

}
