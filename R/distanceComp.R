
#' Find the Jensen Shannon divergence between distribution matrices of graphs
#'
#' @param distrib1 Probability distribution matrix of graph 1
#' @param distrib2 Probability distribution matrix of graph 2
#' @param returnnodeDist Return node-wise distances(default=FALSE), if TRUE returns
#' a list containing 2 elements: 1) nodewise distances and 2)graph distance
#' - between the graph pairs
#' @import Matrix
#' @import philentropy
#' @export
#'
#' @examples
#' \dontrun{
#' library(igraph)
#' num_nodes <- 60
#'g1 <- make_tree(num_nodes, children =2, mode = "out")
#'E(g1)$weight <- seq_len(ecount(g1))
#'g2 <- make_tree(num_nodes, children = 3, mode = "out")
#'E(g2)$weight <- seq_len(ecount(g2))
#' binList <- getBins(list(g1, g2))
#' ndd1 <- getNodeDistanceDistr(g1, binList)
#' ndd2 <- getNodeDistanceDistr(g2, binList)
#' getGraphpairdistance(ndd1, ndd2)}
getGraphpairdistance <- function(distrib1, distrib2, returnnodeDist = FALSE){
  # JSD distance between distributions (node-wise)
  Di <- matrix(0, nrow=nrow(distrib1), ncol=1)
  for (i in 1:nrow(distrib1)){
    Di[i] <- suppressWarnings(sqrt(jensen_shannon(distrib1[i, ], distrib2[i, ],
                                                  testNA = TRUE, unit = "log2")))

  }
  Dg <- colMeans(Di, na.rm = TRUE, dims = 1)
  ifelse(returnnodeDist, return(list(Di, Dg)), return(Dg))
}

#' Find the pairwise Jensen Shannon divergence between distribution matrices of
#' a list of graphs
#' @param distribMatlist list of probability distribution matrices
#'
#' @return Gram matrix with pairwise distances between graphs
#' @import slam
#' @export
#' @examples
#' \dontrun{
#' data("KidneyGraphs")
#' binsList <- getBins(KidneyGraphs)
#' nddList <- lapply(KidneyGraphs, function(x) getNodeDistanceDistr(x, binsList))
#' getGraphlistdistance(nddList)}
getGraphlistdistance <- function(distribMatlist, returnnodeDist=FALSE){
  numGraphs <- length(distribMatlist)
  distanceMat <- matrix(nrow = numGraphs, ncol = numGraphs)
  if (returnnodeDist == TRUE){
    forTensdim <- nrow(distribMatlist[[1]])
    distanceTensor <- array(0, dim = c(numGraphs, numGraphs, forTensdim))
    distanceTensor <- slam::as.simple_sparse_array(distanceTensor)
  }
  for (i in 1:numGraphs){
    distrib1 <- distribMatlist[[i]]
    for (j in 1:numGraphs){
      distrib2 <- distribMatlist[[j]]
      distComp <- getGraphpairdistance(distrib1, distrib2, returnnodeDist = returnnodeDist)

      if (returnnodeDist == TRUE){
        distanceMat[i, j] <- distanceMat[j, i] <- distComp[[2]]
        distanceTensor[i, j, ] <- distanceTensor[j, i, ] <- distComp[[1]][,1]
      }else{
        distanceMat[i, j] <- distanceMat[j, i] <- distComp
      }
    }
  }

  ifelse(returnnodeDist, return(list(distanceTensor, distanceMat)),
         return(distanceMat))
}

#' getGraphlistdistance4Parts
#'
#' @param distribMatlist list of probability distribution matrices
#' @param index possible values 1-4
#' @description used when there are numerous graphs, the graph samples can be
#' divided into four as specified by the index. Run in 4 separate R sessions.
#'Save the 4 matrices and reassemble the gram matrix
#' @return Gram matrix with Pairwise distances
#' @import slam
#' @export
#'
getGraphlistdistance4Parts <- function(distribMatlist, index = 1,
                                       returnnodeDist = FALSE){

  n <- length(distribMatlist)
  nby2 <- n/2
  distanceMat <- matrix(nrow = n, ncol = n)
  if (returnnodeDist == TRUE){
    forTensdim <- nrow(distribMatlist[[1]])
    distanceTensor <- array(0, dim = c(numGraphs, numGraphs, forTensdim))
    distanceTensor <- slam::as.simple_sparse_array(distanceTensor)
  }
  if (index==1){
    for (i in 1:(ceil(nby2)-1)){
      for (j in (i+1):(ceil(nby2))){
        distComp <- getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]]
                                         , returnnodeDist = returnnodeDist)
        if (returnnodeDist == TRUE){
          distanceTensor[i, j, ] <- distanceTensor[j, i, ] <- distComp[[1]][,1]
          distanceMat[i, j] <- distanceMat[j, i] <- distComp[[2]]
        }else{
          distanceMat[i, j] <- distanceMat[j, i] <- distComp
        }
      }
      print(i)
    }
  }
  if (index==2){
    for (i in (floor(nby2)+1):(n-1)){
      for (j in (i+1):n){
        distComp <- getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]]
                                         , returnnodeDist = returnnodeDist)
        if (returnnodeDist == TRUE){
          distanceTensor[i, j, ] <- distanceTensor[j, i, ] <- distComp[[1]][,1]
          distanceMat[i, j] <- distanceMat[j, i] <- distComp[[2]]
        }else{
          distanceMat[i, j] <- distanceMat[j, i] <- distComp
        }
      }
      print(i)
    }
  }
  if (index==3){
    for (i in 1:(ceil(nby2)-1)){
      for (j in (i+1+floor(nby2)):n){
        distComp <- getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]]
                                         , returnnodeDist = returnnodeDist)
        if (returnnodeDist == TRUE){
          distanceTensor[i, j, ] <- distanceTensor[j, i, ] <- distComp[[1]][,1]
          distanceMat[i, j] <- distanceMat[j, i] <- distComp[[2]]
        }else{
          distanceMat[i, j] <- distanceMat[j, i] <- distComp
        }
      }
      print(i)
    }
  }

  if (index==4){
    for (j in (ceil(nby2)+1):n){
      for (i in (j-(floor(nby2))):(nby2)){
        distComp <- getGraphpairdistance(distribMatlist[[j]], distribMatlist[[i]]
                                         , returnnodeDist = returnnodeDist)
        if (returnnodeDist == TRUE){
          distanceTensor[i, j, ] <- distanceTensor[j, i, ] <- distComp[[1]][,1]
          distanceMat[i, j] <- distanceMat[j, i] <- distComp[[2]]
        }else{
          distanceMat[i, j] <- distanceMat[j, i] <- distComp
        }
      }
      print(j)
    }
  }
  ifelse(returnnodeDist, return(list(distanceTensor, distanceMat)),
         return(distanceMat))
}
