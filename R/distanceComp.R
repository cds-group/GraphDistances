
#' Find the Jensen Shannon divergence between distribution matrices of graphs
#'
#' @param distrib1 Probability distribution matrix of graph 1
#' @param distrib2 Probability distribution matrix of graph 2
#'
#' @import Matrix
#' @import philentropy
#' @export
#'
#' @examples
#' \dontrun{
#' g1 <- make_tree(10, mode = "out")
#' g2 <- make_tree(10, children = 3, mode = "out")
#' getGraphpairdistance(g1, g2)}
getGraphpairdistance <- function(distrib1, distrib2){
  # JSD distance between distributions (node-wise)
  Di <- as.data.frame(matrix(ncol=nrow(distrib1), nrow=1))
  for (i in 1:ncol(distrib1)){
    Di[i] <- suppressWarnings(sqrt(jensen_shannon(distrib1[i, ], distrib2[i, ],
                                  testNA = TRUE, unit = "log2")))

  }
  Dg <- rowMeans(Di, na.rm = TRUE, dims = 1)
  return(Dg)
}

#' Find the pairwise Jensen Shannon divergence between distribution matrices of
#' a list of graphs
#'
#' @param distribMatlist list of probability distribution matrices
#'
#' @return Gram matrix with pairwise distances between graphs
#' @export
#'
getGraphlistdistance <- function(distribMatlist){
  numGraphs <- length(distribMatlist)
  distanceMat <- matrix(nrow = numGraphs, ncol = numGraphs)

  for (i in 1:numGraphs){
    distrib1 <- distribMatlist[[i]]
  for (j in 1:numGraphs){
    distrib2 <- distribMatlist[[j]]
    distanceMat[i, j] <- distanceMat[j, i] <-
      getGraphpairdistance(distrib1, distrib2)
    }
  }
  return(distanceMat)
}

#' getGraphlistdistance4Parts
#'
#' @param distribMatlist list of probability distribution matrices
#' @param index possible values 1-4
#' @description used when there are numerous graphs, the graph samples can be
#' divided into four as specified by the index. Run in 4 separate R sessions.
#'Save the 4 matrices and reassemble the gram matrix
#' @return Gram matrix with Pairwise distances
#' @export
#'
getGraphlistdistance4Parts <- function(distribMatlist, index = 1){

  n <- length(distribMatlist)
  nby2 <- n/2
  distanceMat <- matrix(nrow = n, ncol = n)
  if (index==1){
    for (i in 1:(ceil(nby2)-1)){
      for (j in (i+1):(ceil(nby2))){
        distanceMat[i, j] <- distanceMat[j, i] <-
          getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]])
      }
      print(i)
    }
  }
  if (index==2){
    for (i in (floor(nby2)+1):(n-1)){
      for (j in (i+1):n){
        distanceMat[i, j] <- distanceMat[j, i] <-
          getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]])
      }
      print(i)
    }
  }
  if (index==3){
    for (i in 1:(ceil(nby2)-1)){
      for (j in (i+1+floor(nby2)):n){
        distanceMat[i, j] <- distanceMat[j, i] <-
          getGraphpairdistance(distribMatlist[[i]], distribMatlist[[j]])
      }
      print(i)
    }
  }

  if (index==4){
    for (j in (ceil(nby2)+1):n){
      for (i in (j-(floor(nby2))):(nby2)){
        distanceMat[i, j] <- distanceMat[j, i] <-
          getGraphpairdistance(distribMatlist[[j]], distribMatlist[[i]])
      }
      print(j)
    }
  }
  return(distanceMat)

}
