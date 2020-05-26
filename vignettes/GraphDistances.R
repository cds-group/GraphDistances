## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  devtools::install_github("cds-group/GraphDistances")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  library(GraphDistances)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  library(igraph)
#  num_nodes <- 60
#  g1 <- make_tree(num_nodes, children =2, mode = "out")
#  E(g1)$weight <- seq_len(ecount(g1))
#  g2 <- make_tree(num_nodes, children = 3, mode = "out")
#  E(g2)$weight <- seq_len(ecount(g2))
#  
#  # Compute the distances between the node distance distribution of 2 graphs
#  binList <- getBins(list(g1, g2))
#  ndd1 <- getNodeDistanceDistr(g1, binList)
#  ndd2 <- getNodeDistanceDistr(g2, binList)
#  nddDistancepair <- getGraphpairdistance(ndd1, ndd2)
#  
#  # Compute the distances between the transition probability matrix (one walk) of 2 graphs
#  trans1 <- getTransitionmatrix(g1, walk=1)
#  trans2 <- getTransitionmatrix(g2, walk=1)
#  transDistancepair <- getGraphpairdistance(trans1, trans2)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  data("KidneyGraphs")
#  
#  binsList <- getBins(KidneyGraphs)
#  nddList <- lapply(KidneyGraphs, function(x) getNodeDistanceDistr(x, binsList))
#  nddDistanceMulti <- getGraphlistdistance(nddList) #Takes sometime for computation
#  
#  transList <- lapply(KidneyGraphs, function(x) getTransitionmatrix(x, walk=1))
#  transDistanceMulti <- getGraphlistdistance(transList) #Takes sometime for computation

