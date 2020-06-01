# GraphDistances

Code to compute **distribution** based distance measures between directed and undirected graphs/networks [1, 2]. 

### Install
``` 
#install.packages("devtools")
devtools::install_github("cds-group/GraphDistances")
```
### Usage

#### Load package
```
library(GraphDistances)
```

#### Distance computation: **Pairwise**
Distance computation between the NDD (node distance distribution) and Transition Probability (TM-one walk) matrices of two example graphs.
```
library(igraph)
num_nodes <- 60
g1 <- make_tree(num_nodes, children =2, mode = "out")
E(g1)$weight <- seq_len(ecount(g1))
g2 <- make_tree(num_nodes, children = 3, mode = "out")
E(g2)$weight <- seq_len(ecount(g2))

# Compute the distances between the node distance distribution of 2 graphs
binList <- getBins(list(g1, g2))
ndd1 <- getNodeDistanceDistr(g1, binList)
ndd2 <- getNodeDistanceDistr(g2, binList)
nddDistancepair <- getGraphpairdistance(ndd1, ndd2)

# Compute the distances between the transition probability matrix (one walk) of 2 graphs
# For n walk matrix change the 'walk' parameter to n
trans1 <- getTransitionmatrix(g1, walk=1)
trans2 <- getTransitionmatrix(g2, walk=1)
transDistancepair <- getGraphpairdistance(trans1, trans2) 
```

#### Distance computation: **Multiple graphs**
nddDistanceMulti and transDistanceMulti are the Gram matrices produced after distance computation between the NDD and TM-one walk matrices of [40 graphs](data/KidneyGraphs.RData) provided with the package. The [annotation](data/annoKidney.RData) of these graphs can be loaded by running data(annoKidney) in R for classification, clustering and visualization tasks.
```
data("KidneyGraphs")

binList <- getBins(KidneyGraphs)
nddList <- lapply(KidneyGraphs, function(x) getNodeDistanceDistr(x, binList))
nddDistanceMulti <- getGraphlistdistance(nddList) #Takes sometime for computation

transList <- lapply(KidneyGraphs, function(x) getTransitionmatrix(x, walk=1))
transDistanceMulti <- getGraphlistdistance(transList) #Takes sometime for computation
```

#### Distance computation: **Multiple graphs: Parallel**
getGraphdistance4PartsParallel can be used to run the distance computation in parallel.
```
library(future.apply)
plan(multiprocess)
binList <- getBins(KidneyGraphs)

nddListPar <- future_lapply(KidneyGraphs, function(x) getNodeDistanceDistr(x, binList))
nddDistanceMultiPar <- getGraphdistance4PartsParallel(nddListPar)

transListPar <- future_lapply(KidneyGraphs, function(x) getTransitionmatrix(x, walk=1))
transDistanceMultiPar <- getGraphdistance4PartsParallel(transListPar)
```

### Cite
[1] Granata, I., Guarracino, M.R., Kalyagin, V.A., Maddalena, L., Manipur, I. and Pardalos, P.M., 2018, December. Supervised classification of metabolic networks. In 2018 IEEE International Conference on Bioinformatics and Biomedicine (BIBM) (pp. 2688-2693). IEEE.
https://ieeexplore.ieee.org/abstract/document/8621500

[2] Granata, I., Guarracino, M.R., Kalyagin, V.A., Maddalena, L., Manipur, I. and Pardalos, P.M., 2020. Model simplification for supervised classification of metabolic networks. Annals of Mathematics and Artificial Intelligence, 88(1), pp.91-104.
https://link.springer.com/article/10.1007/s10472-019-09640-y
