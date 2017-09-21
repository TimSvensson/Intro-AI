hroads = matrix(c(1:100), nrow= 10, byrow=TRUE)
vroads = matrix(c(1:100), nrow= 10, byrow=FALSE)

org = c(1, 10)
dest = c(1, 7)
neighbours = list(c(1,9), c(1,7))
roads = c(hroads, vroads)
g = c()

getManhattanDistance=function(node, goal) {
  manhat = abs(node[1] - goal[1]) + abs(node[2] - goal[2])
  return (manhat)
}

getHeuristicsAUX = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  aux = getManhattanDistance(org, dest)
  aux = aux * ((mean(roads[1]) + mean(roads[2]))/2)
  return (aux)
}

getHeuristics = function(neighbours, goal, roads){
  #Ta fram heuristic f??r noden
  
  neighbours.h = vector("numeric", length = length(neighbours))
  
  for(i in 1:length(neighbours)) {
    neigh = getHeuristicsAUX(neighbours[i], dest, roads)
    neighbours.h[i] <- neigh
  }
  
  return (neighbours.h)
}

# If x or y value > currentPos then move right or up
# If x or y value < currentPos then move left or down

getManhattanDistance(org, dest)
