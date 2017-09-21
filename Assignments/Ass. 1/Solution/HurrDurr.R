hroads = matrix(c(1:100), nrow= 10, byrow=TRUE)
vroads = matrix(c(1:100), nrow= 10, byrow=FALSE)

node = c(1, 8)
goal = c(1, 7)
neighbours = list(c(1,9), c(1,7))
roads = c(hroads, vroads)
g = c()

getManhattanDistance=function(node, goal) {
  print("Manhattan")
  node = c(1, 8)
  goal = c(1, 7)
  
  print("Goal")
  print(goal)
  print("Node")
  print(node)
  manhat = abs(node[1] - goal[1]) + abs(node[2] - goal[2])
  print(manhat)
  return (manhat)
}

getHeuristicsAUX = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  print("AUX")
  aux = getManhattanDistance(node, goal)
  aux = aux * ((mean(roads[1]) + mean(roads[2]))/2)
  print(aux)
  return (aux)
}

getHeuristics = function(neighbours, goal, roads){
  #Ta fram heuristic f??r noden
  
  neighbours.h = c("numeric", length = length(neighbours))
  
  for(i in 1:length(neighbours)) {
    neigh = getHeuristicsAUX(neighbours[i], goal, roads)
    neighbours.h[i] <- neigh
  }
  
  print("Heuristic")
  print(neighbours.h)
  return (neighbours.h)
}

# If x or y value > currentPos then move right or up
# If x or y value < currentPos then move left or down

