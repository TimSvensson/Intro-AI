myCar=function(roads, car, packages) {

    # roads={matrix(hroads)|matrix(vroads)}
    # car=list(position(x,y),wait=0,load=0,nextMove=NA,mem=list())
    # Package={source(x,y)|destination(x,y)|status(i)}
    
    
    
    #print("Roads:")
    #print(roads)
    
    # If there is no path in mem
    if (length(car$mem) == 0) {
        # calculate optimal path and store it in mem
        car$mem = getRoute(packages, car)
    }
    
    carPos = getCarPosition(car)
    destPos = getCarDestination(car)
    
    # if a node stored in mem is reached, remove it
    if (carPos[1] == destPos[1] & carPos[2] == destPos[2]) {
        car$mem = getRoute(packages, car)
        destPos = getCarDestination(car)
    }
    
    # find optimal path to next node in mem
    car$nextMove = getNextMove(carPos, destPos, roads)
    
    print("Packages:")
    print(packages)
    
    print("Car:")
    print(car)
    
    return(car)
}

getCarDestination=function(car) {
    mem = c(car$mem[1], car$mem[2])
    
}

getCarPosition=function(car) {
    return (c(car$x, car$y))
}

getNextMove=function(carPos, destPos, roads) {
  print("CARPOS")  
  print(carPos)
    direction = list(down=2, left=4, right=6, up=8, stay=5)
    
    if (carPos[1] < destPos[1]) {
        nextMove = direction$right
    } else if (carPos[1] > destPos[1]) {
        nextMove =  direction$left
    } else if (carPos[2] < destPos[2]) {
        nextMove = direction$up
    } else if (carPos[2] > destPos[2]) {
        nextMove = direction$down
    } else {
        nextMove = direction$stay
    }
    
    return(nextMove)
}

getEdgeCost = function(neighbours, currPos) {
  
  
}

getNeighbors = function(currPos, roads) {
  rows = nrow(roads$vroads)
  if (currPos[1] == 1) {
    if( currPos[2] == 1 ) {
      # Anropa getEdgeCost() och getHeuristics() f??r listan
      neighbours = list(c(1,2), c(2,1))
      bestEdge = getEdgeCost(neighbours, currPos)
      bestHeuristic = getHeuristics(neighbours)
      return (list(c(1,2), c(2,1)))
    } else if (currPos[2] == rows) {
      return(list(c(1,rows-1), c(2,rows)))
    } else {
      return(list(c(currPos[1]+1, currPos[2]), c(currPos[1], currPos[2]+1), c(currPos[1], currPos[2]-1)))
    }
  }
  if (currPos[1] == rows) {
    if( currPos[2] == 1 ) {
      return (list(c(rows-1, 1), c(rows,2)))
    } else if (currPos[2] == rows) {
      return(list(c(rows,rows-1), c(rows)))
    } else {
      return(list(c(currPos[1]-1, currPos[2]), c(currPos[1], currPos[2]+1), c(currPos[1], currPos[2]-1)))
    }
  }
  if (currPos[2] == 1){
    return(list(c(currPos[1]+1, currPos[2]), c(currPos[1]-1, currPos[2]), c(currPos[1], currPos[2]+1)))
  }  
  if (currPos[2] == rows) {
    return(list(c(currPos[1]+1, currPos[2]), c(currPos[1]-1, currPos[2]), c(currPos[1], currPos[2]-1)))
  }

  return(list(c(currPos[1]+1, currPos[2]), c(currPos[1]-1, currPos[2]), c(currPos[1], currPos[2]-1), c(currPos[1], currPos[2]+1) ))
  
}

#
#   package end
#

#
#   frontier
#

#
#   frontier
#   
#   The frontier matrix is an ordered set of nodes where the top node (the first
#   row in the matrix) is the next best step in the A* algorithm.
#   
#   Example:
#   
#           [,1]    [,2]    [,3]    [,4]    [,5]
#   [1,]    a1      b1      c1      d1      f1
#   [2,]    a2      b2      c2      d2      f2
#   [3,]    ...
#
#   Where
#       (aN, bN) is the current node in question,
#       (cN, dN) is the node that has the cheapest path to the current node, and
#       fN is the A* cost related to the node.
#       
#   The matrix will always be ordered after the value of f, the lowest value of f
#   will be at the top of the matrix.
#

#   frontier.get
#
#   frontier    - A frontier matrix, the old frontier.
#   nodes       - A frontier matrix, new nodes to be added to the frontier.
#   return      - A frontier matrix, the new frontier with the nodes matrix added.
#
frontier.get = function(frontier.old, nodes) {
  
  # 1. find and resolve doubles
  for (i in nrow(frontier.old):1) {
    for (j in nrow(nodes):1) {
      if (frontier.node.current.equals(frontier.old[i,], nodes[j,])) {
        if (frontier.old[i,5] > nodes[j,5]) {
          frontier.old[i,] = c(0,0,0,0,-1)
        } else {
          nodes[j,] = c(0,0,0,0,-1)
        }
        break;
      }
    }
  }
  
  print("frontier.old")
  print(frontier.old)
  
  print("nodes")
  print(nodes)
  
  frontier.old = frontier.remove.zeroRows(frontier.old)
  nodes = frontier.remove.zeroRows(nodes)
  
  print("frontier.old")
  print(frontier.old)
  
  print("nodes")
  print(nodes)
  
  # 2. create new frontier matrix big enough for all nodes
  
  
  # 3. fill and sort the new frontier matrix with all nodes
  
  #return (frontier.new)
}

frontier.node.current.equals = function(a, b) {
  return(
    a[1] == b[1] & a[2] == b[2]
  )
}

frontier.remove.zeroRows = function(m) {
  for (i in NROW(m):1) {
    print("m")
    print(m)
    if (frontier.is.zeroRow(m[1,])) {
      m=m[-i,,drop=FALSE]
    }
  }
  return(m)
}

frontier.is.zeroRow = function(row) {
  
  row.zero = c(0,0,0,0,-1)
  
  if (identical(row, row.zero)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

frontier.test = function() {
  frontier.old = matrix(1:20,ncol=5,byrow=TRUE)
  nodes = matrix(c(1,2,5,6,2,
                   6,7,1,2,10,
                   2,3,4,5,5),
                 ncol=5,
                 byrow=TRUE)
  
  print("frontier.old")
  print(frontier.old)
  
  print("nodes")
  print(nodes)
  
  frontier.new = frontier.get(frontier.old, nodes)
  
  print("frontier.new")
  print(frontier.new)
}

#
#   frontier end
#

#Creates the visited matrix
visited.create = function(){
  visited.h = matrix(0, nrow = 100, ncol = 5)
}

#Adds a new element
visited.add = function(matrix, visited.h){
  rbing(matrix, visited.h)
  return (matix)
}

#The function
visited = function(matrix){
  visited.h = vector("numeric", length = 100)
  visited.h = nrow(matrix)
  node.visited = visited.h[0]
  visited.clean(matrix, visited.h)
  return (node.visited)
}

#Cleans the matrix
visited.clean = function(matrix, visited.h){
  visited.h = NULL
  matrix = NULL
}

getNonDeliveredPackages=function(packages) {
    rVal = packages
    for (i in 1:nrow(packages)) {
        if (rVal[i,5]!=0) {
            rVal=matrix(rVal[-i,], ncol=nCol(rVal), drop=FALSE)
        }
    }
    return(rVal)
}

packages.pickedUp = function(pkgs) {
    for (i in 1:nrow(pkgs)) {
        if (pkgs[i,5] == 1) {
            return (i)
        }
    }
    return (0)
}

pkgs.toPickUp = function(pkgs) {
    return(pkgs[pkgs[,5]==0,
                ,
                drop=FALSE])
}

getRoute=function(pkgs, car) {
    
    car.pos = c(car$x, car$y)
    pkg.pu = packages.pickedUp(pkgs)
    
    if (pkg.pu > 0) {
        return (c(pkgs[pkg.pu, 3],
                  pkgs[pkg.pu, 4]))
    }
    
    pkgs = pkgs.toPickUp(pkgs)
    
    pkgs.distance = vector("numeric", length=nrow(pkgs))
    
    for (i in 1:nrow(pkgs)) {
        pkg.pos = c(pkgs[i,1],
                    pkgs[i,2])
        pkgs.distance[i] = getManhattanDistance(car.pos, pkg.pos)
    }
    
    pkgs.min = which.min(pkgs.distance)
    return(c(pkgs[pkgs.min, 1],
             pkgs[pkgs.min, 2]))
}

getIndexOfClosestNode=function(pickUps, origin) {
    
    print("pick ups")
    print(pickUps)
    print("origin")
    print(origin)
    
    bestPkg = -1
    
    if (is.vector(pickUps) == TRUE) return(1)
    
    for (crntPkg in 1:nrow(as.matrix(pickUps))) {
        if (bestPkg == -1) {
            bestPkg = crntPkg
        } else {
            
            crntPkgDistance = getManhattanDistance(origin, pickUps[crntPkg,])
            bestPkgDistance = getManhattanDistance(origin, pickUps[bestPkg])
            
            if (crntPkgDistance < bestPkgDistance) {
                bestPkg = crntPkg
            }
        }
    }
    return (bestPkg)
}

getManhattanDistance=function(origin, destination) {
    return ( abs(origin[1] - destination[1]) + abs(origin[2] - origin[2]) )
}

getRouteDistance=function() {}

getHeuristicsAUX = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  return (getManhattanDistance(node, goal)* mean(roads))
}

getHeuristics = function(neighbours, goal, roads){
  #Ta fram heuristic f??r noden

  neighbours.h = vector("numeric", length = length(neighbours))
  
  for(i in 1:lenght(neighbours)) {
    neighbours.h[i] = (getHeuristicsAUX(neighbours[i], goal, roads))
  }
  
  return (neighbours.h)
}
#
# Program start
#
runDeliveryMan(myCar)