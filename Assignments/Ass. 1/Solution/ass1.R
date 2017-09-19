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