myCar=function(roads, car, packages) {

    # roads={matrix(hroads)|matrix(vroads)}
    # car=list(position(x,y),wait=0,load=0,nextMove=NA,mem=list())
    # Package={source(x,y)|destination(x,y)|status(i)}
    
    # If there is no path in mem
    if (length(car$mem) == 0) {
        # calculate optimal path and store it in mem
        car$mem = car.destination.set(packages, car)
    }
    
    car.pos = car.position.get(car)
    dest.pos = car.destination.get(car)
    
    # if a node stored in mem is reached, remove it
    if ((car.pos[1] == dest.pos[1] & car.pos[2] == dest.pos[2])) {
        car$mem = car.destination.set(packages, car)
        dest.pos = car.destination.get(car)
    }
    
    # find optimal path to next node in mem
    car$nextMove = getNextMove(car.pos, dest.pos, roads)
    
    print("Packages:")
    print(packages)
    
    print("Car:")
    print(car)
    
    # print("Roads:")
    # print(roads)
    
    return(car)
}

#
#   car
#

#   car.destination.set
#   
#   pkgs    - A package matrix
#   car     - A car list
#   return  - A numeric vector containing the car's new destination.
#   
car.destination.set = function(pkgs, car) {
    
    car.pos = c(car$x, car$y)
    
    if (length(packages.get.held(pkgs)) > 0) {
        return (package.get.dropOff.position(packages.get.held(pkgs)))
    }
    
    pkgs = packages.get.pickUp(pkgs)
    
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

#   car.destination.get
#
#   car     - The car list
#   return  - A numeric vector containing the car's destination.
#   
car.destination.get = function(car) {
    return(c(car$mem[1], car$mem[2]))
}

#   car.position.get
#   
#   car     - The car list
#   return  - A numeric vector containing the car's position.
#   
car.position.get = function(car) {
    return (c(car$x, car$y))
}

#
#   car end
#

#
#   packages
#

#   packages.get.delivered
#   
#   pkgs    - A package matrix
#   Return  - All rows containing packages that have been delivered.
#   
packages.get.delivered=function(packages) {
    return(pkgs[pkgs[,5]==2,
                ,
                drop=FALSE])
}

#   packages.get.held
#   
#   pkgs    - A package matrix
#   return  - All rows containing packages that are held in the car.
#   
packages.get.held = function(pkgs) {
    return(pkgs[pkgs[,5]==1,
                ,
                drop=FALSE])
}

#   packages.get.pickUp
#   
#   pkgs    - A package matrix
#   return  - All rows containing packages that are to be picked up.
#   
packages.get.pickUp = function(pkgs) {
    return(pkgs[pkgs[,5]==0,
                ,
                drop=FALSE])
}

#
#   packages end
#

#
#   package
#

#   package.get.pickUp.position
#
#   pkg     - A numeric vector representing a package.
#   return  - A numeric vector representing the pick up position.
package.get.pickUp.position = function(pkg) {
    return(c(pkg[1],
             pkg[2]))
}

#   package.get.dropOff.position
#
#   pkg     - A numeric vector representing a package.
#   return  - A numeric vector representing the drop off position.
package.get.dropOff.position = function(pkg) {
    return(c(pkg[3],
             pkg[4]))
}

#
#   package end
#

getNextMove = function(carPos, destPos, roads) {
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

getManhattanDistance=function(origin, destination) {
    return ( abs(origin[1] - destination[1]) + abs(origin[2] - origin[2]) )
}

getRouteDistance=function() {}

getHeuristics = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  return (getManhattanDistance(node, goal))
}

#
# Program start
#
runDeliveryMan(myCar)