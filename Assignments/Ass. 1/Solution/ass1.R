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
    return(package.get.pickUp.position(pkgs[pkgs.min,]))
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
    frontier.old = frontier.remove.zeroRows(frontier.old)
    nodes = frontier.remove.zeroRows(nodes)
    
    # 2. create new frontier matrix big enough for all nodes
    frontier.new.nrows = nrow(frontier.old) + nrow(nodes)
    frontier.new = matrix(nrow=frontier.new.nrows,
                          ncol=ncol(frontier.old))
    
    # 3. fill and sort the new frontier matrix with all nodes
    
    print("frontier.old")
    print(frontier.old)
    
    print("nodes")
    print(nodes)
    
    while (i <= frontier.new.nrows) {
        
        print(paste("i", i))
        
        old.best = frontier.get.best(frontier.old)
        nodes.best = frontier.get.best(nodes)
        
        print("old")
        print(old.best)
        
        print("node")
        print(nodes.best)
        
        if (frontier.better(old.best, nodes.best)) {
            frontier.new[i,] = old.best
            frontier.old = frontier.remove.row(frontier.old, old.best)
        } else {
            frontier.new[i,] = nodes.best
            nodes = frontier.remove.row(nodes, nodes.best)
        }
        
        i = i + 1
    }
    
    return (frontier.new)
}

#   frontier.remove.row
#
#   Comment:
#       If f does not contain a row identical to r, f will be returned without
#       modification.
#
#   f       - The frontier matrix
#   r       - The row to be removed
#   return  - f without the row r
#
frontier.remove.row = function(f, r) {
    for (i in 1:nrow(f)) {
        if (identical(f[i,], r)) {
            return(f[-i,
                     ,
                     drop=FALSE])
        }
    }
    return(f)
}

frontier.better = function(a, b) {
    if (length(a) == 0) return(FALSE)
    if (length(b) == 0) return(TRUE)
    if (a[5] < b[5]) return(TRUE)
    
    return(FALSE)
}

frontier.node.current.equals = function(a, b) {
    return(
        a[1] == b[1] & a[2] == b[2]
    )
}

frontier.remove.zeroRows = function(m) {

    row.current = 1
    while (row.current <= nrow(m)) {
        if (frontier.is.zeroRow(m[row.current,])) {
            m=m[-row.current,
                ,
                drop=FALSE]
        } else {
            row.current = row.current + 1
        }
    }
    return(m)
}

frontier.is.zeroRow = function(row) {
    
    row.zero = c(0,0,0,0,-1)
    
    if (identical(row, row.zero)) {
        r = TRUE
    } else {
        r = FALSE
    }

    return(r)
}

frontier.get.best = function(f) {
    return(f[which.min(f[,5]),])
}

frontier.test = function() {
    frontier.old = matrix(1:20,ncol=5,byrow=TRUE)
    nodes = matrix(c(1,2,5,6,2,
                     6,7,1,2,10,
                     2,3,4,5,5),
                   ncol=5,
                   byrow=TRUE)
    
    frontier.new = frontier.get(frontier.old, nodes)
    
    print("================")
    print("RESULT FROM TEST")
    print("================")
    print("")
    print("Inputs:")
    print("")
    print("frontier.old")
    print(frontier.old)
    print("")
    print("nodes")
    print(nodes)
    print("")
    print("Results:")
    print("")
    print("frontier.new")
    print(frontier.new)
}

#
#   frontier end
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

getHeuristicsAUX = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  return (getManhattanDistance(node, goal)* mean(roads))
}

getHeuristics = function(neighbours, goal, roads){
  #Ta fram heuristic f??r noden
  
  neighbours.h = vector("numeric", length = length(neighbours))
  
  for(i in 1:length(neighbours)) {
    neighbours.h[i] = (getHeuristicsAUX(neighbours[i], goal, roads))
  }
  
  return (neighbours.h)
}

#
#   Run tests
#
#frontier.test()

#
# Program start
#
runDeliveryMan(myCar)