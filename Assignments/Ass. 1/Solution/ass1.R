myCar = function(roads, car, packages) {
  # roads={matrix(hroads)|matrix(vroads)}
  # car=list(position(x,y),wait=0,load=0,nextMove=NA,mem=list())
  # Package={source(x,y)|destination(x,y)|status(i)}
  
  # If there is no path in mem
  if (length(car$mem) == 0) {
    # calculate optimal path and store it in mem
    car$mem = car.destination.set(packages, car)
  }
  #########REMOVE WHEN DONE##################
  search(c(5,5), c(7,8), roads)
  ########################################### 
  car.pos = car.position.get(car)
  dest.pos = car.destination.get(car)
  
  # if a node stored in mem is reached, remove it
  if ((car.pos[1] == dest.pos[1] & car.pos[2] == dest.pos[2])) {
    car$mem = car.destination.set(packages, car)
    dest.pos = car.destination.get(car)
  }
  
  # find optimal path to next node in mem
  car$nextMove = getNextMove(car.pos, dest.pos, roads)
  
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
  
  pkgs.distance = vector("numeric", length = nrow(pkgs))
  
  for (i in 1:nrow(pkgs)) {
    pkg.pos = c(pkgs[i, 1],
                pkgs[i, 2])
    pkgs.distance[i] = getManhattanDistance(car.pos, pkg.pos)
  }
  
  pkgs.min = which.min(pkgs.distance)
  return(package.get.pickUp.position(pkgs[pkgs.min, ]))
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
packages.get.delivered = function(packages) {
  return(pkgs[pkgs[, 5] == 2,
              ,
              drop = FALSE])
}

#   packages.get.held
#
#   pkgs    - A package matrix
#   return  - All rows containing packages that are held in the car.
#
packages.get.held = function(pkgs) {
  return(pkgs[pkgs[, 5] == 1,
              ,
              drop = FALSE])
}

#   packages.get.pickUp
#
#   pkgs    - A package matrix
#   return  - All rows containing packages that are to be picked up.
#
packages.get.pickUp = function(pkgs) {
  return(pkgs[pkgs[, 5] == 0,
              ,
              drop = FALSE])
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


getNextMove = function(carPos, destPos, roads) {
  direction = list(
    down = 2,
    left = 4,
    right = 6,
    up = 8,
    stay = 5
  )
  
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

getEdgeCost = function(neighbours, currentPos, roads) {
  hroads = roads$hroads
  vroads = roads$vroads
  
  
  print("Length neighbours:")
  print(length(neighbours))
  print(neighbours)
  g = list()
  for (i in 1:length(neighbours)) {
    
    # Neighbour to the right
    
    if (neighbours[[i]][1] > currentPos[1] & neighbours[[i]][2] == currentPos[2]) {
      print("neighbours[[i]][1]")
      print(neighbours[[i]][1])
      print("neighbours[[i]][2]")
      print(neighbours[[i]][2])
      print("currentPos[1]")
      print(currentPos[1])
      print("currentPos[2]")
      print(currentPos[2])
      g <- c(g, hroads[currentPos[2], currentPos[1]+1])
    }
    
    # Neighbour to the left
    
    if (neighbours[[i]][1] < currentPos[1] & neighbours[[i]][2] == currentPos[2]) {
      print("neighbours[[i]][1]")
      print(neighbours[[i]][1])
      print("neighbours[[i]][2]")
      print(neighbours[[i]][2])
      print("currentPos[1]")
      print(currentPos[1])
      print("currentPos[2]")
      print(currentPos[2])
      g <- c(g, hroads[currentPos[2], currentPos[1]-1])
    }
    
    # Neighbour above
    
    if (neighbours[[i]][2] > currentPos[2] & neighbours[[i]][1] == currentPos[1]) {
      print("neighbours[[i]][1]")
      print(neighbours[[i]][1])
      print("neighbours[[i]][2]")
      print(neighbours[[i]][2])
      print("currentPos[1]")
      print(currentPos[1])
      print("currentPos[2]")
      print(currentPos[2])
      g <- c(g, vroads[currentPos[2]+1, currentPos[1]])
    }
    
    # Neighbour below
    
    if (neighbours[[i]][2] < currentPos[2] & neighbours[[i]][1] == currentPos[1]) {
      print("neighbours[[i]][1]")
      print(neighbours[[i]][1])
      print("neighbours[[i]][2]")
      print(neighbours[[i]][2])
      print("currentPos[1]")
      print(currentPos[1])
      print("currentPos[2]")
      print(currentPos[2])
      g <- c(g, vroads[currentPos[2]-1, currentPos[1]])
    }
  }
  
  print("Finished making g:")
  print(g)
  return (g)
  
  # If x or y value > currentPos then move right or up
  # If x or y value < currentPos then move left or down
  
}

getNeighbours = function(currPos, roads, destination) {
  
  neighbours = list()
  rows = nrow(roads$vroads)
  if (currPos[1] == 1) {
    if (currPos[2] == 1) {
      # Anropa getEdgeCost() och getHeuristics() f??r listan
      neighbours = list(c(1, 2), c(2, 1))
    } else if (currPos[2] == rows) {
      neighbours = list(c(1, rows - 1), c(2, rows))
    } else {
      neighbours = list(
        c(currPos[1] + 1, currPos[2]),
        c(currPos[1], currPos[2] + 1),
        c(currPos[1], currPos[2] - 1)
      )
    }
  } else if (currPos[1] == rows) {
    if (currPos[2] == 1) {
      neighbours = list(c(rows - 1, 1), c(rows, 2))
    } else if (currPos[2] == rows) {
      neighbours = list(c(rows, rows - 1), c(rows))
    } else {
      neighbours = list(
        c(currPos[1] - 1, currPos[2]),
        c(currPos[1], currPos[2] + 1),
        c(currPos[1], currPos[2] - 1)
      )
    }
  } else if (currPos[2] == 1) {
    neighbours = list(
      c(currPos[1] + 1, currPos[2]),
      c(currPos[1] - 1, currPos[2]),
      c(currPos[1], currPos[2] + 1)
    )
  } else if (currPos[2] == rows) {
    neighbours = list(
      c(currPos[1] + 1, currPos[2]),
      c(currPos[1] - 1, currPos[2]),
      c(currPos[1], currPos[2] - 1)
    )
  } else {
    neighbours = list(
      c(currPos[1] + 1, currPos[2]),
      c(currPos[1] - 1, currPos[2]),
      c(currPos[1], currPos[2] - 1),
      c(currPos[1], currPos[2] + 1)
    )  
  }
  
  
  
  
  neighboursHeuristics = getHeuristics(neighbours, destination, roads)
  edgeCost = getEdgeCost(neighbours, currPos, roads)
  currPosHeuristic = getHeuristicsAUX(currPos, destination, roads)
}

getManhattanDistance=function(node, goal) {
  
  xDifference = abs(node[1] - goal[1]) 
  yDifference = abs(node[2] - goal[2])
  
  return (xDifference + yDifference)
  
}

getHeuristicsAUX = function(node, goal, roads){
  #Ta fram heuristic f??r noden
  aux = getManhattanDistance(node, goal)
  #roadsMean = mean(roads$hroads) + mean(roads$vroads)
  #aux = aux * (roadsMean/2)
  return (aux)
}

getHeuristics = function(neighbours, goal, roads){
  #Ta fram heuristic f??r noden
  
  neighbours.h = vector("numeric", length = length(neighbours))
  for(i in 1:length(neighbours)) {
    neigh = getHeuristicsAUX(neighbours[[i]], goal, roads)
    neighbours.h[i] <- neigh
  }

  return (neighbours.h)
}

setFrontier = function(currPosHeur) {
  frontier <- data.frame(
    accCost = 0,
    fCost = currPosHeur,
    destHeuristic = currPosHeur,
    xDestCoord = currentPos[1],
    yDestCoord = currentPos[2],
    xOriginCoord = currentPos[1],
    yOriginCoord = currentPos[2]
  )
  return (frontier)
}

setVisited = function() {
  
  visited <- data.frame(
    accCost = 999,
    fCost = 999,
    destHeuristic = 999,
    xDestCoord = 999,
    yDestCoord = 999,
    xOriginCoord = 999,
    yOriginCoord = 999
    
  )
  
  return(visited)
}

search = function(currentPos, destination, roads) {
  
  
  currPosHeur = getHeuristics(list(currentPos), destination, roads)
  frontier = setFrontier(currPosHeur)
  visited = setVisited()
  # visited <- visited[-c(1),]
  
  newCurrentPos = currentPos
  print("Frontier:")
  print(frontier)
  print("Visited:")
  print(visited)
  #Loop from here
  while((visited[nrow(visited), 4] != destination[1]) && (visited[nrow(visited), 5] != destination[2])){
    
    neighbours = getNeighbours(newCurrentPos, roads, destination)
    heuristics = getHeuristics(neighbours, destination, roads)
    edgeCost = getEdgeCost(neighbours, newCurrentPos, roads)
    print("Edgecost:")
    print(edgeCost)
    xList = list()
    yList = list()
    
    for(i in neighbours) {
      xList = c(xList, i[1])
    }
    
    for(i in neighbours) {
      yList = c(yList, i[2])
    }
    
    row = NaN
  
    for(i in 1:length(xList)){
      if(length(xList) < i) {
        break
      }
      foundACopy = FALSE
      for(j in 1:nrow(frontier)) {
        if(nrow(frontier) < j){
          break
        }
        if(frontier[j,4] == xList[[i]] && frontier[j,5] == yList[[i]]){
          if((frontier[j, 1] + frontier[j, 2]) > (xList[[i]] + yList[[i]])) {
            frontier <- frontier[-c(j),]
          } else if ((frontier[j, 1] + frontier[j, 2]) <= (xList[[i]] + yList[[i]])) {
            foundACopy = TRUE
          } 
        }
      }
      if(foundACopy) {
        next
      }
      
      visited <- rbind(visited, frontier[1,])
      print("Frontier unsorted")
      print(frontier)
      frontier = frontier[-c(1),]
      frontier <- frontier[ order(frontier$fCost + frontier$accCost), ]
      rownames(frontier) <- seq(length=nrow(frontier))
      
      
      nrowVisited = nrow(visited)
      
      tempframe <- data.frame(
        accCost = visited[nrowVisited,1] + (visited[nrowVisited,2] - visited[nrowVisited,3]),
        fCost = edgeCost[[i]] + heuristics[i],
        destHeuristic = heuristics[i],
        xDestCoord = xList[[i]],
        yDestCoord = yList[[i]],
        xOriginCoord = newCurrentPos[1],
        yOriginCoord = newCurrentPos[2]
      )
      frontier = rbind(frontier, tempframe)
    }
    
 
    newCurrentPos = c(visited[nrow(visited), 4], visited[nrow(visited), 5])
  }
  print("Visited")
  print(visited)
  
  rowsinVisited = nrow(visited) 
  wayBack <- data.frame(
    xCoord = visited[rowsinVisited,4], 
    yCoord = visited[rowsinVisited,5]
  )
  print("Concat")
  print(wayBack[1,])
  print("Destination")
  print(destination)
  print("Wayback Original")
  print(wayBack)
  xOrigin = visited[rowsinVisited,6]
  yOrigin = visited[rowsinVisited,7]
  
  for(i in 1:rowsinVisited){
    if(visited[i, 4] == xOrigin && visited[i, 5] == yOrigin){
      tempFrame <- data.frame(
        xCoord = visited[i, 4],
        yCoord = visited[i, 5]
      )
      wayBack <- rbind(tempFrame, wayBack)
      xOrigin = visited[i, 6]
      yOrigin = visited[i, 7]
      print("Wayback")
      print(wayBack)
      
      if(xOrigin == currentPos[1] && yOrigin == currentPos[2]){
        break
      }
    }
  }
  
  return (wayBack)
  
}



#
#   Run tests
#
#frontier.test()

#
# Program start
#
runDeliveryMan(myCar)