source("~/Users/timsvensson/Documents/Kod/R/UU-Kurs-AI/Assignments/ass_1/Solution/tree.R")

myCar=function(roads, car, packages) {

    # roads={matrix(hroads)|matrix(vroads)}
    # car=list(position(x,y),wait=0,load=0,nextMove=NA,mem=list())
    # Package={source(x,y)|destination(x,y)|status(i)}
    
    print("Packages:")
    print(packages)
    
    #print("Roads:")
    #print(roads)
    
    # If there is no path in mem
    if (length(car$mem) == 0) {
        # calculate optimal path and store it in mem
        car$mem = getRoute(packages, car)
    }
    
    carPos = getCarPosition(car)
    destPos = getCarDestination(car)
    
    print("carPos")
    print(carPos)
    print("destPos")
    print(destPos)
    
    # if a node stored in mem is reached, remove it
    if (carPos[1] == destPos[1] & carPos[2] == destPos[2]) {
        car$mem = matrix(car$mem[-1,], ncol=ncol(car$mem))
        destPos = getCarDestination(car)
    }
    
    # find optimal path to next node in mem
    car$nextMove = getNextMove(carPos, destPos, roads)
    
    print("Car:")
    print(car)
    
    return(car)
}

getCarDestination=function(car) {
    mem = c(car$mem[1,1], car$mem[1,2])
}

getCarPosition=function(car) {
    return (c(car$x, car$y))
}

getNextMove=function(carPos, destPos, roads) {
    
    direction = list(down=2, left=4, right=6, up=8, stay=5)
    
    if (carPos[1] < destPos[1]) {
        nextMove = direction$r
    } else if (carPos[1] > destPos[1]) {
        nextMove =  direction$l
    } else if (carPos[2] < destPos[2]) {
        nextMove = direction$u
    } else if (carPos[2] > destPos[2]) {
        nextMove = direction$d
    } else {
        nextMove = direction$s
    }
    
    return(nextMove)
}

routeRecursive=function(packages, crntPos) {
    
    if (nrow(packages)==1) {
        return(packages)
    } else {
        
    }
}

getNonDeliveredPackages=function(packages) {
    rVal = packages
    for (i in 1:nrow(packages)) {
        if (rVal[i,5]!=0) {
            rVal=matrix(rVal[-i,], ncol=nCol(rVal))
        }
    }
    return(rVal)
}

getRoute=function(packages, car) {
    
    tree = tree.create(packages, car)
    print(class(tree))
    print("Tree:")
    cat(tree.toString(tree))
    
    stop(TRUE)
    
    numPackages = nrow(packages)
    tmpPackages = packages
    route = matrix(nrow=2*nrow(packages), ncol=2)
    
    for(i in 1:numPackages) {
        # First case is a special case, use origin of (1,1)
        if (i==1) {
            nextNode = getIndexOfClosestNode(tmpPackages[,1:2], c(1,1))
        } else {
            nextNode = getIndexOfClosestNode(tmpPackages, route[i-1,])
        }
        
        # Add pick up intersection
        route[2*i-1,] = c(tmpPackages[nextNode,1], tmpPackages[nextNode,2])
        # Add drop off intersection
        route[2*i,] = c(tmpPackages[nextNode,3], tmpPackages[nextNode,4])
        # Remove used packages
        tmpPackages = matrix(tmpPackages[c(-nextNode),], ncol=ncol(tmpPackages))
    }
    
    print("Route:")
    print(route)
    
    return(route)
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
            
            crntPkgDistance = getDistance(origin, pickUps[crntPkg,])
            bestPkgDistance = getDistance(origin, pickUps[bestPkg])
            
            if (crntPkgDistance < bestPkgDistance) {
                bestPkg = crntPkg
            }
        }
    }
    return (bestPkg)
}

getDistance=function(origin, destination) {
    return ( abs(origin[1] - destination[1]) + abs(origin[2] - origin[2]) )
}

getRouteDistance=function() {}

#
# Program start
#
runDeliveryMan(myCar)