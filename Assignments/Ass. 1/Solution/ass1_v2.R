#
# S4 Classes
# 

#   intersection
#   
#   x   - coordinate
#   y   - coordinate
setClass("intersection",
         slots=list(x="numeric",
                    y="numeric")
         )
#   intersection class sconstructor
intersection.new <- function(x,y) {
    return(new("intersection",
               x=x, y=y))
}

#   packages
#   
#   pu  - pick up intersection
#   do  - dropp off intersection
#   s   - status of the package
#           0 not picked up
#           1 picked up but not delivered
#           2 delivered to dropp off
setClass("package",
         slots=list(pu="intersection",
                    do="intersection",
                    s="numeric")
         )
# package class constructor
package.new <- function(p) {
    pickup  = intersection.new(x=p[1], y=p[2])
    dropoff = intersection.new(x=p[3], y=p[4])
    status  = p[5]
    return(new("package", pu=pickup, do=dropoff, s=status))
}

# Creates a list of packages
packages.new <- funciton(matrix.pkgs) {
    
    
    
}

#
# end S4
#

#
# Functions
#
myCar=function(roads, car, p) {

    # roads={matrix(hroads), matrix(vroads)}
    # car=list(x, y, wait=0, load=0, nextMove=NA, mem=list())
    # Package = matrix{puX, puY, doX, doY, status}
    
    
    
    #print(roads)
    print("packages")
    print(packages)
    #print(car)
    
    # 1. If no route in car$mem, get route to follow
    if (length(car$mem) == 0) {
        route = getRoute(car, roads, packages)
    } else {
        route = car$mem
    }
    
    # 2. Check if current package has been picked up/dropped off
    #       2.1 If it has, get next destination
    carPosition = c(car$x, car$y)
    pkgPosition = getPackagePosition(car)
    
    if (carPosition == pkgPosition) {
        route = updateRoute(car, roads, packages)
    }
    
    # 3. Get the next move
    car$nextMove = getNextMove(car, roads, packages)
    
    return(car)
}

getRoute=function(car,roads,packages) {
    route = removeDeliveredPackages(packages)
    route = sortRoute(car,roads,packages)
    return(route)
}

sortRoute=function(car,roads,packages) {
    
    route = getShortestPath(matrix(c(0, 0, car$x, car$y, 0),ncol=5,byrow=TRUE), packages)
    return(route[[1]])
}

getShortestPath=function(crntPackage, packages) {
    
    print("getShortestPath")
    print("crntPackage")
    print(crntPackage)
    print("packages")
    print(packages)
    
    # best { route, length of route }
    best = vector(mode = "list", length = 2)
    
    if (nrow(packages)==1) {
        best[[1]] = matrix(packages, ncol=length(packages))
        best[[2]] = getLengthOfRoute(matrix(c(crntPackage, packages[1,]), ncol=ncol(packages)))
    } else {
        for (pkg in 1:nrow(packages)) {
            
            crnt = list(mode = "list", length = 2)
            tmp1 = packages[pkg,]
            tmp2 = matrix(packages[-pkg,], ncol=ncol(packages), byrow=TRUE)
            crnt[[1]] = getShortestPath(tmp1, tmp2)
            crnt[[2]] = getLengthOfRoute(crnt[[1]])
            
            if (is.null(best[1]) | crnt[2] < best[2]) {
                best = crnt
            }
        }
    }
    print("best")
    print(best)
    
    return(best)
}

getLengthOfRoute=function(packages) {
    
    print("getLengthOfRoute")
    print("packages")
    print(packages)
    
    if (nrow(packages) < 2) {
        return(0)
    } else {
        for (pkg in 1:nrow(packages)-1) {
            length = getDistance(packages[[pkg,]], packages[[pkg+1,]])
        }
    }
    print("length")
    print(length)
    return(length)
}

getDistance=function(fromPkg, toPkg) {
    
    print("getDistance")
    print("fromPkg")
    print(fromPkg)
    print("toPkg")
    print(toPkg)
    
    dx = abs(fromPkg[3] - toPkg[1])
    dy = abs(fromPkg[4] - toPkg[2])
    
    d = dx + dy
    print("distance")
    print(d)
    return(d)
}

removeDeliveredPackages=function(packages) {
    for (i in nrow(packages):1) {
        if (packages[i,5] == 2) {
            packages = packages[-i,]
        }
    }
    return(packages)
}
#
# end Functions
#

#
# Program start
#
runDeliveryMan(myCar)