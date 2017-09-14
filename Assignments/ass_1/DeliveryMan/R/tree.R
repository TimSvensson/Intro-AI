setClass("node",
         slots=list(id.str="character",
                    id.num="numeric",
                    branches="list"))

tree.toString <- function(tree) {
    return(tree.toString.aux(0, tree))
}

tree.toString.aux <- function(level, tree) {
    
    if(class(tree)!="node") stop("Tree at level ", level, " is not of type 'node'")
    
    #print(paste("level", level))
    #print(paste("id.str", tree@id.str))
    #print(paste("id.num (", paste(tree@id.num, collapse = ", "), ")"))
    #print(paste("num branches", length(tree@branches)))
    #print("")
    
    for (i in 1:level) s = paste(s, "")
    
    s = paste(s, "+", tree@id.num, "\n")
    
    if (tree@id.str!="leaf") {
        if (length(tree@branches) > 0) {
            for (i in 1:length(tree@branches)) {
                s = paste(s, "",
                          tree.toString.aux(level+1,
                                            tree@branches[[i]]))
            }
        }
    }
    
    return(s)
}

tree.create <- function(pkgs, car) {
    root = new("node",
               id.str   = "root",
               id.num   = c(car$x, car$y, car$x, car$y, 0),
               branches = list(length = nrow(pkgs)))
    
    for (pkg in 1:nrow(pkgs)) {
        root@branches[[pkg]] = node.create(c(pkgs[pkg,1:5]),
                                           pkgs[-pkg,,drop=FALSE])
    }
    
    return(root)
}

node.create <- function(id.num, pkgs) {
    node = new("node",
               id.num=id.num,
               branches=list(length=nrow(pkgs)))
    
    if (nrow(pkgs)==0) {
        node@id.str = "leaf"
    } else {
        node@id.str = "node"
        for (pkg in 1:nrow(pkgs)) {
            node@branches[[pkg]] = node.create(pkgs[pkg,1:5,drop=TRUE],
                                               pkgs[-pkg,,drop=FALSE])
        }
    }
    
    return(node)
}