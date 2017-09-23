# THIS IS ALL YOU DID NIGGA? YOU AIN'T SHIT
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

#' frontier.get
#'
#' @param frontier.old A frontier matrix, the old frontier.
#' @param nodes A frontier matrix, new nodes to be added to the frontier.
#'
#' @return A frontier matrix, the new frontier with the nodes matrix added.
#' @export
#'
#' @examples
frontier.get = function(frontier.old, nodes) {
  # 1. find and resolve doubles
  for (i in nrow(frontier.old):1) {
    for (j in nrow(nodes):1) {
      if (frontier.node.current.equals(frontier.old[i, ], nodes[j, ])) {
        if (frontier.old[i, 5] > nodes[j, 5]) {
          frontier.old[i, ] = c(0, 0, 0, 0, -1)
        } else {
          nodes[j, ] = c(0, 0, 0, 0, -1)
        }
        break
        
      }
    }
  }
  frontier.old = frontier.remove.zeroRows(frontier.old)
  nodes = frontier.remove.zeroRows(nodes)
  
  # 2. create new frontier matrix big enough for all nodes
  frontier.new.nrows = nrow(frontier.old) + nrow(nodes)
  frontier.new = matrix(nrow = frontier.new.nrows,
                        ncol = ncol(frontier.old))
  
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
      frontier.new[i, ] = old.best
      frontier.old = frontier.remove.row(frontier.old, old.best)
    } else {
      frontier.new[i, ] = nodes.best
      nodes = frontier.remove.row(nodes, nodes.best)
    }
    
    i = i + 1
  }
  
  return (frontier.new)
}

#' frontier.remove.row
#'
#' @param f The frontier matrix
#' @param r The row to be removed
#'
#' @return f without the row r
#' @export
#'
#' @examples
frontier.remove.row = function(f, r) {
  for (i in 1:nrow(f)) {
    if (identical(f[i, ], r)) {
      return(f[-i,
               ,
               drop = FALSE])
    }
  }
  return(f)
}


#' frontier.better
#'
#' @param a A row from a frontier matrix
#' @param b A row from a frontier matrix
#'
#' @return TRUE if a's f-value is lower than b's, otherwise false.
#' @export
#'
#' @examples
frontier.better = function(a, b) {
  if (length(a) == 0)
    return(FALSE)
  if (length(b) == 0)
    return(TRUE)
  if (a[5] < b[5])
    return(TRUE)
  
  return(FALSE)
}

#' frontier.node.current.equals
#'
#' @param row.a A row from a frontier matrix
#' @param row.b A row from a frontier matrix
#'
#' @return TRUE if a's current node is identical to b's current node.
#' @export
#'
#' @examples
frontier.node.current.equals = function(row.a, row.b) {
  return(identical(row.a[1:2], row.b[1:2]))
}

#' frontier.remove.zeroRows
#'
#' @param m A frontier matrix
#'
#' @return m without with any zero valued rows removed.
#' @export
#'
#' @examples
frontier.remove.zeroRows = function(m) {
  row.current = 1
  while (row.current <= nrow(m)) {
    if (frontier.is.zeroRow(m[row.current, ])) {
      m = m[-row.current,
            ,
            drop = FALSE]
    } else {
      row.current = row.current + 1
    }
  }
  return(m)
}

#' frontier.is.zeroRow
#'
#' @param row A row from a frontier matrix
#'
#' @return TRUE if the row is a zero valued row.
#' @export
#'
#' @examples
frontier.is.zeroRow = function(row) {
  row.zero = c(0, 0, 0, 0, -1)
  
  if (identical(row, row.zero)) {
    r = TRUE
  } else {
    r = FALSE
  }
  
  return(r)
}

#' frontier.get.best
#'
#' @param f A frontier matrix
#'
#' @return The row in f which has the lowest f-value.
#' @export
#'
#' @examples
frontier.get.best = function(f) {
  return(f[which.min(f[, 5]), ])
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
frontier.test = function() {
  frontier.old = matrix(1:20, ncol = 5, byrow = TRUE)
  nodes = matrix(c(1, 2, 5, 6, 2,
                   6, 7, 1, 2, 10,
                   2, 3, 4, 5, 5),
                 ncol = 5,
                 byrow = TRUE)
  
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
