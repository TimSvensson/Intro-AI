hroads = matrix(c(1:100), nrow= 10, byrow=TRUE)
vroads = matrix(c(1:100), nrow= 10, byrow=FALSE)

testCornerPos = c(c(1,1), c(10,1), c(1,10), c(10,10))
testEdgePos = c(c(5,1), c(10,5), c(5,10), c(1,5))
testMiddlePos = c(c(3,8), c(2,9), c(5,5))

currentPos = c(1,10)
neighbours = list(c(1,9), c(2,10))
g = c()
print("hroads is:")
print(hroads)
print("vroadsh is:")
print(vroads)
print("currentPos is:")
print(currentPos)

for (i in neighbours) {
  
  print("i is:")
  print(i)
  # Neighbour to the right
  if (i[1] > currentPos[1] & i[2] == currentPos[2]) {
    print("Neighbour to right")
    g = c(g, hroads[currentPos[2], currentPos[1]+1])
    print("g:")
    print(g)
    
  }
  
  # Neighbour to the left
  if (i[1] < currentPos[1] & i[2] == currentPos[2]) {
    print("Neighbour to left")
    cat(sprintf("hroads[1] and [2]: %d\n", (hroads[currentPos[1], currentPos[2]]) ))
    g = c(g, hroads[currentPos[2], currentPos[1]-1])
    print("g:")
    print(g)
    
  }
  
  # Neighbour above
  if (i[2] > currentPos[2] & i[1] == currentPos[1]) {
    print("Neighbour above")
    g = c(g, vroads[currentPos[2]+1, currentPos[1]])
    print("g:")
    print(g)
    
  }
  
  # Neighbour below
  if (i[2] < currentPos[2] & i[1] == currentPos[1]) {
    print("Neighbour below")
    g = c(g, vroads[currentPos[2]-1, currentPos[1]])
    print("g:")
    print(g)
  }
}




# If x or y value > currentPos then move right or up
# If x or y value < currentPos then move left or down