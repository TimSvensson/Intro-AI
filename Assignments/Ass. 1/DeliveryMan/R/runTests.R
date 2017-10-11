averageTest <- function(tests){
  sum = 0
  for (i in 1:tests) {
    sum=sum+runDeliveryMan(carReady = aStar, dim = 10, turns = 2000, doPlot = F, pause = 0, del = 5)
    if(i%%10==0){
      print(i)
      print(sum/i)
    }
  }
  print(sum/i)
  return(0)
}
