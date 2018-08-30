firstFunction <- function(){
  x<- rnorm(100)
  x
}

secondFuncion <- function(x){
  print(x)
  print(x  + rnorm(length(x)))
}
  f <- function(x){
    g<- function(y){
      y+Z
    }
    Z<-4
    x+g(x)
  }