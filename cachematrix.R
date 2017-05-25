## This function will calculate the inverse of matrices. It will check if the matrices are different and if the 
##the inverse has already been taken. It will have the inputted matrix as well as the set inverse.


## This function will take an input. It will create a list that has the set value of the matrix and get the value of the 
##of the matrix. It will alos have the set inverse as well as the get inverse.

makeCacheMatrix <- function(x = matrix()) {
  cm <- NULL
  set <- function(z){
    x <<- z
    cm <<- NULL   
  }
  get<- function() x
  setinv <- function(inv)cm <<- inv
  getinv <- function()cm
  list( set=set, get=get, setinv=setinv, getinv=getinv )
}



## This function will check if the inverse has already been taken, if it has it will retireve the set inverse.
## otherwise it will get the inverse of the inputted inverse.

cacheSolve <- function(x, ...) {
  cm<- x$getinv()
  if (!is.null(cm)) {
    message ("Retrieving cached data")
    return(cm)
  }
  data.matrix <- x$get()
  cm<- solve(data,...)
  x$setinv(cm)
  cm
}


  

