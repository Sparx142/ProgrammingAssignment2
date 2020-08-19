## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is used to create matrix
library(MASS) ##library for non squared
makeCacheMatrix <- function(x = matrix()) {
    ##Initializing inverse as NULL
  inv <- NULL             
  set <- function(x){
    x <<- y
    inv <<- NULL
  }
  ## function to get matrix
  get <- function() {x}
  setInverse <- function(inverse) { inv <<- inverse}
  getInverse <- function() {    ##function to obtain inverse
                inver <- ginv(x)
                inver%*%x    }   
  list(set = set, get=get, setInverse=setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## this is used to cached data
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  ## checking whether the inverse is null
  if(!is.null(inv)){
    message("Getting cached data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv    ## Returns the inverse of matrix that is inverse of 'x'
}
