## Programming Assignment #2 for R Programming
## The purpose of this script is to calculate the inverse of a matrix. 
## However, if the calculation has already been performed, it will retrieve the value from the cache

## This function will create a list which has information about retrieving the matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## Computes, caches, and returns the inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
