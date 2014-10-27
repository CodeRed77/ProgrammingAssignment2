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
  #get the value of matrix
  get <- function() x
  #set the value of matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)
}


## Computes, caches, and returns the inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  # checks if calculation has been performed
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # calculate inverse if it's not in cache
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
