## The functions works by accepting a matrix as its argument and then returns its inverse. 
## Furthermore, it avoids repetitive calculations and unnecessary usage of memory by caching the inverse of the matrix 

## The makeCacheMatrix function creates a matrix which can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The cacheSolve function calculates the inverse of the above matrix. It checks for the inverse 
## and if it already exists and the matrix being inverted is the same , 
## returns the inverted value from the cache and prevents recalculation 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting inverse from the cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
