## A set of funtionct for caching the inverse of a matrix, for it is a costly computation.

# Need MASS for ginv() function, for solve() function is limited to square numeric or complex matrix only.
library(MASS)

## The makeCacheMatrix function creates a special "matrix" that caches matrix inversion.
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  set <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) matrixInverse <<- inverse
  
  getInverse <- function() matrixInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function return a matrix that is the inverse of 'x' from cache 
## or calculates it if called for the first time on a specific matrix
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  # Using ginv() instead of solve() function, because solve() is limited to square matrix and ginv() is not.
  matrixInverse <- ginv(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}
