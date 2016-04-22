## Programming Assignment2. This set of functions is intended to form the inverse of an input matrix. In order to avoid unneccessary
## computing, the calculated inversed matrix is cached so that it can be quickly called if needed.

## This function creates a list containig the input matrix and subroutines that will be needed for the CacheSolve() function later
## It first sets the inverse matrix to NULL and defines a list of subroutines called set,get,setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cachesolve is used to hand over the matrix created by the makeCacheMatrix() function. It checks if the inverse matrix has 
## already been computed in a previous call of this function and stored in the variable inv. If yes the cached matrix is prompted, if not
## the functions defined by the output list of makeCacheMatrix are subsetted and called to calculate (using solve()) the inverse matrix 
## using the matrix inputted in the makeCacheMatrix function and stored in the get() subroutine. Last the inverse matrix is cached.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting matrix from cached data")
    return(inv)
  }
  mymat <- x$get()
  inv <- solve(mymat)
  x$setinverse(inv)
  inv
}
