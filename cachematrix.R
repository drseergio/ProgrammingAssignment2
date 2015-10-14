## A set of functions for calculating an inverse of a matrix with caching so
## so that subsequent calls rely on previously computed value.

library(MASS)

## Creates a special "matrix" which is a list of functions to set and retrieve
## values.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates the inverse of a matrix unless a cached value is available. If
## a pre-computed value is available it is returned instead.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data, ...)
    x$setinverse(m)
    m
}
