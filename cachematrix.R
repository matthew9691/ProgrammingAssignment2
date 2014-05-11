## These functions two functions cache the inverse 
## of a square invertible matrix

## makeCacheMatrix
## creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve
## this computes the inverse of special "matrix" 
## returned by the makeCacheMatrix formula. If the inverse 
## has already been calculated, and the matrix is unchanged, then the 
## inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("Retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
