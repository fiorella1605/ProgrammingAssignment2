## This document contains a pair of functions that cache the inverse of a matrix.
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set 'x' matrix and inverse 'm' as null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get 'x' matrix
    get <- function() x
    
    ## Set inverse 'm'
    setinverse <- function(inverse) m <<- inverse
    
    ## Get inverse 'm'
    getinverse <- function() m
    
    ## Create a list with 4 functions
    list(set = set, get = get,
         setinverse = setinverse)
}


## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Read matrix 'm' inverse of 'x'
    m <- x$getinverse()
    
    ## If 'm' is available (not null) return 'm'
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Read matrix 'x'
    data <- x$get()
    
    ## Return a matrix that is the inverse of 'x'
    m <- solve(data, ...)
    
    ## Set inverse 'm' in cache matrix
    x$setinverse(m)
    m
}
