## Implementation of cached version of matrix inverse
## operation. Matrix inversion is a costly computation,
## so caching the inverse of a matrix rather than compute
## it repeatedly may have some benefits.

## Creates a special "matrix" object that can cache its
## inverse value and return it

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y) {
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) ix <<- inverse
    getinverse <- function() ix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special "matrix". If the
## inverse has already been calculated (and the matrix
## has not changed), then return the inverse value from
## the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ix <- x$getinverse()
    if(!is.null(ix)) {
        message("getting cached data")
        return(ix)
    }
    data <- x$get()
    ix <- solve(data, ...)
    x$setinverse(ix)
    ix
}
