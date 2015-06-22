## A pair of functions that compute and cache matrix inverse

## This function creates a special "matrix" object that can cache its inverse.
## input: x is a matrix
## output: an environment contains 4 functions and 2 objects

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setInverse <- function(y) xinv <<- y
        getInverse <- function() xinv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## input: x is an enviornment created by function makeCacheMatrix
## output: a matrix which is the inverse of the matrix object contained in x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getInverse()
        if (!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setInverse(xinv)
        xinv
}
