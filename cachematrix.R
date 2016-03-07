## This function creates a special "matrix" object that can cache its inverse
# set the value of the matrix
# get the value of the matrix
# setInv the value of inverse of the matrix
# getInv get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        cacheInv <- NULL
        set <- function(y) {
                x <<- y
                cacheInv <<- NULL
        }
        get <- function() x
        setInv <- function(inv) cacheInv <<- inv
        getInv <- function() cacheInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the matrix supplied.  
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        m
}
