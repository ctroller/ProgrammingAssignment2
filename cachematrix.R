# The following functions provide utility to create and inverse a special matrix
# that has the ability to cache its inversed matrix
# @author Christian Troller <mailto:info@christiantroller.ch>
# @version 1.0
# @since 2015-01-13

# Creates a special matrix object that can cache it's inverse
# @param x the matrix to make fancy
# @return a list of attributes (set, get, setInverse, getInverse) for the given matrix
makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL
    
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) mInverse <<- inverse
    getInverse <- function() mInverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of the given special matrix `x` returned by makeCacheMatrix.
# @param x the special matrix
# @return the inversed matrix of the given matrix
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
