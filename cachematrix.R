# The following functions provide utility to create and inverse a special matrix
# that has the ability to cache its inversed matrix
# @author Christian Troller <mailto:info@christiantroller.ch>
# @version 1.0
# @since 2015-01-13

# Creates a special matrix object that can cache it's inverse
# @param x the matrix to make fancy
# @return a list of attributes (set, get, setInverse, getInverse) for the given matrix
makeCacheMatrix <- function(x = matrix()) {
    mInverse <- NULL # default our inversion to NULL
    
    # 'resets' the special matrix by assigning a new matrix
    set <- function(y) {
        x <<- y
        mInverse <<- NULL
    }
    get <- function() x # returns our original matrix
    setInverse <- function(inverse) mInverse <<- inverse # assigns the given inverse of our matrix to `mInverse`
    getInverse <- function() mInverse # returns the inverse of our matrix
    
    # return our list of attributes
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes the inverse of the given special matrix `x` returned by makeCacheMatrix.
# @param x the special matrix
# @return the inversed matrix of the given matrix
cacheSolve <- function(x, ...) {
    m <- x$getInverse() # get the inverse of our matrix
    if(!is.null(m)) { # check if we already have a cached value and return it
        return(m)
    }
    # no cached value, onwards to calculation!
    data <- x$get() # get our original matrix
    m <- solve(data, ...) # inverse it
    x$setInverse(m) # cache the inversion
    m # and return the inversion
}
