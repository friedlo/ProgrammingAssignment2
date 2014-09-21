## The following function caches the matrix and its inverse of the
## matrix in the varable y. It detects whether the inverse has already
## been computed and cached or wheterh y is unchangedchanged. The cahced inverse 
## inverse will be returned. Otherwise the inverse will be computed, 
## cached and returned.
## Usage:
##   mchached = makeCacheMatrix();
##   y <- matrix(.......) #defind the matrix
##   cacheSolve(mchached)

## Creates the makeCacheMatrix object to store 
## and retrievd the cached inverse of of the matrix
## in the matrix y.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <- y
        m <- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}



## Computes the inverses of matrix y if the inverse matrix
## does not exist or y has changed. Otherwise the cached
## matrix is used. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m) && dim(x$get()) == dim(y) && all(x$get() == y)) {
        message("getting cached data")
        return(m)
    }
    message("setting cached data")
    x$set(y)
    m <- solve(y, ...)
    x$setmatrix(m)
    m
}



