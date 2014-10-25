##
## Functions included in this script are:
## . makeCacheMatrix(): Make an object that has the possibility to hold
##                      specified matrix and its inverse
## . cacheSolve(): Returns the inverse of specified matrix.
##

##
## Function: makeCacheMatrix()
##
## Make an object that has the possibility to hold a specified matrix
## and its inverse
## Constructor assumes the parameter is a square matrix
##
## Operation $get() will return the matrix
## Operation $set() shall be used to set the matrix 
## Operation $getInverse() will return the inverse matrix if available
## Operation $setInverse() shall be used to set the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getMatrix <- function() x

    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse

    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

##
## Function: cacheSolve()
##
## Returns the inverse of specified matrix.
## Function parameter must have been created with makeCacheMatrix()
## If inverse of matrix has been calculated and cached, this will be returned.
## If inverse of matrix has not been calculated it will be calculated,
##     cached and returned.
##
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
