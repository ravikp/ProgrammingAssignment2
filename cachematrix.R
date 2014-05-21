## This file contains two functions 1. makeCacheMatrix 2. cacheSolve
## These two functions in combination helps to perform the matrix inversion with
## built in caching mechanism. Since matrix inverse operation is extremely
## expensive it is always better to cache the result of the inverse of a matrix.
## This helps to optimize the performance of the matrix inverse operations.

## makeCacheMatrix function creates a special matrix
## that stores the input matrix, special set/get functions to update and
## retrieve input matrix, and special setinverse/getinverse functions to update
## and retrieve the inverse value of the matrix.
## The return value of the makeCacheMatrix function is the special matrix.
makeCacheMatrix <- function(x = matrix()) {
    inverseOfx <- NULL
    set <- function(y) {
            x <<- y
            inverseOfx <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverseOfx <<- inverse
    getinverse <- function() inverseOfx
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve function takes a special matrix object returned from
## makeCacheMatrix function and calculates the inverse of the matrix and stores
## the result in the special matrix object.
## The return value of the cacheSolve function is the inverse of the x.
cacheSolve <- function(x, ...) {
    inverseOfx <- x$getinverse()
    if(!is.null(inverseOfx)) {
            message("getting cached data")
            return(inverseOfx)
    }
    actualMatrix <- x$get()
    inverseOfx <- solve(actualMatrix, ...)
    x$setinverse(inverseOfx)
    inverseOfx
}
