## cachematrix.R
## Coded by: Ryan Yan 
## Revision date: 02.22.15

## These functions allow you to store a matrix, it's inverse, and calculate
## and set these values outside the global environment. Use these functions
## when you don't want to disrupt your work environment but want to work with
## matrix inverses, and save time by calling cached matrix data. 
## cacheSolve() currently only works with matrices invertible by solve().

## This function takes a matrix as an argument and outputs a special object
## that stores and retrives matrix data.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setinv <- function(inverse = NULL) {
        inv <<- inverse
    }
    getinv <- function() {
        inv
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a makeCacheMatrix() object and either retrieves the
## inverse data from the cache, or calculates it and stores it if the cache
## was empty.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    else {
        matrix <- x$get()
        inv <- solve(matrix)
        x$setinv(inv)
        inv
    }
}
