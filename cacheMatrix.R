



## Coursera R Programming
## Session July 2015 - August 2015
## Programming Assignment 2
## Set up a cache to store inverse of a matrix
## Does not perform input validations in this version
## i.e accepts any matrix - may or may not be inversible
## unit tested with 2 x 2 matrix : rbind(c(4,3), c(3,2))
## usage:  
##      m2solve <- rbind(c(4,3), c(3,2))
##      mcache  <- makeCacheMatrix(m2solve)
##      result  <- cacheSolve(mcache)



## Name   : makeCacheMatrix
## Purpose: make a list of functions to save or access inversed matrix
##          use this function to :
##                      1. store the successful result of a matrix inversion
##                      2. access a cached value
##                      3. assign a new matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Name   : cacheSolve
## Purpose: attempt to calculate the inverse of a given matrix;
##          first checks the cache and returns the cached object if found;
##          it tries to solve only on cache miss and in that case, loads the cache
##          for later retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m

}

