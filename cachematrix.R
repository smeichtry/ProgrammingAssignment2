## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly.
## This pair of functions make the caching possible. There are easy to use:
## Just take them instead the 'raw' matrix and the 'raw' solve function.
##

## Constructs a cachable matrix. That is a list, which can hold a target
## matrix and the inverse of this matrix. This list contains the following
## functions: 
##
## * set, get (sets and gets the target matrix). 
## * setinverse, getinverse (sets and gets the inverse of the target matrix).
##
## Args:
##   x:    A square numeric or complex matrix, default to an empty matrix.
##
## Returns:
##   The cachable matrix.
##
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initalize i (inverse) 
    i <- NULL
    
    ## Function definitions
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    ## Save this functions in a list and return the list.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Solves the equation x %*% i = I for x, where i will be the inverse of x.
## This function returns the inverse of x, or an error if x is not inversable.
##
## Args:
##   x:    A square numeric or complex matrix.
##   ... : further arguments passed to solve
##
## Returns:
##   The inverse of x, or an error if x is not inversable.
##
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    ## if i (the inverse matrix) is not null, we'll reeturn it
    ## and exit the function.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Else we get the matrix, calculate the inverse, store it
    ## and return it.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
