## Coursera R Programming course - rprog-006
##
## Add the capability to cache the inverse of a matrix.
## This is implemented using the funcions: makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix(x = matrix)
##   adds caching capability to the matrix
##
## cacheSolve(x, ...)
##   returns the inverse of the matrix created with makeCacheMatrix.
##   Uses the cached value stored in makeCacheMatrix if it exists.
##   If the cached value does not exist, it creates the inverse and
##   caches it by storing it in makeCacheMatrix.
##
## Example usage:
##   
##   

## Add caching capability to a matrix
## This version caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## the cached inverse of the matrix
    s <- NULL
    
    ## set a new matrix, clear the cached inverse
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get the matrix
    get <- function() x
    
    ## set the cached inverse of the matrix
    setsolve <- function(solve) s <<- solve
    
    ## get the cached inverse of the matrix
    getsolve <- function() s
    
    ## make the methods available
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Return the inverse of the matrix in 'makeCacheMatrix'
##  -- return the cached value if it exists, otherwise
##  -- create the inverse, cache and return it

cacheSolve <- function(x, ...) {
    ## check for a cached value and return it if it exists
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached inverse matrix")
        return(s)
    }
    
    ## no cached value found...
    ## create the inverse matrix, cache it and then return it
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
