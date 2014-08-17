## Coursera R Programming course (rprog-006)   Programming Assignment #2
##
## Add the capability to cache the inverse of a matrix.
## This is implemented using the functions: makeCacheMatrix and cacheSolve.
##
## makeCacheMatrix(x = matrix)
##   adds caching capability to a matrix
##
## cacheSolve(x, ...)
##   returns the inverse of the matrix created with makeCacheMatrix.
##   Uses the cached value stored in makeCacheMatrix if it exists.
##   If the cached value does not exist, it creates the inverse and
##   caches it by storing it in makeCacheMatrix.
##
## Example usage:
##   m <- makeCacheMatrix(matrix(1:4, 2,2))
##   cacheSolve(m)   # first call creates and caches the inverse
##   cacheSolve(m)   # returns the cached inverse
##
##   m$set(matrix(6:9, 2,2))  # change the matrix, also clears the cached value
##   cacheSolve(m)            # create and cache the inverse
##   cacheSolve(m)            # returns the cached inverse
##
##   m$get()                  # returns the matrix
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
