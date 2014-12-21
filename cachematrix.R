## Cache the inverse of an invertible matrix.
##
## Assumptions: the supplied matrix is always invertible.
##
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 
##
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
##
## cacheSolve: Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the 
## cache.
##
## Sample Usage:
##
## > source("cachematrix.R")
## > amatrix <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
## > amatrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > 
## > amatrix$getinverse()
## NULL
## > cacheSolve(amatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(amatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > amatrix$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
##

## makeCacheMatrix
##
## Creates a special "matrix" object that can cache its inverse and is  
## a list containing the following functions:
## 
## set - set the value of the matrix and clear its cached inverse
## get - get the value of the matrix
## setinverse - set the value of the cached matrix inverse
## getinverse - get the value of the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        # Update correct instances of matrix and inverse
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) {
        # Update correct instance of matrix inverse
        i <<- inverse
    }
    
    getinverse <- function() i
    
    # Return list of mutator & accessor functions for matrix and inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
##
## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Get cached inverse matrix if any
    i <- x$getinverse()
    
    if(!is.null(i)) {
        # Cached matrix inverse exists so return it 
        
        # Useful for debugging. Comment out for maximum performance.
        message("getting cached data")
        
        return(i)
    }

    # Cached inverse matrix doesn't exist so get matrix
    data <- x$get()
    
    # Compute, cache & return the matrix inverse
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
