## R-Programming Week 3 Assignment 2
##  Stephen Eaton <seaton@strobotics.com.au>
##
## Two functions used to calculate the inverse of a matrix


## function: makeCacheMatrix
##  Purpose:    Creates a cached matrix object
## 
makeCacheMatrix <- function(x = matrix()) {
        # cached matrix object
        m <- NULL
        
        ## Setters/Getters
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        
        # inverse setters/getters
        cacheInverse <- function(inv) m <<- inv
        getInverse <- function() m
       
        # Define Object methods 
        list(
             setMatrix = setMatrix, 
             getMatrix = getMatrix,
             cacheInverse = cacheInverse,
             getInverse = getInverse
             )
}


## function: cacheSolve
##  Purpose:    Calculates the inverse of a matrix.
##              If the inverse has already been calculated 
##              (and the matrix has not changed), then cacheSolve will 
##              retrieve the inverse from the cached matrix object.
##
##  Assumptions:  Matrix is a Square Matrix
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get cached matrix
        inverse_m <- x$getInverse()
        
        # check if cached matrix exists and return
        if(!is.null(inverse_m)) {
                message("getting cached data")
                return(inverse_m)
        }
        
        # if not then perform inverse calculation and cache
        data <- x$getMatrix()
        inverse_m <- solve(data, ...)
        x$cacheInverse(inverse_m)
        inverse_m
}
