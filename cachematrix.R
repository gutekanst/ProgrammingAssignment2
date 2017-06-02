## The following two functions (makeCacheMatrix and cacheSolve) create a special "matrix"
## object that can cache its inverse. Then, it computes the inverse of the special "matrix"
## only if the cache matrix does not exist.  If the matrix is already stored in cache, then
## it retreives the stored matrix.

## The function creates a special "matrix" object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Setting inv to NULL as a placeholder for future value
        inv <- NULL
        
        ## Defining function to set matrix x to new matrix y and reset inv to NULL 
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        
        ## Defining function get to return the matrix x
        get <- function() x
        
        ## Defining function setinv returning the inverse matrix
        setinv <- function(inverse) inv <<- inverse
        
        ## Defining function getinv to return the inverse
        getinv <- function() inv
        
        ## Returns special "matrix" containing all of the funcrions defined above
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## This function computes the inverse of the special "matrix" only if the cache matrix
## does not exist, otherwise, it retreives the stored cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Set inv to the current stored cache value of matrix (if exists)
        inv <- x$getinv()
        
        ## Check to see if the inverse matrix is in cache, if so, return inv
        if(!is.null(inv)) {
                
                message("retreiving cached matrix data")
                return(inv)
        }
        
        ## Gets current matrix data to preform inverse
        data <- x$get()
        
        ## Preform the matrix to the data
        inv <- solve(data, ...)
        
        ## Set cache of inverse matrix for furture use
        x$setinv(inv)
        
        ## Return the inverse matrix
        inv
        
}