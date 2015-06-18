## makeCacheMatrix creates a special matrix 'x' 
## cacheSolve calculates the inverse of the matrix if its not done already. If
## the value is already calculated, it reterives it from the cache. 

## makeCacheMatrix creates a special matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
    # the object 'inverse' is delcared inside makeCacheMatrix so that it will be
    # unique each time makeCacheMatrix is assigned to an object. It is initialized
    # for the first time.
    inverse <<- NULL
    
    set = function(y) {
    
        # When the set function is called, it will update the matrix 'x' with
        # its argument. Here the matrix 'x' is not a local object but is a global
        # object declared inside the makeCacheMatrix function. If we used '<-' 
        # insisde this function, then it would create a new object named 'x' 
        # inside the set function only.
    
        x <<- y               # initialize 'x'
        inverse <<- NULL      # initialize the value of 'inverse' object
    }
    
    get = function() {
        # this function returns the matrix 'x'. Here 'x' is used as a free object
        # and hence it is looked up in makeCacheMatrix function where the 'get'
        # function is declared.
        return (x)
    }
  
    setinverse = function(inv) {
        # this function sets the value of 'inverse' object to the argument
        inverse <<- inv
    }
    
    getinverse = function() {
      # this function returns the value of the 'inverse' of the matrix
      return (inverse)
    }
    
    # Return a list containing the get, set, setinverse and getinverse functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of the matrix, if its not calculated
## already. 

cacheSolve <- function(x, ...) {
    
    # retrieve the cached value of the inverse of the matrix
    inverse = x$getinverse()
    
    if (!is.null(inverse)) {  # verify if the inverse is already calculated
      return (inverse)        # return the cached value of inverse, if present
    } else {
      
      # if the inverse is not calculated or cached, calculate it now
      matrix = x$get()      
      inverse = solve(matrix) 
      x$setinverse(inverse)   # chache the value of matrix inverse
      return(inverse)         # return the inverse of the matrix 
    }
    
}
