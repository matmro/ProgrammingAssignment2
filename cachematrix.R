## Programming Assignment No 2
## ---------------------------

makeCacheMatrix <- function(x = matrix()) {
# Creates a special "matrix" object that can cache its inverse. 
#
# Args:
#   x: an invertible matrix object
#
# Returns a list of functions:
#   set: specifies matrix if no argument passed to makeCacheMatrix
#   get: get matrix
#   setinverse: obtains inverse matrix
#   getinverse: returns value of inverse matrix
#
    inv <- NULL
    set <- function(y) {  
        # Applies deep binding "<<-"" to variables x, inv
        x <<- y  
        inv <<- NULL  
    }
    get <- function() {  
        # Returns makeCacheMatrix argument x
        x
    }
    setinverse <- function(inverse) {
        # Takes inverse as argument and applies deep binding to inv
        inv <<- inverse
    }
    getinverse <- function() {
        # Returns inv from parent environment
        inv
    }
    # Return vector type list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
    
}


cacheSolve <- function(x, ...) {
# Returns a matrix that is the inverse of x, either from cache or calculation.
#
# Args: 
#    x: invertible matrix object
# 
# Returns: 
#   inv: inverse matrix
# 
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
