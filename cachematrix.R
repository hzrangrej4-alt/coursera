## These functions provide a way to optimize matrix inversion computations.
## By utilizing R's lexical scoping, the matrix and its computed inverse 
## can be cached within a special object environment, eliminating the need 
## to repeatedly calculate the inverse for unchanged data.

## This function creates a special "matrix" object (implemented as a list) 
## that can cache its input matrix and its computed inverse.
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix, initialized to NULL
    inv <- NULL
    
    # Method to set/change the matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL # Clear the cache since the underlying matrix changed
    }
    
    # Method to retrieve the matrix value
    get <- function() x
    
    # Method to store the calculated inverse in the cache
    setInverse <- function(inverse) inv <<- inverse
    
    # Method to retrieve the cached inverse
    getInverse <- function() inv
    
    # Return a list containing all the defined methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## hasn't changed), it retrieves the result from the cache instead of recalculating.
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    inv <- x$getInverse()
    
    # If a cached version exists, return it immediately
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise, get the original matrix data
    data <- x$get()
    
    # Compute the inverse matrix using solve()
    inv <- solve(data, ...)
    
    # Save the computed inverse back into the cache
    x$setInverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
