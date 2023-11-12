## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse to NULL
        inv <- NULL
        
        # Define a function to set the matrix, updating the inverse cache
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Setting a new matrix invalidates the cached inverse
        }
        
        # Define a function to get the current matrix
        get <- function() x
        
        # Define a function to set the cached inverse matrix
        setInverse <- function(inverse) inv <<- inverse
        
        # Define a function to get the cached inverse matrix
        getInverse <- function() inv
        
        # Return a list of functions that can calculate the inverse matrix operation
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # Check if the inverse is already cached, and if so return the inverse matrix
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)  
        }

        # if the inverse matrix is not cached, calculate one
        mat <- x$get()          
        inv <- solve(mat, ...)  
        # Cache the inverse for future use
        x$setInverse(inv)  
        
        # Return the inverse matrix
        inv
}
