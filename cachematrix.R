## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize the inverse to NULL
        inv <- NULL
        # Define a function to set the matrix, updating the inverse cache
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Setting a new matrix invalidates the cached inverse
        }
        # Define a function to get the stored matrix
        get <- function() x
        # Define a function to set the cached inverse
        setInverse <- function(inverse) inv <<- inverse
        # Define a function to get the cached inverse
        getInverse <- function() inv
        # Return a list of functions that can manipulate the matrix object
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        # Check if the inverse is already cached
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)  # If cached, return the cached inverse
        }
        mat <- x$get()  # Retrieve the matrix
        inv <- solve(mat, ...)  # Compute the inverse
        x$setInverse(inv)  # Cache the inverse for future use
        inv
}
