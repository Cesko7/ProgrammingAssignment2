makeCacheMatrix <- function(x = matrix()) {
    cached_inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set a new matrix and reset cached inverse
    setMatrix <- function(y) {
        x <<- y
        cached_inv <<- NULL  
    }
    
    # Function to get the current matrix
    getMatrix <- function() x
    # Function to store the inverse of the matrix
    setInverse <- function(inverse_matrix) cached_inv <<- inverse_matrix
    # Function to retrieve the cached inverse if available
    getInverse <- function() cached_inv
    # Return a list containing all the functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    cached_inv <- x$getInverse()  # Retrieve the cached inverse if it exists
    # If the inverse is already cached, return it
    if (!is.null(cached_inv)) {
        message("retrieving cached inverse")
        return(cached_inv)
    }
    
    # Otherwise, compute the inverse, cache it, and return it
    matrix_data <- x$getMatrix()
    computed_inv <- solve(matrix_data, ...)  # Compute the inverse
    x$setInverse(computed_inv)  # Cache the computed inverse
    computed_inv  # Return the computed inverse
}
