makeCacheMatrix <- function(x = matrix()) {
  cached_inv <- NULL  
  
  setMatrix <- function(y) {
    x <<- y
    cached_inv <<- NULL  
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse_matrix) cached_inv <<- inverse_matrix
  getInverse <- function() cached_inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  cached_inv <- x$getInverse()  
  
  if (!is.null(cached_inv)) {
    message("getting cached data")
    return(cached_inv)
  }
  
  matrix_data <- x$getMatrix()
  computed_inv <- solve(matrix_data, ...)  
  x$setInverse(computed_inv)  
  computed_inv
}
