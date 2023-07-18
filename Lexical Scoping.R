# Create a cache for a matrix and its inverse
makeCacheMatrix <- function(matrix_data = matrix()) {
  inverse_matrix <- NULL
  
  # Function to set the matrix data
  setMatrix <- function(matrix) {
    matrix_data <<- matrix
    inverse_matrix <<- NULL
  }
  
  # Function to get the matrix data
  getMatrix <- function() {
    matrix_data
  }
  
  # Function to set the inverse of the matrix
  setInverseMatrix <- function(inverse) {
    inverse_matrix <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverseMatrix <- function() {
    inverse_matrix
  }
  
  # Return a list of functions for setting/getting matrix and its inverse
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

# Function to calculate the inverse of a matrix with caching
cacheSolve <- function(cache_matrix, ...) {
  # Try to get the cached inverse of the matrix
  cached_inverse <- cache_matrix$getInverseMatrix()
  if (!is.null(cached_inverse)) {
    message("Using cached inverse")
    return(cached_inverse)
  }
  
  # If the cached inverse is not available, calculate the inverse and cache it
  matrix_data <- cache_matrix$getMatrix()
  inverse_matrix <- solve(matrix_data, ...)
  cache_matrix$setInverseMatrix(inverse_matrix)
  
  inverse_matrix
}
