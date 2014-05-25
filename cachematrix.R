# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
# a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will 
# not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

# Make a matrix object. Returns a list of 4 functions:
## set - set main matrix
## get - get main matrix
## setInvMatrix - set inverted matrix
## getInvMatrix - get inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  
  # Validate parameters
  if(!is.matrix(x)){
    message("Error: Expecting a matrix.")  
    return
  }
  
  # Initialize values
  invMatrix <- NULL
  
  # Set main matrix
  set <- function(m) {
    x <<- m
    invMatrix <<- NULL
  }
  # Get main matrix
  get <- function() x
  
  # Set inverted matrix
  setInvMatrix <- function(inv) invMatrix <<- inv
  
  # Get inverted matrix
  getInvMatrix <- function() invMatrix
  
  # Return a list of 4 functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

# Invert a matrix object created with makeCacheMatrix function. If inverted matrix was previously cached, a cached value is returned.
cacheSolve <- function(x, ...) {
  
  # Check if cached inverted matrix exists
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("Note: Returning cached matrix.")
    return(invMatrix)
  }
  
  # If no cache exists - try to invert it
  message("Note: No cached matrix found.")
  
  ## Validate matrix with simple checks before solving
  if(!is.matrix(x$get())){
    message("Error: Expecting a matrix.")
    return(NA)
  }
  if( ncol(x$get()) != nrow(x$get()) | is.na(mean(x$get())) ){
    message("Error: Expecting a square matrix without missing values.")
    return(NA)
  }
  
  ## Invert the matrix
  invMatrix <- solve(x$get(),...) 

  ## Set cached value
  x$setInvMatrix(invMatrix)
  
  # Return inverted matrix
  message("Note: Matrix was successfully inverted.")
  return(invMatrix)
}
