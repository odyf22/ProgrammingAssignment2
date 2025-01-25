## The following code implements a caching system for the inverse of a matrix.
## The `makeCacheMatrix` function creates a special object that stores a matrix and can cache its inverse.
## The `cacheSolve` function computes the inverse of the matrix, using the cached value if available
## to avoid redundant computation.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse cache when the matrix is updated
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix stored in the special "matrix" object.
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the cached inverse to save computation time.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if available
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # If not cached, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse using solve()
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the computed inverse
}
