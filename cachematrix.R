

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable 'inv' to store the inverse of the matrix
  set <- function(y){  # Define a function 'set' that allows setting the matrix value
    x <<- y  # Assign the input matrix 'y' to the variable 'x' using the global assignment operator '<<-'
    inv <<- NULL  # Reset the stored inverse since the matrix has changed
  }
  get <- function() x  # Define a function 'get' that retrieves the current matrix value
  setInverse <- function(solveMatrix) inv <<- solveMatrix  # Define a function 'setInverse' to set the precomputed inverse
  getInverse <- function() inv  # Define a function 'getInverse' to retrieve the precomputed inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  # Return a list of the defined functions
}


cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the precomputed inverse from the provided matrix object 'x'
  if (!is.null(inv)) {  # Check if the inverse exists in cache
    message("getting cached data")  # Display a message indicating cached data is being retrieved
    return(inv)  # Return the cached inverse
  }
  
  data <- x$get()  # Retrieve the matrix data from the provided matrix object 'x'
  inv <- solve(data)  # Compute the inverse of the matrix using the solve function
  x$setInverse(inv)  # Cache the computed inverse by setting it in the provided matrix object 'x'
  inv  # Return the computed inverse
}

