## This script has two functions. makeCacheMatix and cacheSolve. 
## makeCacheMatrix creates a special matrix object that can cache it's inverse
## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # This variable is used to store cached inverse matrix
  inv <- NULL
  
  # Set the matrix to variable
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Get the matrix
  get <- function() x
  
  # Set the inverse of the matrix to the variable
  setinv <- function(inverse) inv <<- inverse
  # get the inverse of the matrix
  getinv <- function() inv
  
  # Return the list of getter and setters
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is not null, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Inverse of the matrix is not avaialble, get the inverse and assign it to variable
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return
  inv
}
