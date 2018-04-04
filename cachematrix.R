##############################################################################
# This code presents a pair of functions that cache the inverse of a matrix. 
#
##############################################################################
## Example of how to test the code:
##############################################################################

# Define a matrix like:
# B = matrix(c(3, 2, 0, 0, 0, 1, 2, -2, 1), nrow=3, ncol=3)

# Create the special matrix object:
# M = makeCacheMatrix()

# Use the set nested function to asign values:
# M$set(B)

# Call the cacheSolve function:
# cacheSolve(M)
##############################################################################

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##############################################################################

## This function computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##############################################################################
