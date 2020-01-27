# This function wraps a matrix in inside a list of auxiliary functions
#   these functions are responsible for storing the inverse of the
#   matrix received as argument.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # variable that stores the calculated inverted matrix

  # function to update the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # return the matrix
  get <- function() x

  # a pair o functions to update and retrieve the inverted matrix
  setinverted <- function(newInv) inv <<- newInv
  getinverted <- function() inv

  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
}


# This function return the inverted of a given function, this function
#   tries to obtain a cached version and if its absent the function
#   calculated and cache the inverted matrix for the given matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverted()
  if(!is.null(inv)){
    return(inv)
  }

  inv = solve(x$get(), ...)
  x$setinverted(inv)
  inv
}
