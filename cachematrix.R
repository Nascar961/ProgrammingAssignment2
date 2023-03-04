## Functions create a matrix, stores its inverse, and returns it


#Function creates a matrix, and also stores its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(inv) inverse <<- inv
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


#Function returns the cached inverse of a matrix or calculates it and returns it if it is not cached.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get_inverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  return(inverse)
}