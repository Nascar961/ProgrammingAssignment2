## Functions create a matrix, stores its inverse, and returns it

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

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