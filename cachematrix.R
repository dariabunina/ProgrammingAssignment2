## The functions compute the inverse of a given matrix and cache it.

## This function solves the provided matrix and saves its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function checks if the matrix has been solved already (and in this cases loads
#the cached data), otherwise it calculates the iverse of the matrix:

cacheSolve <- function(x, ...) {
       m <- x$get_inverse()
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$set_inverse(m)
       m
}
