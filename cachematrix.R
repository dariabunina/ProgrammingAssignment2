## The functions compute the inverse of a given matrix and cache it.

## This function solves the provided matrix and saves its inverse.

makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  y <- solve(x)
  get_solved <- y
  list(x = x, get_solved = get_solved)
}


## This function checks if the matrix has been solved already (and in this cases loads
#the cached data), otherwise it calculates the iverse of the matrix:

cacheSolve <- function(x, ...) {
       m <- x$get_solved
       if(!is.null(m)) {
         message("getting cached data")
         return(m)
       }
       data <- x$get_m()
       y <- solve(data, ...)
       x$get_solved(y)
       y
}
