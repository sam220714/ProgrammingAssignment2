## Couple of functions to cache the inverse of a matrix, in order to make matrix inversion computation faster

## The following function creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve_mat) i <<- solve_mat
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function calculates the inverse of the special matrix created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(i)
  i
}
