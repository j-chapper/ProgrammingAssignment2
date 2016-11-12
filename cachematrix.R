## Functions that allow the results of matrix operations to be cached,
## and later retrieved from the cache instead of recomputing them.

## Given a matrix, return a list of functions for get/setting its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Given a matrix, return and cache its inverse.
## If the inverse has previously been cached, return the cached value
## without recomputing it.

cacheSolve <- function(x, ...) {

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m  
  
}
