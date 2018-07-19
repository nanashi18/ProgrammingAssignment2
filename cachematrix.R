 ## assignment 2

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  m_inverse <- NULL ## store inverse of matrix m
  set <- function(y) {
    m <<- y
    m_inverse <<- NULL
  }
  get <- function() m
  setInverse <- function(m) m_inverse <<- m
  getInverse <- function() m_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
