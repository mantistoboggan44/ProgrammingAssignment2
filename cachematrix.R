## Creates a matrix that allows an object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   ## sets inverse as NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function() x      ##gets matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv               ##gets inverse of matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes inverse of the matrix returnted by the makeCacheMatrix
## function

cacheSolve <- function(x, ...) {       ##gets cache date
  inv <- x$getInverse()
  if (!is.null(inv)) {                 ##checks whether inverse is null
    message("getting cached data")
    return(inv)                       ##provides inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
