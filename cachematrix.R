## Creates a matrix that allows an object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {                  ## defines the function as a matrix
    inv <- NULL                                              ## sets inv as NULL and will set inverse matrix value
    set <- function(matrix) {                                ## assigns a new set value
      x <<- matrix                                           ## value of matrix (x=matrix()) in main function
      inv <<- NULL                                           ## resets inv to null if a new matrix is found
  }
  get <- function() x                                        ## gets value of matrix function
    setInverse <- function(inverse) inv <<- inverse          ## sets inv value in main argument
    getInverse <- function() inv                             ## gets inverse of matrix
    list(set = set,                                          ## creates list of set, get and inverse functions
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Computes inverse of the matrix returned by the makeCacheMatrix
## function. If inverse already calculated, will return inverse from the cache.

cacheSolve <- function(x, ...) {              ##gets cache date
  inv <- x$getInverse()
  if (!is.null(inv)) {                        ##checks whether inverse is null
    message("getting cached data")
    return(inv)                               ##provides inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
