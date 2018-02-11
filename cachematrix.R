## Put comments here that give an overall description of what your
## functions do

## Builds the matrix to be used by the cacheSolve function

makeCacheMatrix <- function() {
  makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


## Inverts the cached matrix from makeCacheMatrix
## Reurns unchanged matrix if already inverted

cacheSolve <- function() {
  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
      message("Loading cached matrix data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
}
