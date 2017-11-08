## Put comments here that give an overall description of what your
## functions do

## These two functions will cache the inverse of a matrix
## rather than re-evaluting it over and over.

## makeCacheMatrix creates a list to set and get the value of the matrix
## and then to set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve checks if the inverse of the matrix is not already cached.
## If not cached, it computes the inverse and sets the value in cache.
## It then returns the inverse of the matrix, either from cache or computed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("Getting cached data.")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
