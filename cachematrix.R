# Matrix inversion is usually cpu intensive and caching may optimize
# processing rather than compute it on demand. 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invMatrix <<- inverse
  getinverse <- function() invMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
# following function computes inverse of a matrix.  
# for all invertible matrices.
cacheSolve <- function(x, ...) {
  invMatrix <- x$getinverse()
  if(!is.null(invMatrix)) {
    message("getting cached data.")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setinverse(invMatrix)
  invMatrix
}