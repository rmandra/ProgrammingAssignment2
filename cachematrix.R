## Put comments here that give an overall description of what your
## functions do
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

## Write a short comment describing this function
## Determine inverse of a matrix and cache it
makeCacheMatrix <- function(x = matrix()) {
## This function, creates a special "vector", which is really a list containing:
##  1.set the value of the vector
##  2.get the value of the vector
##  3.set the value of the matrix inverse
##  4.get the value of the matrix inverse
##  Assumes given matrix is invertable

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## Write a short comment describing this function
## Check cache for inverse, if it doen't exist, create it
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
