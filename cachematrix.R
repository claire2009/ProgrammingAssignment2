## This is a pair of functions that computes the inverse of a matrix and store it in the cache. 
## It first checks to see if the inverse of the matrix has already been computed. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets it in the cache.


## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  ## create a special "matrix object"
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse matrix
  setinverse <- function(solve) m <<- solve
  ## get the value of the inverse matrix
  getinverse <- function(solve) m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## check if the inverse of the matrix has already been calculated. If so, retrive the inverse from the cache.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the inverse has not been calculated yet, compute it now and set it in the cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
