## Shortens the computation of finding the inverse
# of a matrix by caching the data

## This function creates a list that
# - set the value of the list
# - get the value of the list
# - set the value of the inverse matrix
# - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the 
# list created with the above function. However, it first 
# checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the 
# computation. Otherwise, it calculates the inverse of the 
# data and sets the value of the inverse in the cache via 
# the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
