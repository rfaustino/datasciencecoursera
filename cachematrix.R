## 17/05/2014
## The next two functions will allow to cache a matrix inversion computation and return 
## the cache values if the computation was already been done.
## The functions are "makeCacheMatrix" and "cacheSolve"
## This is a work of the Coursera Programming Assignment 2 (peer assessment)


## makeCacheMatrix function by RF, 26-04-2014
## This function creates a special "matrix", which is really a list containing a function to set and
## get the values of the matrix and inverse matrix;
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y                     # change the value of "cached" matrix (in a different environment) to the value of the argument;
    inv <<- NULL                # the inverse matrix will be NULL;
  }
  get <- function() x           # return the value of "cached" x
  setInverse <- function(inverse) inv <<- inverse     # change the value of "cached" inverse matrix to the value of the argument;
  getInverse <- function() inv                        # return the value of "cached" inverse matrix
  list(set = set, get = get,                          # the function creates a list of functions
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function by RF, 26-04-2014
## This function try to return the inverse matrix previously computed or 
## compute it if the result isn't in cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()         # try to get the inverse matrix that is in cache;
  
  if(!is.null(inv)) {           # if the matrix is in cache ...
    message("getting cached data")
    return(inv)                 # return the message and inverse matrix;
  }
  data <- x$get()               # get the matrix previously intiatialized in makeCacheMatrix function;
  inv <- solve(data, ...)       # compute the inverse matrix using solve function;
  x$setInverse(inv)             # update the cache with the inverse matrix;
  inv                           # return the inverse matrix;
}