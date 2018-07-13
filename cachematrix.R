## Caching the Inverse of a Matric: Programming Assignment 2
## R Programming Course, Coursera

## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the "matrix" returned by 
## makeCacheMatrix above or retrieves cached value if inverse
## has already been calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
