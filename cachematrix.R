## Put comments here that give an overall description of what your
## functions do

## Given Initial Example Functions
## makeVector

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Given Initial Example Functions
## cacheMean

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Write a short comment describing this function
## This is the assignment function
## makeCacheMatrix
## This supposed to get the inverse of a matrix.
## What this does is it sets the value of the matrix
## and gets the value of the matrix. It also sets and
## gets the inverse of the matrix using the solve
## function in R.

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


## Write a short comment describing this function
## This is the assignment function
## cacheSolve
## This computes for the inverse matrix in makeCacheMatrix.
## If solved already, just get it from the cache instead.
## The function call to get the cache data is getinverse() 
## and if no cache data exist, it will solve for it instead
## using the setinverse function which is also called from
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
