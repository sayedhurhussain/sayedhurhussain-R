#### **** ---- .... Caching data .... ---- **** ####

#### This function creates a special "matrix" object that can cache its inverse.
#' @param x an inversible matrix.
#' @return a special matrix which knows its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#### If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#' @param x a matrix for which wants to know the inverse.
#' @return a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  return(m)
}



#### Testing:

## Creates a inversible matrix
A <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)

## Creates a special type of matrix, that caches its inverse
m <- makeCacheMatrix(A)

## Firt call calculates and stores the inverse
cacheSolve(m)

## Second time brings data from "cache"
cacheSolve(m)
