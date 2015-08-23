## Caching the inverse of a Matrix:
## Matrix inversion is usually a costly computatiion
## and there might be some benefit to cachig the inverse 
## of a matrix rather than compute it repeatedly.

## This file is used to create a spectial object that
## stores a matrix and caches its inverse.

## This functioin creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function (inverse) inv <<- inverse
    getinverse <- function() inv
    list (set = set, get = get, 
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix"
## created by makeCacheMatrix above.
## If the inverse has been calculated already (and no changes 
## on the matrix), then it should retrieve the inverse from
## the cache instead of calculating it again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
          message ("getting cached data")
          return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

