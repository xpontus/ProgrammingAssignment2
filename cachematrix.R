## These functions provide a new implementation of a Matrix-like
## object in R that caches the inverse of the matrix

## makeCacheMatrix creates a new object that provides four interface
## functions: set and get provide setter and getter functions for the matrix itself
## seting and getinv provide access to the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inver <<- invers 
        getinv <- function() inver
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve should be used as a replacement for the solve function
## cacheSolve first attempts to retrieve a cached inverse from the
## Matrix object, and returns this is succesful. If unsucessful, 
## the inverse is computed, stored in the Matrix object and returned.

cacheSolve <- function(x, ...) {
   inver <- x$getinv()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  ma <- x$get()
  inver <- solve(ma, ...)
  x$setinv(inver)
  inver

}
