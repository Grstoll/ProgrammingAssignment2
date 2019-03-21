## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix.

## Write a short comment describing this function 
## The first function, makeCacheMatrix creates a "matrix", with a list containing functions to set the matrix value, get the matrix value, set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix. cacheSolve retrieves the inverse from the cache, if no changes in the matrix are made.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
