## This program provides two function to cache the inverse of
## matrices


## makeCacheMatrix creates a special "matrix", which is really a
## list containing functions to get/set the matrix and the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve caches the inverse of a matrix

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- t(data, ...)
    x$setInverse(inv)
    inv
}
