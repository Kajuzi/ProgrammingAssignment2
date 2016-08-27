## 27-08-2016
## 
## These functions compute the inverse of an invertible matrix 
## They take advantage of R's scoping rules to cache the result
## and improve performance. The inverse is computed if the matrix 
## changes. Otherwise, it is retreaved from cache

## Create the "matrix" object and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solved) inv <<- solved
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes an invertible matrix and returns its inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting inverse from cache...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
