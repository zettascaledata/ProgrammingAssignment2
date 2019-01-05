## Caching the Inverse of a Matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## If the inverse has already been calculated then the cachesolve will retrieve the inverse from the cache.
        if(!is.null(inv)) {
                message("getting cached version of the data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
