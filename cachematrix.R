## The makeCacheMatrix function sets the value of a matrix, 
## calculates its inverse, and then caches that value.

## The cacheSolve function searches the cache for the inverse 
## of 'x'. If the solution is in the cache, it is returned. 
## If the inverse is not in the cache, the solution is calculated
## and returned.

## This function calculates the inverse of a matrix and caches it.

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

## This function returns the cached solution. If it does not exist,
## the solution is calculated and returned.

cacheSolve <- function(x, ...) {
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