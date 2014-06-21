## These functions allow you to create matrices that cache their inverse,
## and to retreive the cached inverse

## creates a "caching" matrix object
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## returns the inverse of the given cacheMatrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    message("setting cached inverse")
    temp <- x$get()
    i <- solve(temp, ...)
    x$setinverse(i)
    i
}
