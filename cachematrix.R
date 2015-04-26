## Those functions are use to cache the inverse of a matrix and return the cached value instead of computing it repeatedly.

## makeCacheMatrix function: define methods to set or get a matrix, and to set or get the inverse of a matrix in the cache using function closures.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}

## cacheSolve function: computes the inverse of a matrix, stores it in the cache or get it in the cache if it already exists.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting data from cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
