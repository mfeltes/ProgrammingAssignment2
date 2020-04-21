## Calculating the inverse of a matrix is computationally expensive
## This set of functions enables caching of matrix inverse, saving time!

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setmatinv <- function(solve) s <<- solve
    getmatinv <- function() s
    list(set = set, get = get, setmatinv = setmatinv, getmatinv = getmatinv)
}


## cacheSolve takes a special matrix (from makeCacheMatrix) and calculates inverse
## if inverse has already been calculated/cached, the inverse is retrieved (not calculated)

cacheSolve <- function(x, ...) {
    s <- x$getmatinv()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setmatinv(s)
    s   ## Return a matrix that is the inverse of 'x'
}
