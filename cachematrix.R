## makeCacheMatrix() will cache the inverse of
## an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() will compute and retrieve the
## inverse of a matrix

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

## execute as:
## m1 <- diag(3, 5)
## cachedMatrix <- makeCacheMatrix(m1)
## cacheSolve(cachedMatrix)