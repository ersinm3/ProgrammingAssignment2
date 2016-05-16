## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s = NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(set = set, get = get,
        setsolution = setsolution
        getsolution = getsolution)
}


## cacheSolve computes the inverse of the special matrix returned by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolution()
    if (is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solution(data, ...)
    x$setsolution(s)
    s
}
