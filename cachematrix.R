## The two functions below are designed to cache the inverse of a
## matrix, in order to save the unnecessary computation cost

## This function creates a special "matrix" object, which is a list
## containing functions to set and get the matrix, as well as set
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the matrix has not changed and
## the inverse has already been calculated, then the inverse should 
## be retrieved from the cache

cacheSolve <- function(x, ...) {
        m <- x$getmean()
        if (!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}
