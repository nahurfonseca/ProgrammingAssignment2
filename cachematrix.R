## These functions create an object to calculate the inverse of a matrix and
## cache its result.

## Define the object CacheMatrix with methods:
# set : set the matrix to solve
# get : get the matrix
# setsolve : set the inverted matrix (output of solve)
# getsovle : get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) im <<- solve
        getsolve <- function() im
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Calculates the inversion of matrix x, either returns the previously cached
# solution, or computes and caches a new one.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getsolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- Solve(data, ...)
        x$setsolve(im)
        im
}
