## This function returns the inverse of a matrix. 
## If this has already been returned, the function returns the result already saved in cache.


## Initialize objects, functions and their behaviors 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Returns the inverse of the matrix, from the cache if already calculated.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## An example of a matrix from which to return the inverse.

x <- makeCacheMatrix( matrix(c(3, 3.2, 3.5, 3.6), 2, 2))

## execute the function

cacheSolve(x)
