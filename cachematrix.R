## This program  inverses invertible matrix and cache the inverse for optimization.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 rm <- NULL
        set <- function(y) {
                x <<- y
                rm <<- NULL
        }
        get <- function() x
        setsoln <- function(solve) rm <<- solve
        getsoln<- function() m
        list(set = set, get = get,
             setmean = setsoln,
             getmean = getsoln)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
   rm <- x$getsoln()
        if(!is.null(rm)) {
                message("getting cached data")
                return(rm)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsoln(rm)
        rm
}
