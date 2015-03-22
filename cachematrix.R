## This program  inverses invertible matrix and cache the inverse for optimization.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 revmatrix <- NULL
        set <- function(y) {
                x <<- y
                revmatrix <<- NULL
        }
        get <- function() x
        setsoln <- function(solve) revmatrix <<- solve
        getsoln<- function() m
        list(set = set, get = get,
             setsoln = setsoln,
             getsoln = getsoln)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
   revmatrix <- x$getsoln()
        if(!is.null(revmatrix)) {
                message("getting cached data")
                return(revmatrix)
        }
        data <- x$get()
        revmatrix <- solve(data, ...)
        x$setsoln(revmatrix)
        revmatrix
}
