## This file contains two functions. A function "makeCacheMatrix" takes as input a matrix element and
## retuns an object that allows self-caching of the inverse of the provided input matrix. A second function 
## "cacheSolve" resolve the inverse of the object created with the "makeCacheMatrix" function that is provided 
## as input. It is assumed that the provided matrix is invertible.

## Creates an object albe to self-cache a matrix inverse  

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
			set <- function(y) {
					x <<- y
					xinv <<- NULL
			}
			get <- function() x
			setinv <- function(inv) xinv <<- inv
			getinv <- function() xinv
			list(set = set, get = get,
				 setinv = setinv,
				 getinv = getinv)
}


## Solve and cache the matrix inverse of an object created with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
