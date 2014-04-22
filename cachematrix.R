## The following functions seek to cut down on the time calculating
## the inverse of a matrix by storing a previous matrix and its inverse
## and checking newly input matrices against these.

## The "makeCacheMatrix" function assigns to a variable a list of functions that
## do the following:
## 1. set/store the value of the matrix
## 2. get/retrieve the value of the matrix
## 3. set/store the inverse of the matrix
## 4. get/retrieve the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list (set = set, get = get, setinv=setinv, getinv = getinv)
}


## The "cacheSolve" function takes as input a variable created using the 
## "makeCacheMatrix" function. It checks whether a solution has already been
## calculated and stored. If yes, the solution is returned. If not, the
## solution is calculated, stored, then returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("Getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}
