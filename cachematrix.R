## Put comments here that give an overall description of what your
## functions do

### makeCacheMatrix is a function that can create a matrix and cache it, to be used over again, without costly computation time.

### cacheSolve is a function that uses the matrix returned by makeCacheMatrix. It has smarts to determine if matrix has already been cached or not.

## Write a short comment describing this function

### This function creates a special matrix object that can cache
### its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(setinverse) m <<- setinverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

### This function computes the inverse of the special matrix
### returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
