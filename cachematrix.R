### Description: The purpose is to minimize computation time, given a large Matrix that
### needs to have it's inverse computed. If said Matrix is used alot, cache the result, 
### and use as needed to keep things moving quicker, than repeatedly calculating the
### inverse of a Matrix.

### 1st function:
###        makeCacheMatrix is a function that can compute the inverse of a matrix
###        and cache it, to be used over again, without costly computation time.

### 2nd function:
###        cacheSolve is a function that uses the matrix returned by makeCacheMatrix.
###        It has smarts to determine if matrix has already been cached or not.


makeCacheMatrix <- function(x = matrix()) {  
  ### some needed initialization before we begin
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  ### computes the inverse of the Matrix
  set_the_matrix <- function(solve) m <<- solve
  ### returns the stored  Matrix
  get_the_matrix <- function() m
  ### interfaces used by cache.Solve below to manipulate the Matrix
  list(set = set, get = get,
       set_the_matrix = set_the_matrix,
       get_the_matrix = get_the_matrix)
}


### This function determines if the Matrix being passed in as an argument,
### has already been cached, or has not been cache.

### If it's been cache, return the Matrix, otherwise
### compute the inverse of the Matrix and store for later use.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$get_the_matrix()
  ### If the cached Matrix exists, return it
  if(!is.null(m)) {
    message("getting cached data")
    # returns the cached Matrix, instead of computing it
    return(m)
  }
  ### Otherwise, compute the inverse and store it for later use.
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$set_the_matrix(m)
  m
}

### Testing the above
### 1. Create a basic matrix
### 2. Call makeCacheMatrix on this matrix and save in new matrix object
### 3. Pass this saved matrix to cachesolve - since it's the first
###    time, the inverse is computed and stored.
### 4. Pass this saved matrix to cachesolve again - since it's already
###    cached in step 3, this function simply returns the cache inverse
###    of the matrix.
