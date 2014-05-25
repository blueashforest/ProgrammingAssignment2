## This R function is able to cache potentially time-consuming computations when calculating the inverse
## of a matrix.

## The makeCacheMatrix defines and returns a list of functions: set the matrix, get the matrix,
## set the inverse matrix and get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The cacheSolve function calculates the inverse of the "matrix" created with the makeCacheMatrix.
## It first checks to see if the inverse of the matrix has been calculated already.  If it is available, 
## it gets the inverse matrix from the cache and skips the computation.  If not, it computes the inverse
## of the matrix and sets the inverse matrix in th cache via the setinversematrix function. 

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinversematrix(m)
  m
}

