## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Created functions that create, cache, and retrieve the inverse of a matrix.

## This function will create the matrix that will cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL                ##set the matrix_inverse by NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse  <- function(solve) matrix_inverse <<- solve
  getinverse  <- function() matrix_inverse
  list(set = set, get = get,
       setinverse  = setinverse,
       getinverse  = getinverse )
}


## This function that will get the inverse of the matrix if it has not been previously computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse <- x$getinverse()                     ## check if the matrix_inverse is coputed before
          if(!is.null(matrix_inverse)) {
            message("getting cached data")
            return(matrix_inverse)
          }
          data <- x$get()                                    ## get the matrix to compute it's inverse
          matrix_inverse <- solve(data, ...)                 ## compute the matrix_inverse
          x$setinverse(matrix_inverse)                                    ## set matrix_inverse of matrix x
          matrix_inverse
}
