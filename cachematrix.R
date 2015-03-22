## These functions work together to create a matrix object that can optionally cache its own inverse.  
## The makeCacheMatrix  function initializes the matirix object.  The inverse is initially set to NULL
## The cacheSolve function checks whether the inverse matrix is null.  If so, it calculates and caches the inverse.
## Otherwise, it returns the cached value of the inverse matrix.

## Create the matrix object storing the value of a matrix.  Optionally store the value of the inverse.
## Interact with object using functions described below.

makeCacheMatrix <- function(x) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv = NULL) i <<- inv
  getinv <- function() i
  matObj <- list(
    set = set, 
    get = get, 
    setinv = setinv, 
    getinv = getinv)
  matObj
}

## Return cached inverse matrix, if it exists.  Otherwise calculate, caches, and returns the inverse matrix.

cacheSolve <- function(x) {
  i <- x$getinv()
  if( !is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
        ## Return a matrix that is the inverse of 'x' 
  i
}
