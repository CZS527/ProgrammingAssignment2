## These functions work together to create a matrix object that can cache its own inverse.  
## Initially


## Create the matrix object storing the value of a matrix.  Optionally store the value of the inverse.
## Interact with object using functions described below.

makeCacheMatrix <- function(x) {
  i <- NULL
#  print(environment())
#  evn <- environment()
#  print(parent.env(evn))
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv = NULL) i <<- inv
  getinv <- function() i
#  getevn <- function() environment()
  matObj <- list(
    set = set, 
    get = get, 
    setinv = setinv, 
    getinv = getinv)
  matObj
}

## Return cached inverse matrix, if it exists.  Otherwise calculate and returns the inverse matrix.

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
