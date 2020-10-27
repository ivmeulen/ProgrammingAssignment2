## Together, these two functions allow for the caching of a matrix and its inversion, but one does not work
## absent the other. To ensure proper function of "cacheSolve", "makeCacheMatrix" must be run first.

## This first function generates a list of functions that can modify a matrix, and its associated inversion.
## Important to note, this function does NOT calculate the inversion that it stores. It assumes that
## whatever matrix you feed it (as the inversion) has been correctly calculated elsewhere.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get_data <- function() x
  set_inverse <- function(i) inversion <<- i
  get_inverse<- function() inversion
  list(set = set, get_data = get_data,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This second function works off of the first list of functions that were established to
## enable and empower it.

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null( i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
