## Together, these two functions allow for the caching of a matrix and its inversion, but one does not work
## absent the other. To ensure proper function of "cacheSolve", "makeCacheMatrix" must be run first.

## This first function generates a list of functions that can modify a matrix, and its associated inversion.
## Important to note, this function does NOT calculate the inversion that it stores. It assumes that
## whatever matrix you feed it (as the inversion) has been correctly calculated elsewhere.

makeCacheMatrix <- function(x = matrix()){
  # Make an empty object for the inverse matrix to occupy
  invert <- NULL
  set <- function(y){
    x <<- y
    invert <<- NULL
  }
  get_data <- function() x
  set_inverse <- function(i) invert <<- i
  get_inverse<- function() invert
  list(set = set, get_data = get_data,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This second function works off of the first list of functions that were established to
## enable and empower it.

cacheSolve <- function(x, ...) {
  
  # First check if the existing "matrix" has a NULL inversion
  check_inv <- x$get_inverse()
  if(!is.null(check_inv)){
    
    ## If inverse is not NULL, then pull it from the cached value
    message("getting cached data")
    inv <- x$get_inverse()
    
    ## Define a variable, then spit out the variable from the environment
    inv
    
  # Set the else{} to match the if{}
 }else{
   
   # inform the user that this is a fresh operation
   message("calculating new inversion")
   
   # Get the data with the function previously described in the list
   invData <- x$get_data()
   
   # Calculate the inversion from that data
   inversion <- solve(invData, ...)
   
   # Set the inversion
   x$set_inverse(inversion)
   
   # Assign the inversion to a variable
   i <- x$get_inverse()
   
   # Report the inversion (a matrix that is the inverse of 'x')
   i
} }
