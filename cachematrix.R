## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## inv will store the catched inverse matrix
  inv <- NULL
  
  ## set for matrix
  set <- function(y) {      
    x <<- y
    inv <<- NULL   
  }
  
  ## get function for matrix
  get <- function() x
  
  ## set for the inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## get for the inverse
  getinv <- function() inv
  
  ## Return the matrix with new defined functions
  list(set = set , get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve : It will compute the inverse of the matrix. It returns the catched inverse, if the inverse is already calculated before.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  
  ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
    
  }
  
  # Calculate the inverse and cache the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(inv)
  
  # Return it
  inv
  
  
}
