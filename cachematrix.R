
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  
  # Set the stored inverse to NULL
  inv_cont <- NULL
  
  # Set the values of the matrix
  set <- function(y) {
    x <<- y
    inv_cont <<- NULL # if this is met then the matrix has changed
  }
  
  # get the matrix
  get <- function() x
  
  # set the inverse of the matrix 
  set_inverse <- function(inverse) inv_cont <<- inverse
  


  # get the inverse of matrix
  get_inverse <- function() inv_cont
  


  # freturning a list the functions defined earlier in this script.
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  
  # get the inverse
  
  inv_cont <- x$get_inverse()
  
  # check to see if inverse already exists & check if already cached - if yes, return cached inverse


  if(!is.null(inv_cont)) {
    message("getting cached data...")
    return(inv_cont)
  }
  
  # if no then get the matrix
  data <- x$get()

  
  # compute inverse of matrix
  inv_cont <- solve(data, ...)

  
  # cache the inverse of matrix
  x$set_inverse(inv)
  
  # return the inverted matrix
  inv_cont
}
