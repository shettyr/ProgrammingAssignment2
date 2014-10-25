## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
#This function caches the inverse value of the input matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL;
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(value) inv <<- value
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
#This function returns inverse value of a matrix from cache . 
#On cache miss it computes the inverse value of a matrix and stores in cache.
#
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse of matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv  
}
