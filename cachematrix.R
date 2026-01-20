## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will be used as a 'box' for storing the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i){
    inv <<- i
  }
  getinv <- function() inv
  list(set = set, get =get, setinv = setinv,getinv=getinv)

}


## Write a short comment describing this function
## This function creates the inverse of a matrix and stores it in the 'box' if it's empty.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
