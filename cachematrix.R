## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } #set the value of the matrix
  get <-function() x #get the value of the matrix
  setinverse <- function(inv) inv <<- inv #set the value of the inverse
  getinverse <- function() inv #get the value of the inverse
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } #check if already calculated
  data <- x$get()
  inv <- solve(data, ...) # if not, start calculate
  x$setinverse(inv) 
  inv
}
