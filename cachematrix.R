## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } #set the value of the matrix
  get <-function() x #get the value of the matrix
  setinverse <- function(inv) m <<- inv #set the value of the inverse
  getinverse <- function() m #get the value of the inverse
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #check if already calculated
  data <- x$get()
  m <- solve(data, ...) # if not, start calculate
  x$setinverse(m) 
  m
}
