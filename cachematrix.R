## makeCacheMatrix creates a special "matrix".
## cacheSolve gets or computes the inverse of matrix created by makeCachMatrix. 

## makeCacheMatrix creates a special "matrix", which is a list 
## containing functions to set the value of matrix, get the value
## of the matrix, set the inverse of matrix, and get the inverse of 
## the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x<<-y
    inver<<- NULL
  }
  get <- function() x
  setinver <- function(inverset) inver <<- inverset
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver, getinver = getinver)
}


## Function calculates the inverse of a special "matrix" object 
## created by makeCacheMatrix function. If the inverse of the matrix 
## is already calculated before, it gets the inverse matrix from the cache
## and skip the computation. Otherwise, it calculate the inverse of matrix
## and sets the inverse of matrix in the cache via the setinver function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data,...)
  x$setinver(inver)
  inver
}
