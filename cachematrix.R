makeCacheMatrix <- function(x = matrix()) {

}
## These are functions that cache the inverse of a matrix
## A special matrix object is created to cahce its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Create the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    m
  }
  
  ## Set the matrix inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the matrix inverse
  getInverse <- function() {
    i
  }
  
  ## List the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Get the special matrix inverse from "makeCacheMatrix". If the inverse has 
## been created already, "cachesolve" will retrieve the cache inverse. 

cacheSolve <- function(x, ...) {
  
  ## Create an inverse matrix of 'x'
  m <- x$getInverse()
  
  ## Return the set inverse
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Create a matrix
  data <- x$get()
  
  ## Use matrix multiplication to calcuclate the inverse
  m <- solve(data) %*% data
  
  ## the inverse to the object is set
  x$setInverse(m)
  
  ## Return matrix
  m
}

