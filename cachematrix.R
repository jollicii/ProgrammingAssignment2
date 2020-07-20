## Put comments here that give an overall description of what your
## functions do
## The following functions create and cache matrix inverse
## So that future calculation could be easier

## Write a short comment describing this function
## makeCacheMatrix creates and cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  
  ## setting up m as NULL
  m <- NULL
  
  ## setting up set() to define x and m
  set <- function(y){
    x <<- y
    m <<- NULL
  } 
  
  ## setting up get() to retrieve value of x 
  get <- function() x
  
  ## setting m as inverse of x
  setinverse <- function(inverse) m <<- inverse
  
  ## setting up getinverse() to retrieve value of m
  getinverse <- function() m
  
  ## make a list to contain what's produced above
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve retrieves the cached inverse of matrix

cacheSolve <- function(x, ...) {
  
  ## retrieving inverse
  m <- x$getinverse()
  
  ## returning m to parent environment if m is a NULL
  if(!is.null(m)){
    message("getting matrix inverse")
    return(m)
  }
  
  ## calculating inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        
}
