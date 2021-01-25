
## R Programming Week 3 Assignment

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object  can cache its inverse
  
  makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL                             ## initialize inv as NULL
    set <- function(y) {                    ## define the set function 
      x <<- y                             ## value of matrix 
      inv <<- NULL                        
    }
    get <- function() x                     ## define the get fucntion -
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv 
    getinverse <- function() inv                     ## gets the value of inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
  }
  
  
  
  cacheSolve <- function(x, ...) {
    ## Return inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }