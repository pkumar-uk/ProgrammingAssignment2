## Perumal Kumar 25th July, 2014
## 
## makeCacheMatrix(x) where x is a matrix to be inverted, it returns a list of 4 funtions and initalises the variables 
## variable to store the inverse matrix
##
## the four functions returned are
## set(y) can be used to reset matix for which inverse need to be calculated
##        if the new matrix being set same as old then no update happens and a message 
##        is returned
## get() returns the current matrix in memory
## setinverse(m) moves the inverse of the matrix to memory
## getinverse() returns the inverted matrix stored in cache
##
## cacheSolve function takes the list returned by makeCacheMatrix as input and returns the 
## inverse of the matrix. It checks if the inverse is already cached, returns cached value
## with message or else calculates inverse
##
## matequal(x,y) function takes two matrix as input and checks if they are the same
## returns TRUE or FALSE

## Usage
## create a matrix for which inverse needs to be found
## pass the matrix to makeCacheMatrix function whose return value is stored in a list
## use the list returned from makeCacheMatrix to call cacheSolve function to get inverse of the 
## matrix
## 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
      if(matequal(y, x)) {
        message("Matrix same as previous input")
        if (is.null(m)) {
          message("Inverse of Matrix not yet calculated")
          invisible(m)
        } else {
          message("Inverse of matrix in cache - it is shown below")
          return(m)
        }
        
      }
     
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(invert) m <<- invert
  
  getinverse <- function() m

  if (!is.null(x)) x<<- x
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

matequal <- function(x, y) {
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}
