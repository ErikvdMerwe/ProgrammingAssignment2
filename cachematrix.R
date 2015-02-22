## The following functions were compiled in comletion of Programming Assignment 2 of 
## R Programming. The first function creates a square variable. while the second function
## calculates its inverse, whilst first checking the cache if the solution was not calculated
## on a previous occassion.

## The following function receives a square matrix as input is used to cache that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {  
  matrixInverse <- NULL
  y <- NULL
  
  setMatrix <- function(y) { 
    x <<- y
    matrixInverse <<- NULL
  }
  
  getMatrix <- function () x
  
  setInv <- function(matrix, matrixInverse){
    matrix <<- x
    inv <<- matrixInverse
  } 
  
  getInv <- function(x) 
    inv

  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}

## The following function determines if a value was cached on a previous occassion and returns
## the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv
  if(!is.null(inv) && identical(x$getMatrix(),x$setMatrix())) {
    message("Retrieving cached data:\n")
    return(inv)
  }
  
  else{
    message("Calculating inverse...")
    
    y <- x$getMatrix()
    x$setMatrix(y)
    inv <- solve(y, ...)
    x$setInv(inv)
    
    return(inv)
  }
}
