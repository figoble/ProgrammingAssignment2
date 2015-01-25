## Programming Assignment_2: Lexical Scoping
## Below two functions that cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that caches its inverse..
## It contains the following functions and returns the list of them:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## set up empty matrix x and a NULL inverse
  inverse <- NULL
  
  ## 1) set function sets the value of the matrix by assigning matrix and resetting inverse to NULL when function is called
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  ## 2) get function returns the value of the matrix 
  get <- function() x
  
  ## 3) setinverse assigns the value of inverse of the matrix
  setinverse <- function(solve) inverse <<- solve
  
  ## 4) get the value of the inverse
  getinverse <- function() inverse
  
  ## return list of all functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
  
  
## cacheSolve function computes the inverse of the special "matrix" that is set by the makeCacheMatrix function. 
## If the inverse has already been calculated and the matrix has not changed, then the cacheSolve function retrieves the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## retrieve value of the inverse
  inverse <- x$getinverse()
  
  ## check if inverse was calculated and if matrix has changed
  ## when inverse already previously calculated, then return cached value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse value is NULL, then calculate inverse from matrix, x, and return new value
  data <- x$get()
  inverse <- mean(data, ...)
  x$setinverse(inverse)
  inverse
}