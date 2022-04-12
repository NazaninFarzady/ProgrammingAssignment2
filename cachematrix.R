## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates an instance of a square/invertible matrix
## nvIt takes a matrix saved in the private variable 'x'
## Returns a list of public functions to set and get the values of matrix
## calculates, saves/caches, and returns the inverse of matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  
  # Initial the inverse to NULL
  invMat <- NULL
  
  # Set new values of matrix and clear cached inverse
  set <- function(y){
    x <<- y
    invMat <<- NULL
  }
  
  # Return matrix elements
  get <- function() x
  
  # Calculate the inverse of matrix elements
  setInverse <- function(solve) invMat <<- solve
  
  # Return the inverse of matrix
  # It will be NULL if cacheSolve has not been called or new matrix values have been set
  getInverse <- function() invMat
  
  # Return value of makeCacheMatrix is a list of functions that are public
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}




## Write a short comment describing this function

## The cacheSolve function uses the public functions of makeCacheMatrix
## It takes an instance of a matrix created above,
## If the matrix has not changed and the inverse was already calculated, it returns the inverse of matrix
## Otherwise, returns the freshly calculated inverse and also caches it future calls

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  invMat <- x$getInverse()
  
  # Return the cached value of inverse (if the inverse of matrix has been calculated, not changed, and saved)
  if(!is.null(invMat)){
    message('Getting cached data')
    return(invMat)
  }
  
  # Otherwise, calculate the fresh inverse and cache it for later calls
  # Assign the elements of matrix 'x' to 'mat'
  mat <- x$get()
  
  # Calculate the inverse of matrix
  invMat <- solve (mat)
  
  # Save (cache) the calculated inverse so it does
  x$setInverse(invMat)
  
  # Return the cached inverse of matrix 'x'
  invMat
  
  }
