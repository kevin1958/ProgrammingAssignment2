## Put comments here that give an overall description of what your
## functions do
# This programming assignment seeks the correct use and knowledge of construct
#funcions and demonstrate lexical scoping skills. Assignment has two parts: first,
#we create a function that cache the inverse of a matrix, and second one, retrieve
#the inverse from the cache.


## Write a short comment describing this function
# This function creates a special "matrix" object that can store a matrix and then 
#a cached its inverse, instead of recalculating. We need a pair of functions that
#cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse (to NULL)       
  inv <- NULL
  # Setter for matrix        
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter for matrix        
  get <- function() x
  # Setter for the inverse        
  setInverse <- function(inverse) inv <<- inverse
  # Getter for the inverse        
  getInverse <- function() inv 
  # Return functions of getter and setter as a list        
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the matrix returned by makeCacheMatrix
#function. If the inverse has already been calculated (and the matrix has not
#changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Get the store inverse from the cache
  inv <- x$getInverse()
  # If it exists that inverse in the cache, return it        
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # If it not exists, calculate, store and return it    
  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(inv)
  # Return the inverse        
  inv      
}