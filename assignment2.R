## Caching the inverse of a Matrix

## makeCacheMatrix creates an object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  myX <- x
  
  getMatrix <- function() { return myX } #function that returns the matrix
  setInverse <- function(inverseX) { inverse <<-  inverseX }  
  getInverse <-  function() { return inverse }
  list( get=getMatrix, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the makeCacheMatrix
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  originalMatrix <- x$get() #get original matrix from makeCacheMatrix
  inverse  <- solve(originalMatrix) #
  x$setInverse(inverse) #save calculated inverse
  return inverse #print the inverse
}