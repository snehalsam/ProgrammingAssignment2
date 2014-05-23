## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list 'checkmatrix' in the global environment 
## where the original matrix is stored and an empty matrix placeholder for 
## the inverse are created

makeCacheMatrix <- function(x = matrix()) {
  
  checkmatrix <<- list ( original = x, inverse = matrix())
  
}


## cacheSolve first checks if the matrix is the same as the original.
## In case the matrix has changed then the cached inverse should not
## be fetched. If the matrix has not changed then it checks if a
## cached inverse exists and returns it. Otherwise it solves the inverse
## and returns it as well as caching it for future reference.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  checkorg <- identical(checkmatrix$original, x)
  checkinv <- checkmatrix$inverse
  
  if (checkorg == TRUE) {
    
    if (!is.na(checkinv)) {
      message("getting cached data")
      return(checkinv)
    }
  }
  
  
  temp_inverse <- solve(x)
  checkmatrix$inverse<- temp_inverse
  return(temp_inverse)
  
}
