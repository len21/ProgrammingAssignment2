# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # set x = empty matric
  # set inverse variable to null
  mtxInverse <- NULL
  
  # Create a new method called "set" that set the value of the matrix
  set<-function(y){
    x <<- y
    # set function assigns the argument to x
    mtxInverse <<- NULL
    # reset Inverse cto NULL
  }
  
  # Create a new method called "get" that get the value of the matrix
  get <- function() x
  
  # Create a new method called "setmatrix" that get the inverse of variable mtxInverse
  setmatrixInverse<-function(solve)  mtxInverse <<- solve
  
  # Create a new method called "getmatrix" which returns the inverse matrix
  getmatrixInverse <- function() mtxInverse
  
  # return a list with the four methods/functions
  list(set = set, get = get, setmatrixInverse = setmatrixInverse, getmatrixInverse = getmatrixInverse)
}


# calculates the mean of the special "matrix" created with the in the function makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  # get the value of the inverse
  mtxInverse <- x$getmatrixInverse()
  
  # If the value of Inverse is NOT null returns that value 
  if(!is.null(mtxInverse)){
    message("getting cached data")
    return(mtxInverse)
  }
  
  # since mtxInverse is null (aka not cached) set data = to the value of x and calculate the inverse
  data <- x$get()
  
  # set mtxInverse = Inverse of x
  mtxInverse <- solve(data, ...)
  
  # set the new Inverse value to cache
  x$setmatrixInverse(mtxInverse)
  
  # Returns the new Inverse value
  mtxInverse 
  
}
