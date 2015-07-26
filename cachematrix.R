## Coursera Data Science Specialization Track from the Johns Hopkins School for Public Health
## Programming Assignment 2 - R Programming

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly .
## The following two functions are used to calculate the inverse of a matrix and to 
## cache the result.


makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: A function to create a special "matrix" object that can cache its inverse.
  ## Args: 
  ##   x: a square invertible matrix
  ## Returns: 
  ##   A list containing functions to
  ##    1. set the matrix
  ##    2. get the matrix
  ##    3. set the inverse
  ##    4. get the inverse
  ##  This list will be used as the input to cacheSolve()

  inverseM <- NULL
  set <- function(y) {
    # `<<-` will assign a value to an object in an environment different to the current environment.     
    x <<- y
    inverseM <<- NULL
  }
  
  get <- function() x
  setInverseM <- function(solve) inverseM <<- solve
  getInverseM <- function() inverseM
  
  return(list(
    set = set, 
    get = get,
    setInverseM = setInverseM,
    getInverseM = getInverseM))
  
}


cacheSolve <- function(x, ...) {
  ## cacheSolve: This function computes the inverse of the special "matrix" returned by 
  ##  makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
  ##  then cacheSolve should retrieve the inverse from the cache.
  ## Args: 
  ##   x: output of makeCacheMatrix()
  ## Returns: 
  ##   The inverse of the original matrix input to makeCacheMatrix()  

  inverseM <- x$getInverseM()
  
  # Check if the inverse has already been created
  if(!is.null(inverseM)) {
    message("getting cached inverse matrix")
    # Return the inverse matrix and exit the function0
    return(inverseM)
  }

  # Inverse has not been cached, so get the data and 
  # calculate the inverse
  data <- x$get()
  inverseM <- solve(data, ...)
  
  # set the inverse in the cache
  x$setInverseM(inverseM)
  return(inverseM)
  
}
