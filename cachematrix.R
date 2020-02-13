## functions to create matrix and calculate its inverse and then cache 
## the value of that inverse

## function in which you can create a matrix, get that matrix, 
## set its inverse and obtain its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # Create a matrix
  creatematrix <- function(m){
    x <<- m
    inverse <<- NULL
  }
  # Getting the matrix created
  get <- function() x
  ##Setting the inverse & saving the value in the parent enviroment 
  ##(makeCacheMatrix function)
  setinverse <- function(inv) inverse <<- inv
  # getting the inverse
  getinverse <- function() inverse
  
  list(creatematrix = creatematrix, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Function to calculate the inverse of a given matrix & caching that inverse

cacheSolve <- function(x, ...) {
  #Get the current inverse of the matrix
  y <- x$getinverse()
  #if the inverse already exist then return it's value
  if(!is.null(y)){
    message("the inverse has already been calculated. Getting value...")
    return(y)
  }
  # the value of "data" is the matrix
  data <- x$get()
  # calculate the inverse of "data" with "solve" function
  y <- solve(data, ...)
  #save the value of "y" (inverse) in the "inverse" variable of the matrix with 
  #the "setinverse" function of that matrix
  x$setinverse(y)
  #return the inverse
  y
}