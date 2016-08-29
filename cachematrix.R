## Put comments here that give an overall description of what your
## functions do

## --------------------------------------------------------------
## invertibleMatrix
##
## This Functions  
## Check if the matrix has square dimentions
invertibleMatrix <-function(x){
  dim(x)[1] == dim(x)[2]
}

## --------------------------------------------------------------
## makeCacheMatrix
## Function based on the VectorMake example 
## available at: https://github.com/rdpeng/ProgrammingAssignment2
## 
## This function validates if the matrix is invertible
## Then initializes the inverse property
## Declares a set method for the matrix
## Declares a get method for the matrix
## Declares a set method to set the inverse of the matrix
## Declares a get method to get the inverse of the matrix
## Returns the inverse property
## Return a list of the methods
## --------------------------------------------------------------
makeCacheMatrix <- function( m = matrix() ) {
  if (invertibleMatrix(m)) {
    i <- NULL
    set <- function( matrix ) {
      m <<- matrix
      i <<- NULL
    }
  
    get <- function() {m}
    setInverse <- function(inverse) { i <<- inverse }
    getInverse <- function() { i }
  
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }
}

## --------------------------------------------------------------
## cacheSolve
## Function based on the cacheMean example 
## available at: https://github.com/rdpeng/ProgrammingAssignment2
## 
## Return a matrix that is the inverse of 'x'
## This function sets the inverse of the matrix to m
## Then verifies if m is not null
## if so it will return the value of m
## Then it gets the value of x and assign it to data
## solves the matrix inverse by appling the function solve()
## sets the value of the inverse to m
## Returns the inverse property
## --------------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("retrieving cached data ... ")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}