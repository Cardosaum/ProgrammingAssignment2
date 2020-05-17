## Matheus, 2020

##
## `makeCacheMatrix` creats a list that contains a function to get an set a matrix and its inverse.
## and, besides this, also use matrix inverseMatrix to store the inverse of matrix.
##

makeCacheMatrix <- function(x = matrix()) {

  # initialize inverseMatrix
  inverseMatrix <- NULL

  # sets a matrix 
  set <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
  }

  # gets a matrix
  get <- function() { x }

  # sets and also gets its inverse
  setInverse <- function(inverse) { inverseMatrix <<- inverse }
  getInverse <- function() { inverseMatrix }

}


## 
## `cacheSolve` checks if the inverse of a given matrix exist in cached format.
## if it indeed exists, then it is returned. 
## else, the function calculates its inverse.
##


cacheSolve <- function(x, ...) {
    
  # first, we check if its inverse exists
  inverseMatrix <- x$getInverse()
  if (!is.null(inverseMatrix)) {
      message("We have the cached data!\nRetriving it...")
      return(inverseMatrix)
  }

  # if we can't find the inverse, we need to calculate it
  # here, we use `solve` to calculate its inverse
  message("We'll need to calculate the inverse of this matrix...")
  inverseMatrix <- solve(x$get(), ...)
  x$setInverse(inverseMatrix)
  return(inverseMatrix)
}
