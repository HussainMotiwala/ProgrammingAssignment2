## Finding the inverse of a matrix is a time consuming operation, hence the
## below functions try to retrieve matrix inverse of already solved matrix from
## cache and actually solve for inverse only when the solution is not available
## in the cache
## makeCacheMatrix creates a special vector which return functions to get the 
## original matrix, set a value of matrix, get the matrix inverse and set a 
## value for marix inverse
## CacheSolve retrieves the solution from cache of the input matrix and if not 
## available finds the inverse of the matrix using solve function

## Using <<- operator which can be used to assign a value to an object in an 
## environment that is different from the current environment, set function
## sets the value of the matrix to the input, and its inverse to null, get 
## function is used to retrieve the martix, setinverse function also uses <<- 
## operator to set the value of inverse and getinverse to retrieve the value of 
## inverse stored in the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function first tries to retrieve the invers of the input from cache
## if the inverse is available, it prints getting cached data, and in case it is
## not available i.e. it is null it uses solve() function to compute the inverse
## and stores the computed inverse in the cache, and then returns the computed
## value

cacheSolve <- function(y, ...) {
  inverse <- y$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- y$get()
  inverse <- solve(data,...)
  y$setinverse(inverse)
  inverse
}
