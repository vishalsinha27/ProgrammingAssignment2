## makeCacheMatrix provides ability to cache the matrix and its inverse. 
## CacheSolve returns the inverse of the matrix either from the cache or calculate and return it.
## Usage : a <- matrix() ; cacheSolve(makeCacheMatrix(a)) returns the inverse of the matrix a and store it to cache for future call. 

## makeCacheMatrix function takes matrix as an argument. user can also set matrix by calling the set method
# setInverse method will store the inverse of the matrix
## getInverse will return the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  } 
  
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## cacheSolve function first checks if the inverse of the matrix is stored in the cache by calling makeCacheMatrix getInverse method.
## If it is not then it calculates the inverse by calling solve method and store it to cache.
## Returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting data from cache")
          return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}
