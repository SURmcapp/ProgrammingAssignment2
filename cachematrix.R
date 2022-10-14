## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. 

## Here is a function to create a special 'matrix' object that can cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## - This is a function to compute the inverse of the special 'matrix' returned by
##   the makeCacheMatrix function above.  
## - If the matrix has not changed and the inverse has already been calculated
##   then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mtx <- x$get()
  i <- solve(mtx, ...)
  x$setinverse(i)
  i
}
