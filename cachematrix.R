## Great a matrix and can cache its inverse to avoid 
## repeated computation

## This function creates a special "matrix" 
## object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinver <- function(inverse) m <<- inverse
    getinver <- function() m
    list(set = set, get = get,
         setinver = setinver,
         getinver = getinver)

}


## return the inverse of the matrix by catching the value or 
## computing if it has not been computed.

cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinver(m)
  m
}
