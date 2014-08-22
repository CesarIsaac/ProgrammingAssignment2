makeCacheMatrix <- function(x = matrix()) {   ##Creates the Matrix
  m <- NULL    ##  m will store our 'matrix' and it's reset to NULL every time
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x    ## this function returns the value of the original matrix
  setinverse <- function(solve) m <<- solve  ## this is called by cacheSolve() during the first cacheSolve()
  getinverse <- function() m ## this will return the cached value to cacheSolve()
  list(set = set, get = get,  ##  This list is returned with the newly created object. 
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {  ## the input is an object created by makeCacheMatrix
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")  ## ... send this message to the console
    return(m)   ## ... and return the matrix ... "return" ends 
  }
  data <- x$get()
  m <- solve(data, ...)  ##this gets the inverse of the matrix
  x$setinverse(m)
  m
}
