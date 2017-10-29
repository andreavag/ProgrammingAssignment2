#Creating makeChaceMatrix as an analogue to the mean function in example
makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
  x <<- y
  invert <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#Function that creates the invert matrix or caches if it is already calculated
cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("getting cached data.")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setinverse(invert)
  invert
}
