## First function, makeCacheMatrix() creates an R object that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinve <- function(inve) m <<- inve
  getinve <- function() m
  list(set = set, get = get,
       setinve = setinve,
       getinve = getinve)  
}

##The second function, cacheSolve() requires an argument that is returned by makeMatrix() in order to
##retrieve the inverse matrix from the cached value that is stored in the makeMatrix() object's environment.
cacheSolve <- function(x, ...){
  m <- x$getinve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinve(m)
  m
}
