## Two functions: 
## makeCacheMatrix: Prepare a matrix and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize m to NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Function to return matrix
  get <- function() x
  
  ## Function to set inverse into m
  setInverse <- function(solve) m <<- solve(x)
  
  ## Function to get inverse
  getInverse <- function() m
  
  ## List of Functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve: Return the inverse(Solve) if there is no change to matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If m is not null then return cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## else set new Inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m  
}
