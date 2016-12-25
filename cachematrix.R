## 2 functions to calculate the inverse of a matrix and cache the results for 
## future reference. If the matrix has not changed, then the cached results are returned.

## makeCacheMatrix produces a list that can referred to set or get the matrix, and set or get
## solved inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function will test the makeCacheMatrix getsolve value to see if the matrix has
## been previously solved.  If not, then it will call the other values of the list to 
## calculate and set the inverse for future reference

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
