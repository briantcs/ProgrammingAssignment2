## This function creates a special "matrix" object that can cache its inverse.
## This function first set the value of the matrix
## Thenn get the value of the matrix
## Thirdly set the value of inverse of the matrix
## Lastly get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
    x <<-y
    m<<-NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve=setsolve,
       getsolve=getsolve)
  
}


## The following function calculate inverse of an invertible special "matrix"
## It first checks to see if inverse has already been calculated
## If so, it gets the inverse from cache and skips the computation
## Otherwise, it calculates the inverse and the value of inverse in the cache via the setinverse function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}
