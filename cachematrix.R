## The following two functions create a matrix object
## with a caching inverse function

## makeCacheMatrix creates a matrix object defined by four methods:
## get, set, setinv, getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve(matrix) returns the inverse of a matrix, pulling it from
## the cache if it exists and creating (and caching) it otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) return (inv)
  inv = solve(x$get(), ...)
  x$setinv(inv)
  inv
}
