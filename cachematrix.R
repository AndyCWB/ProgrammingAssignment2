## These functions define a matrix which caches it's own inverse
## Written for the Coursera R programming course.
## Two functions: 
##      makeCacheMatrix created a cached matrix from a normal matrix
##      cacheSolve - returns the inverse, computed using solve() and cached for subsequent calls

## makeCacheMatrix(x) creates a cached Matrix from a normal matrix
## E.g., c = rbind(c(1, -1/4), c(-1/4, 1))
##       m = makeCacheMatrix(c)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x      ## returns matrix 
  getinv <- function() inv
  setinv <- function (i) inv <<- i  ## Updates stored inverse
  list(set = set, get = get,
       getinv = getinv, setinv=setinv)
}

## Compute the inverse and save it in the cache for next time
## cacheSolve(m) 

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (is.null(inv)) { 
    message("Computing inverse")
    m <-x$get()
    inv <- solve(m)    # this is the actual inverse computation 
    x$setinv(inv)
    return(inv)
  }
  else { 
    message ("returning cached value")
    return (inv)
  }
  return 
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

