## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

c=rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(c)

## m$getinv()


## Compute the inverse and save it in the cache for next time

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getinv()
  if (is.null(inv)) { 
    message("Computing inverse")
    m <-x$get()
    inv <- solve(m) 
    m$setinv(inv)
    inv
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

