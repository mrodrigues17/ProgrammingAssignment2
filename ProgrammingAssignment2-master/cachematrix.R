## These functions first create a special matrix, then caches the
## inverse of the matrix

## this function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix())  {
  m <- NULL
  set <- function(y) {
    x<<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix that "makeCacheMatrix" returns.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...)  {
  m <- x$getinverse()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Feel free to test
