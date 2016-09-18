## Below are two functions that are used to create a special object
## that stores a matrix and caches its inverse 

## The first function, makeCacheMatrix creates a special matrix, 
## which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(Inv) m <<- Inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse of the matrix 
## created with the abovection. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache. 
## If not, it calculates the inverse of the data and sets the 
##value of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
