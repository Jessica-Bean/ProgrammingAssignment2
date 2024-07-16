## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function will create a list that stores set, get, set the inverse and get the inverse value of matrix

makeCacheMatrix <- function(x = matrix()) { #creates a special
  # "matrix" object that can cache its inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)}
  
## makeCacheMatrix creates x's inverse and cache it with <<- operator

cacheSolve <- function(x, ...) { # computes the inverse of the special "matrix" returned by makeCacheMatrix above
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  # if the inverse has been calculated(and matrix unchanged),cacheSolve retrieve the inverse from the cache 
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}

