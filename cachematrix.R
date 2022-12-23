## Below are two functions that are used to create a special object 
##that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a matrix containing a function to

##1. set the value of the matrix

##2. get the value of the matrix

##3. set the value of the inverse matrix using solve

##4. get the value of the inverse matrix using solve

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<- function() x
  setsolve <- function(solve) m <<- solve
  getsolve<- function() m
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)

}


## The following function calculates the INVERSE of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse has already 
##been calculated. If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the setsolve function.

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

##Example of how to run it to find inverse
##cacheSolve(makeCacheMatrix(x=matrix(1:4, nrow=2, ncol=2)))
