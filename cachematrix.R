## Rprograming, Assignment 2

## These functions [1] consruct an artificial matrix by analogy to makeVector
## [2] given matrix as input, check cache for its inverse;
## if found, return it, else calculate the invere and cahe it

## This function creates the artificial matrix, which is a lst of four functions, set, get, setinverse and getinverse
## what each does is explained below inline

## To use these functions, first assign to a variable with makeCacheMatrix
## as in
##   x <- matrix(c(1,2,33,49),nrow=2,ncol=2); b <- makeCacheMatrix(x)
## then you can get the inverse with cacheSolve(b)
## and agai,n but this time from cache!! Woo hoo.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}





## This function does the  inverting and caching, or fetching the inverse, as required

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
  
  m <- x$getinverse()
  if(!is.null(m)) {
    
    ## if the inverse is in cache, m is given its value which is returned
    message("getting cached data") 
    return(m)
  }
  ## otherwise, the inverse function (solve) is appled to x
  data <- x$get()  
  m <- solve(data)
  
  ## the inverse just found is cached and returned
  x$setinverse(m)  
  m
  
  
}
