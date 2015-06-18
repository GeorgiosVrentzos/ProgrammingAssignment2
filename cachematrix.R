## Matrix inversion is usually a time consuming computation. The idea behind the "makeCacheMatrix"
## and the "cacheSolve" functions is to allow users to cache the inverse of a matrix 
## in the scenario that the contents of the matrix are not changing

## My code was based on the "Caching the Mean of a Vector" example. I copied the two functions 
## and adjusted the code accordingly by replacing the Mean calculation with the one 
## for Inversing a Matrix

## [1] The "makeCacheMatrix" function creates a special matrix object that can cache it inverse 
## The object is really a list containing a function to Set and Get the value of the matrix and
## Get and Set the value of the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
  


## [2] The "cacheSolve" function computes the inverse of the special matrix  returned by the previous (above) 
## function "makeCacheMatrix. If the inverse has already been calculated earlier, the cachesolve 
## will get the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}  
## Example: Normal Matrix vs  Inverse Matrix
##  matrix(10:13,2,2)     vs  cacheSolve(makeCacheMatrix(matrix(10:13,2,2)))
##        [,1]  [,2]                [,1]    [,2]
##  [1,]   10    12           [1,]   -6.5     6
##  [2,]   11    13           [2,]    5.5    -5
