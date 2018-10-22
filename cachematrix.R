## Description:
## My functions create an object to cache and compute the inverse of the matrix.

## This function creates a 'matrix' object to cache the inverse of matrices.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x <<- y
    inv <<- NULL
  }
  get<-function() x
  setInv <- function(solve) inv<<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv, 
       getInv = getInv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## It retrieves the inverse from the cache,
## if the previous function has already calculated inverse. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <-x$getInv()
  if(!is.null(inv)){
    message("getting cache data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setInv(inv)
  inv
}
