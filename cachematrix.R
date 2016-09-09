## This function caches the inverse, allows to set the inverse of a matrix
## It also allows to get and set the values of the dat
## The function can be accessed through a series of get and set commands both for getting/setting data as well as inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinverse <- function(inv) i <-- inv
  getinverse <- function() i 
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the cached inverse of matrix. If inverse is not cached then it calculates it and the populates cache as well

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i = x$getinverse
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get
  i <- solve(x)
  x$setinverse(i)
  i
}
