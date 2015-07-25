## These functions are used to calculate and store the inverse of various matrices so that inverses don't have to be re-calculated.

## makeCacheMatrix is used to store a matrix and the associated list of functions to a global variable.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(sol) i <<- sol
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


##cacheSolve computes the inverse of the stored matrix, and stores the inverse in "i".

cacheSolve <- function(x) {
   i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  else{
    data <- x$get()
    i <- solve(data) 
    x$setinv(i)
    i
   }     
}
