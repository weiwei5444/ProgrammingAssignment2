## This function takes the inverse of a matrix, where the value of
## inverse can be cached so that when it is needed again, it can be
## looked up in the cache rather than recomputed.

## makeCacheMatrix creates a special "matrix", which can cahe the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set<- function(y){
    x<<-y
    i<<-NULL
  }
  get <-function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## cacheSolve calculates the inverse of the special "matrix" created with the 
## above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message(("gettng cache data"))
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
