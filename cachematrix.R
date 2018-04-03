## makeCacheMatrix creates a "matrix" that is actually a list with a function
## the function sets the value of the matrix
## gets the value of the matrix
## sets the value of the matrix
## gets the value of the inverse

makeCacheMatrix <- function (x=matrix()) {
      i <- NULL
      set <- function(y){
        x<<- y
        i<<- NULL
      }
      get<- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse<- function () i
      list (set=set,
            get=get,
            setinverse=setinverse,
            getinverse=getinverse)
}

## this function will compute the inverse of the matrix created above. 
## it will retrieve the matrix from the cache if it has already been 
## calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
      
