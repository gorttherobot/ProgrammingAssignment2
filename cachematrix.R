## This is is the second programming assignment for Coursera

## This function creates an object which can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      inv<- NULL
      set <- function(y){
            x<-y
            inv<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv<<- inverse
      getinv <- function() inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## returns the inverse of the special matrix above. It also caches the inverse in y$getinv()
## so it can be fetched later.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
      } else {
            mat <- x$get()
            inv <- solve(mat, ...)
            x$setinv(inv)
      }
      inv
}
