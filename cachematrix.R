## These two functions demonstrate how the result of a potentially time consuming
## operation can be cached and later retrieved. In this example we compute the 
## inverse of a matrix

## The makeCacheMatrix function creates a special vector which contains methods to 
## get and set the values of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<-y
    i <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve takes the matrix and returns its inverse. The first time
## it is called for a particular matrix, the solve() function is called
## subsequent calls simply return the cached result assuming the matrix
## has not been changed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
