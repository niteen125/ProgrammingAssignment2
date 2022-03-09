## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## x as matrix 
##inversematrix will store inverse of x

makeCacheMatrix <- function(x = matrix()) {
  
  inversematrix <- NULL
  set <- function(y) 
  {
    x <<- y
    inversematrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inversematrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## caching inverse matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)) 
  {
    message("Inverse matrix cached result")
    return(inversematrix)
  }
  data <- x$get()
  inversematrix <- solve(data, ...)
  x$setinverse(inversematrix)
  inversematrix
}
