## Functions used to create matrix and cache inverse of this matrix
## to save working memory and time for repeatedly computing the inverse of large matrices

## Function to create the matrix and enable the functions "set","get","setinverse", and "getinverse"

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  y <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
    y <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to get inverse of the matrix stored in makeCacheMatrix

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
