#For this assignment two functions are used to inverse a matrix, cache the results, and to retrieve it. 

#makeCacheMatrix creates an inverse matrix and cache's this inv matrix.

makeCacheMatrix <- function(x = matrix()) {
  imatrix <- NULL
  set <- function(y){
    x <<- y
    imatrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) imatrix <<-inverse
  getinv <- function() imatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function retrieves the cached inverse matrix, checks if the object is not a null object, and prints the result.

cacheSolve <- function(x, ...) {
  imatrix <- x$getinv()
  if(!is.null(imatrix)) {
    message("getting cached data")
    return(imatrix)
  }
  data <- x$get()
  imatrix <- solve(data, ...)
  x$setinv(imatrix)
  imatrix
}

# Check for if the functions work
mymatrix <- matrix(rnorm(9), 3 , 3)
myinvmatrix <- makeCacheMatrix(mymatrix)
cacheSolve(myinvmatrix)