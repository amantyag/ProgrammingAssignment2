## The 2 functions calculates the inverse of the input matrix. This is for the coursera 
## assignment 2 of R programming course in Coursera. Matrix supplied must be
## invertible. 
## eg:on console type : m1 <- matrix(sample.int(1000,size=16,replace=TRUE), nrow=4)
##x1 <- makeCacheMatrix(m1)
## cacheSolve(x1)

## It creates cache of the matrix if it does not exist earlier, otherwise 
## returns the previous cached one

makeCacheMatrix <- function(x = matrix()) {
  dum <- NULL
  set <- function(y){
    x <<- y
    dum <<- NULL
  }
  get <- function() x
  setinv <- function(solve) dum <<- solve
  getinv <- function() dum
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## It returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  dum <- x$getinv()
  if(!is.null(dum)) {
    message("getting cached data")
    return(dum)
  }
  data <- x$get()
  dum <- solve(data, ...)
  x$setinv(dum)
  dum
}
