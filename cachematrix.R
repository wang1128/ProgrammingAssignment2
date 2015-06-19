## This two functions are for solving the Assignment 2 of R Programming
## By Penghao Wang
## The first function is to make cache matrix and set inverse of the input Matrix
## The command should be like following
## A<-matrix(rnorm(16),4,4)
## B<- makeCacheMatrix(A)
## cacheSolve(B)
## cacheSolve(B)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) m <<- solve
  getInverseMatrix <- function() m
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
  
}


## This function check whether the Matrix Inversed or not. 
##If the Matrix haven't been inversed then the funtion create a inverse of Matrix
## If not, the "getting cached data" will be printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
}
