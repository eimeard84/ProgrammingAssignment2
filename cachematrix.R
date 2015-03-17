## When a matrix x is passed to makeCacheMatrix, it stores both x and 
## the inverse of x in a new "cache matrix". 
## If the "cache matrix" output of makeCacheMatrix is passed to cacheSolve, 
## the inverse of x will either be retrieved from the cache matrix, or 
## (if the stored inverse does not exist or refers to another matrix) 
## calculated from scratch

## Make a new "cache matrix" object which stores both the initial matrix x
## and its inverse

# Usage: y <- makeCacheMatrix(x)
#        x_inv <- cacheSolve(y)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
   # m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Takes a "cache matrix" output by makeCacheMatrix and (if it exists and 
## still refers to the most recently cached matrix) retrieves the inverse, 
## or else calculates it from scratch

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Except technically it returns a matrix that is the inverse of the 
  # matrix you originally passed to the makeCacheMatrix function, 
  # and you need to pass that "cache matrix" to this function
  
  # install.packages("compare",lib="/tmp")
  # library(compare, lib.loc="/tmp")
  # IF NECESSARY, UN-COMMENT THESE LINES TO ACQUIRE THE COMPARE PACKAGE
  library(compare)
  # Need to load this library to compare two matrices
  
  data <- x$get()
  # Retrieve the actual matrix from the "cache matrix"
  m <- x$getinv()
  # Retrieve the stored inverse matrix from the "cache matrix"
  
  if (!is.null(m)) {
  # Checks whether the inverse matrix exists
    q <- (compareEqual((m%*%data), diag(sqrt(length(data)))))
    # Checks whether it is the inverse of the current matrix
    # i.e. that their product is the identity matrix
    # (Assumption included in assignment: any matrix passed
    # to the function will be invertible => square)
    # compareEqual allows for a small margin of error, so the 
    # identity matrix is not required to consist only of 1s and 0s
    if (as.logical(q[1])) {
    # Even though only one logical is returned by compareEqual, 
    # it is in the form of a list and so must be altered
    # for the if statement
        message("getting cached data")
        # This is kind of a lie because we already got the cached data,
        # but whatever, it lets people know this part of the code is
        # being called
        return(m)
        # If the stored inverse matrix is the inverse of the current
        # stored matrix, return the stored inverse matrix
  }
}

m <- solve(data, ...)
# If there is no stored inverse matrix, or if it is not the inverse of
# the current matrix, invert the current matrix
x$setinv(m)
# Store the inverted matrix in the "cache matrix"
m
# Return the inverted matrix
}
