
# The  aim in this exercise is to write a pair of functions, namely, 
# "makeCacheMatrix" and "cacheSolve" 

# Given below is a function called makeCacheMatrix which creates a "matrix" that gives 
# its inverse for the given input 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function

## Given below is a function called cacheSolve. It computes the inverse of the above given matrix (makeCacheMatrix) 
## If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Checking the program

m <- matrix(rnorm(4),2,2)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
