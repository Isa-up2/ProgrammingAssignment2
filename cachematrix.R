## This pair of function cache the inverse of a matrix.
## Is the operation is alredy done you don't need to recomputed.

## First function: creates a list containing a function to:
## 1- Set the value of the matrix
## 2- Get the value of the matrix
## 3- Set the value of the inverse of the matrix
## 4- Set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Second function: calculate the inverse of a matrix. First it checks if the inverse
## of that matrix has already been calculated, if so, it gets the inverse from cache.
## Otherwise it calculated the inverse of the matrix and set it in the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix, from the cache, that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## Calculated and return a matrix that is the inverse of 'x'
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinv(m)
  m
}
