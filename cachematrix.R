## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special Matrix, which is really a list containing a function to
## get matrix, set matrix, get inverse of the matrix and set inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m<<-null
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix. First, it checks to see
## if the mean has already been calculated. If so, gets the inverse from the cache. Otherwise, calculates the
## inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
          message ("getting cached data")
          return (m)
      
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse (m)
    m
}
