## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(originalMatrix = matrix()) {
  cachedMatrixInverse <- NULL
  set <- function(y) {
    originalMatrix <<- y
    cachedMatrixInverse <<- NULL
  }
  get <- function() x
  setMatrix <- function(matrix) m <<- matrix
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(cacheMatrix) {
  m <- cacheMatrix$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- cacheMatrix$get()
  m <- solve(data)
  cacheMatrix$setMatrix(m)
  m
}
