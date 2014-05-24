## Below are two functions that go hand in hand. The makeCacheMatrix function
## creates a special type of object that holds a matrix, that matrix's inverse
## (if it's been calculated before), and 4 functions that can get/set the 
## special object's matrix and matrix inverse.
## The second function will, one way or another, return the inverse of the matrix 
## held in a 'cacheMatrix' object created from the 'makeCacheMatrix' function.
## If the cacheMatrix object has a cache with its matrix's inverse stored, then
## cacheSolve will return the cached result without any calculations. Otherwise,
## cacheSolve will calculate the inverse of the matrix within the 'cacheMatrix'
## object, tell the 'cacheMatrix' object to store the result, and then return that result.

makeCacheMatrix <- function(matrix = matrix(data = c(2,2,3,2), nrow=2, ncol=2)) { ## default, arbitrarily chosen matrix to use
  cachedMatrixInverse <- NULL ## this is the cache - initialize to NULL
  set <- function(y) {
    matrix <<- y ## set the matrix variable (originally our default matrix) to whatever you pass into set
    cachedMatrixInverse <<- NULL ## re-initialize our cache to NULL
  }
  get <- function() matrix ## simply return the matrix we are working with
  setMatrix <- function(matrix) cachedMatrixInverse <<- matrix ## take a matrix and store it in cache
  getMatrix <- function() cachedMatrixInverse ## simply return what we have cached
  list(set = set, get = get, ## return a list of each of the functions defined above
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## cacheSolve takes a 'cacheMatrix' object and will either retrieve the cached 
## matrix inverse if there is one, or calculate the matrix inverse and cache/return it

cacheSolve <- function(cacheMatrix) {
  result <- cacheMatrix$getMatrix() ## get the cached value
  if(!is.null(result)) { ## if the cached value is NOT null, then we already know the result - return that result
    message("getting cached data")
    return(result)
  }
  data <- cacheMatrix$get() ## otherwise, we need to get the matrix we are working with so we can calculate its inverse
  result <- solve(data) ## calculating the inverse
  cacheMatrix$setMatrix(result) ## set the cache to the inverse
  result ## return the inverse
}
