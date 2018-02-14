## Put comments here that give an overall description of what your functions do

# This set of two functions take advantage of the scoping rules of the R language
# and how they can be manipulated to preserve state inside of an R object to
# cache potentially time-consuming computations, such as computing matrix inverse.

# The first function, makeCacheMatrix, creates a special "matrix" object,
# which is a list that has all the functions defined and can cache its inverse.
# Note that this function does not compute the inverse, as its is to cache the inverse.

# The second function, cacheSolve, can retrieve or computes the inverse of the
# special "matrix" returned by makeCacheMatrix function above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
# Otherwise, it will retrieve the matrix data to compute and update its inverse matrix.

## Write a short comment describing this function

# This function, makeCacheMatrix creates a special "matrix" object, which is a list 
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {
    # set the value of the matrix x in the parent environment 
    x <<- y
    matInv <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the matrix inverse
  # matInv is defined in the parent environment and it needs to access it
  # after setMatInv() completes, the code uses the <<- form of the assignment operator
  # to assign the input argument to the value of matInv in the parent environment.

  setMatInv <- function(inverse) matInv <<- inverse

  # get the value of the matrix inverse
  getMatInv <- function() matInv

  # The code assigns each of these functions as an element within a list(),
  # and returns it to the parent environment.
  # Note that each element in the list is named, and naming the list elements
  # allows the use of the the extract operator form $ to access the functions by name
  # in the downstream R code.
  list(set = set, get = get, setMatInv = setMatInv, getMatInv = getMatInv)
}

## Write a short comment describing this function

# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("computing matrix inverse")
  data <- x$get()
  m <- solve(data, ...)
  x$setMatInv(m)
  #m
}

## Testing
set.seed(111)
aMatrix <- makeCacheMatrix(matrix(rnorm(25), nrow=5, ncol=5))
str(aMatrix)
aMatrix$get()                # retrieve the value of matrix
aMatrix$getMatInv()          # retrieve the value of matrix inverse, which should be NULL
cacheSolve(aMatrix)          # notice the message with matrix inverse being recomputed
cacheSolve(aMatrix)          # notice the message with getting cached matrix inverse
aMatrix$getMatInv()          # retrieve it directly, now that it has been cached
aMatrix$getMatInv()%*%aMatrix$get()  # This gives Identity Matrix
