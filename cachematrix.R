## Assignment: Caching the Inverse of a Matrix
## 1: Write a function to create a matrix and can cache its inverse
## 2: Write a function to compute the inverse of the special matrix 
##    returned by the first function. If the inverse has already been
##    calculated and the matrix is unchanged, this function will 
##    retrieve the cached inverse

# makeCacheMatrix: return a list of functions to:
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  
    # m will hold the cached inverse matrix
    m <- NULL
  
  # define a function to set the vector and reset the m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # returns the vector x
  get <- function() x
  
  # assign mean to m
  setmean <- function(mean) m <<- mean
  # return m
  getmean <- function() m
  
  # return the special vector
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}


# cacheSolve: compute the inverse of the matrix. 
# It will return the cached inverse, if it is already computed before.
# Otherwise, it will calculate to get the result.
cacheSolve <- function(x, ...) {
  m <- x$getmean()
  
  # return cached inverse if it's being computed before
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # compute the inverse
  data <- x$get()
  m <- mean(data, ...)
  
  # cache the inverse
  x$setmean(m)
  
  # return result
  m
}
