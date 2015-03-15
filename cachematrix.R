## Put comments here that give an overall description of what your
## functions do

## How to use. Example
## > matriz <- makeCacheMatrix(matrix(c(1,0,1,2,4,0,3,5,6),3,3))
## > inversa <- cacheSolve(matriz)
## > matriz$get() %*% inversa

## Create a special object from a matrix to store inverse result
## x$get -> used to get the initial matrix
## x$setInv -> used to store the result when calculated
## x$getInv -> used to get the solution when it is cached

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Initializing cache object with the information available (not the inverse matrix since it is not calculated here)
  ## get -> gets the input matrix
  get <- function() x
  # Initializing setInv with m (null) and the operation
  setInv <- function(solve) m <<- solve
  # Initializing getInv with m (null)
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Fields' names  and funcion to calculate inverse (instead of mean example) changed
## Input is an "cacheObject" created with makeCacheMatrix 
## Function verify if there is a inverse stored in the object, otherwise it calculates the solution

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # Access to inverse stored inside the object X  
  m <- x$getInv()
  # If m is null then there is no cached calculation
  if(!is.null(m)) {
    message("getting cached data")
    # Return cached inverse matrix
    return(m)
  }
  # No cached inverse so it has to be calculated
  # x$get to get initial matrix
  data <- x$get()
  # Calculate the inverse matrix with solve function
  m <- solve(data, ...)
  #Store inverse matrix calculation in the object to be available for following call
  x$setInv(m)
  m
  
}


## Initial example with vector and mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

