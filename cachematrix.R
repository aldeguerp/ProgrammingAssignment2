## Put comments here that give an overall description of what your
## functions do

## How to use. Example
## > matriz <- makeCacheMatrix(matrix(c(1,0,1,2,4,0,3,5,6),3,3))
## > inversa <- cacheSolve(matriz)
## > matriz$get() %*% inversa

## Create a special object from a matrix to store inverse result
## x$get -> gets the input matrix
## x$setInv -> used to store the result when calculated
## x$getInv -> used to get the solution when it is cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Fields' names  and funcion to calculate inverse (instead of mean example) changed
## Input is an "cacheObject" created with makeCacheMatrix 
## Function verify if there is a inverse stored in the object, otherwise it calculates the solution


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
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

