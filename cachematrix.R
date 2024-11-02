## A pair of functions that cache the inverse of a matrix

## starting my code by copying the generic set up from makeVector and cachemean functions
## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #assigning variable m as NULL. Variable still exists but holds no value. Cache for inverse
  set <- function (y){ #defining a function named set that takes one argument, y. Function assigns a new value to x.
    x <<- y #<<- is used to assign y to x in the parent environment instead of local environment within this set function
    m <<- NULL #reset the inverse cache. Indicates some cached result is no longer valid 
  }
  get <- function()x #retrieves the current value of x. x is defined in the parent environment
  setInverse <- function(inverse) m <<- inverse #to store or cache an inverse matrix
  getInverse <- function () m #returns the cached value of m (or NULL if no value has been cached)
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #returns functions as a list 
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getInverse() # to retrieve the cached inverse matrix
  #if inverse is already cached: 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get() #if inverse is not cached, to retrieve the original matrix
  m <- solve(data, ...) #calculate the inverse of a matrix 
  x$setInverse(m) #cache the newly calculated inverse for future use 
  m # return the inverse matrix
}
