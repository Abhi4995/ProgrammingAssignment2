## First function makeCacheMatrix is used to set a cache value of matrix ( n this case invertable), 
##  which is used in the next function to calcucate the inverse of a cached matrix


## makeCacheMatrix returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    # `<<-` is used to assign a value to an object in an environment 
    # different from the current environment. Setting up a cached value.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse 
  
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  
  
}


## This function return a matrix, which is inverse of the called matrix.
## Takes input from the function makeCacheMatrix and marks the inverse of the cached value.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x' taken from makeCacheMatrix
       
        inv <- x$getinv  ## takes value for inverse from makeCacheMatrix
        
        if (!is.na(inv))
        {
          inv  ##value taken from cache if inverse exists
          
        }
        
        ##calculating inverse if above if statement is false
        
        matrix.data <- x$get()
        
        inv <- solve(matrix.data,...)
        
        x$setinv(inv) ##this function sets value of inverse in cache for future use if any.
        
        inv
        
  
  
}
