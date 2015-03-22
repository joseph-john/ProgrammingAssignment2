## We are try the optmize the processing time it takes for R to compile 
## inverse of matrix which in general takes lot of resources
## The idea is not to compile inverse of matrix more than once for a given matrix

## This fuction creates a list of functions to set, get, setsolve and getsolve for 
## a given "SQUARED" Matrix
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL                     
  set <- function(y){                      
    x <<- y
    m <<- NULL              
  }
  get <- function() x                           
  setsolve <- function(solve) m <<- solve   
  getsolve <- function() m        
  list(set = set, get = get,                    
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function helps us to cache the inverse for a give SPECIAL matrix
## Which We created using makeCacheMatrix function
cacheSolve <- function(x, ...) {                 
  ma <- x$getsolve()
  if(!is.null(ma)) {                 
    message("getting cached inverse matrix data")
    return(ma)
  }
  data <- x$get()                               
  ma <- solve(data, ...)
  x$setsolve(ma)
  ma
}