## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## my function for AP_2
# creates the matrix
makeCacheMatrix <- function(x = matrix()) {
  #checks the inverted for NULL
  inverted <- NULL
  #checks if inverted
  set <- function(y) {
    x <<- y
        inverted <<- NULL
  }
  #set get and set functions over the matrix
  get <- function() x
   setinverse <- function(inverse) inverted <<- inverse
   getinverse <- function() inverted
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { 
  #checks if the value exists and get it :-
  inverted <- x$getinverse()
  #if is not null return that
  if(!is.null(inverted)) {
    message("getting cached data.")
    return(inverted)
  }
  data <- x$get()
  #invert the matrix
  inverted <- solve(data)
  x$setinverse(inverted)
  inverted
}
