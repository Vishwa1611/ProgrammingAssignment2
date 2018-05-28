## This is a R code which can cache he value of the an inverted matrix in the memrory, 
## which can be further used and hence help in saving time by not computing it every time. 

## This function creates aspecial materix object which can stored the cached value of the matrix in it 
## The function set,get, setInverse and getIverse are used as follows
## set -> set the value of the matrices 
## get -> gets the value of the matrix needed
## setInverse -> sets the value of the inverted matrix 
## getInverse -> gets the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(solve) {
    inver <<- solve
  }
  
  getInverse <- function(){ 
    inver
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated, then the cachesolve will retrieve the inverse. 
## Computing the inverse of a square matrix is
## done with the solve function in R. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver      
}