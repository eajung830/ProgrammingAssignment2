## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special object that reads in a matrix and allows it to store the cached 
## inverse of the matrix
makeCacheMatrix <- function(x = matrix(),...) {
  invMat<-NULL
  ## allows to input a new matrix
  set <- function(y){
    x<<-y
    invMat<<-NULL
  }
  get <- function() x
  setinverse <- function(data) invMat <<- data
  getinverse <- function() invMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve takes in an object created by the makeCacheMatrix function
## It returns the cached inverse matrix (variable "invMat") if it is available
## otherwise it calculates the inverse matrix, stores it in cache, and returns it
cacheSolve <- function(x,...){
  ##this makes sense because x is a "makeCacheMatrix" variable
  invMat <- x$getinverse() 
  if(!is.null(invMat)){
    message("getting cached inverse matrix")
    return(invMat)
  }
  originalMat <- x$get()
  invMat<- solve(originalMat,...)
  x$setinverse(invMat)
  invMat
}

