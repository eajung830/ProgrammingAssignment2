## Put comments here that give an overall description of what your
## function makeCacheMatrix creates an object that stores both a matrix and its inverse matrix. The inverse matrix is stored in cache memory
## function cacheSolve takes in the object created in makeCacheMatrix and returns the inverse matrix to be stored in cache. If
## the inverse matrix has already been stored in cache, it returns the cached data instead


## makeCacheMatrix creates a special object that reads in a matrix and allows it to store the cached 
## inverse of the matrix
makeCacheMatrix <- function(x = matrix(),...) {

  invMat<-NULL ## initiate variable for inverse matrix storage "invMat"
  ## allows to input a new matrix
  set <- function(y){
    x<<-y
    invMat<<-NULL
  }

  ## reports the original matrix
  get <- function() x 
  ## sets the inverse matrix in cache
  setinverse <- function(data) invMat <<- data
  ## returns the inverse matrix in cache 
  getinverse <- function() invMat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve takes an object created by the makeCacheMatrix function
## It returns the cached inverse matrix (variable "invMat") if it is available
## otherwise it calculates the inverse matrix, stores it in cache, and returns it
cacheSolve <- function(x,...){
  ##initates local variable invMat and reads invMat value from the object x
  invMat <- x$getinverse() 

  ## checks if not null, then this means inverse matrix was already stored in cache
  if(!is.null(invMat)){
    message("getting cached inverse matrix")
    return(invMat)
  }
  
  ## if the invMat returned null, means the inv matrix has not yet been calculated
  ## the code below calculates inverse matrix and stores it in cache
  originalMat <- x$get()
  invMat<- solve(originalMat,...)
  x$setinverse(invMat)
  invMat
}

