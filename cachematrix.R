## Create a matrix that can cache its own inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #inverse property 
  
  set<-function(matrix){
    mat<<-matrix 
    inv <<- NULL
  }
  
  #Get the matrix method 
  get <- function() {
    mat 
  }
  
  #Set the inverse of the matrix 
  setInverse <- function(inv){
    inv <<- inverse
  }
  
  #Get the inverse back
  getInverse <- function() {
    inv
  }
  
  #Return a list of all of the methods
  #Including get and set 
  list(set = set , get = get, setInverse = setInverse, getInverse= getInverse)
}


##Find the inverse of the special matrix returned 
## by the function named makeCacheMatrix 

##If inverse has been found + matrix has not changed, 
## then cacheSolve retrieves inverse from the cache  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()
  
  #If already set, return inverse 
  if (!is.null(mat)){
    message("I am getting the cached data now...")
    return(mat)
  }
  
  #Get matrix from object back 
  data <- x$get()
  
  #Inverse calculation 
  mat <-solve(data) %*% data 
  
  #Set the inverse
  x$setInverse(mat)
  
  #Return the matrix 
  mat

}

