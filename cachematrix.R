## The following functions introduce the "<<-" operator. This operator can be 
## used to assign a value to an object in a separate enviornment (inside a function).

## In this example we are calculating the inverse of a matrix. 
## This calculation can be quite exhaustive, in terms of time and 
## computer processing, given a large matrix. Thus, if we need to access
## the inverse of the same matrix it would be most efficient to calculate 
## the inverse once and store the result. Then, we can access the inverse as needed.


## "makeCacheMatrix" essentially acts as a storage room or shelf for the user
## provided matrix and the inverse matrix. These two matrices (original and inverse)
## are stored in this function's environment. One can not change these matrices
## without using the functions returned by "makeCacheMatrix" (getmatrix(),setmatrix(),etc.)
## Thus, the inverse matrix can be stored, undisturbed, until needed for a calculation.

## This function "makeCacheMatrix" returns a list that contains functions
## These functions will set a user provided matrix, get that matrix
## set the inverse of the matrix, get the inverse matrix.

makeCacheMatrix <- function(orig_matrix = matrix()) {
  
  # Initialize inverse matrix to NULL
  inv_matrix<-NULL
  
  # Set a user provided matrix, initialize inverse to null
  set<-function(y){
    orig_matrix<<-y
    inv_matrix<<-NULL
  }
  
  # Return original matrix, provided by user
  get<-function() orig_matrix
  
  # Set inverse matrix
  setmatrix<-function(solve) inv_matrix <<- solve
  
  # Return inverse matrix
  getmatrix<-function() inv_matrix
  
  # List containing the four functions defined above, this list is returned
  # "set" and "get" refer to the original matrix
  # "setmatrix" and "getmatrix" refer to the inverse matrix
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The function "cacheSolve" will return an inverse matrix based on a user provided matrix.
## This function first checks if an inverse matrix has been calculated.
## If an inverse has been calculated the inverse is returned and the calculation is skipped.

cacheSolve <- function(orig_matrix, ...) {
  
  #Retrieve inverse matrix from cache, default is NULL
  inv_matrix <- orig_matrix$getmatrix()
  
  #If inverse has been calculated (not NULL), return inverse matrix, exit function
  if(!is.null(inv_matrix)){
    message("getting cached data")
    return(inv_matrix)
  }
  
  #If inverse matrix has been set, the calculation below will be skipped
  #Else ...
  
  #Get original matrix, compute inverse, save inverse in cache, return inverse (in that order)
  matrix_retrieved<-orig_matrix$get()
  
  inv_matrix<-solve(matrix_retrieved, ...)
  
  orig_matrix$setmatrix(inv_matrix)
  
  inv_matrix
}
