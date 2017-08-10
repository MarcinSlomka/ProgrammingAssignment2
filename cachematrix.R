## Put comments here that give an overall description of what your 
# functions do 

# This function is my representation of matrix and  it's inverse
# setMatrix/setinvMatrix are setting values
# getMatrix/getInvMatrix are getting values
# function returns list of functions that let operate on internal representation of
# matrix and it's inverse



makeCacheMatrix <- function(x = matrix()) { 
  
  invMatrix <- NULL  # invere of matrix X
  
  setMatrix <- function(y) {  x <<- y }
  getMatrix <- function( ) { x }
  
  setInvMatrix <- function(z) { invMatrix <<- z }
  getInvMatrix <- function( ) { invMatrix }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix )
} 



## Write a short comment describing this function 
#This function calculates inverse of matrix in optimized wat
# first call is calculated but next ones are returning value from cache

cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  
  IMX <- x$getInvMatrix()  # get inverse matrix
  if(!is.null(IMX)) {      
    
    message("Getting cached inverse of matrix")
    return(IMX)
  }
  
  MX <- x$getMatrix() # get matrix
  
  if( !is.null(MX) ){
    IMX <- solve(MX)   # find inverse
    x$setInvMatrix(IMX) # set inv matrix
    
    message("Getting  just calculated inverse of  x")
    return(IMX)
  }
  else{
    message("Can not inverse NULL matrix" )
    return(NULL)
  }
} 


# create random 4x4   matrix
rand_mx <-matrix(runif(16,1,16),4,4)
rand_mx


#make  matrix based on rand_mx
mx <- makeCacheMatrix(rand_mx)
mx
mx$getMatrix()
mx$getInvMatrix()


#calculate inverse for the first time
inv <- cacheSolve(mx)
inv
mx$getInvMatrix()

#calculate inverse from cache
inv1 <- cacheSolve(mx)
inv1
mx$getInvMatrix()


#check if first time inverese and cached are the same
identical(inv,inv1)


