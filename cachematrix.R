## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the inverse property
	m<-NULL

	## Method to set the matrix
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	
	## Method to get the matrix
	get <- function()x
	
	## Method to set the inverse of the matrix
	setInverse <- function(inverse){
		i<<-inverse
	}
	## Method to get the inverse of the matrix
	getInverse <- function()m
	
	## Return a list of methods
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix" above

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	
	##Just return the inverse if its already set
	if(!is.null(m)){
		message(“getting cashed data”)
		return(m)
	}
	
	## Get the matrix from the object
    	data <- x$get()

    	## Calculate the inverse using matrix multiplication
    	m <- solve(data) %*% data

    	## Set the inverse to the object
    	x$setInverse(m)

    	## Return the matrix
    	m
		

}
