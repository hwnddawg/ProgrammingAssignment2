## makeCacheMatrix and cacheSolve are functions that are used to 
## create, set and access a special object that stores a matrix 
## and computes its inverse.  



## makeCacheMatrix -- a special "matrix" object that can cache 
#  its inverse.  It is a list containing functions to set/get 
#  the value of the matrix or set/get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {

	#  initialize
	inv <- NULL

	#  set
	set <- function(y)  {
		x <<- y 
		inv <<- NULL
	}

	#  get
	get <- function() x 

	#  save computed inverse
	setinverse <- function(solve) inv <<- solve

	#  get/return pre-saved inverse computation 
	getinverse <- function() inv 

	#  alias table 
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

# makeCacheMatrix ends
}



## cacheSolve -- computes the inverse of the special "matrix" 
#  returned by "makeCacheMatrix".  However, "cacheSolve" first 
#  checks to see if the inverse had already been calculated. 
#  If the inverse has already been calculated, then "cacheSolve" 
#  retrieves the inverse from the cache and skips the computation. 
#  Otherwise, "cacheSolve" calculates the inverse and sets the 
#  value of the matrix in the cache via the "setinverse" function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	#  get cached inverse
	inv <- x$getinverse() 

	#  if cached inverse exists, return it
	if (!is.null(inv))  {
		return(inv)
	}
	#  immediately exit function cacheSolve with return

	#  fall thru here only if cached inverse is null (empty) 

	#  get and compute inverse
	data <- x$get()
	inv <- solve(data, ...) 
	
	#  set (store) the computed inverse in the cache 
	x$setinverse(inv)

	return(inv)
	#  exit function cacheSolve
}
