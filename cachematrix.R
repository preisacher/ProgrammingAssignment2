## These functions are built very closely to the examples we
## were given using vectors.  All I really did was to change from
## using Vectors to a Matrix and of course added the solve to Inverse a
## Matrix.
## 

## The makeCacheMatrix returns a list of functions that can be used to return
## its inverse ... solve. 

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x <<- y
		m <<- NULL
	}
	        
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, 
	get = get,
	setmatrix = setmatrix,
	getmatrix = getmatrix)

}


## This function computes the inverse of the matrix that is returned by
## the makeCacheMatrix function
## To test you can run the following:
## The following statement will create a 2 by 2 square matrix with 1 2 on 
## the first row and 2 1 on the second
## > myMatrix <- cbind(c(1:2),c(2:1))
## The cacheSolve function returns the matrix inverted.
## > cacheSolve(makeCacheMatrix(myMatrix))
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
##


cacheSolve <- function(x, ...) {
  
	m<-x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
		  
	matrix<-x$get()
	m<-solve(matrix, ...)
	x$setmatrix(m)
			  
	## Return the inverse of the matrix
	m
}
