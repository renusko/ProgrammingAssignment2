## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list to functions which can set and get
## some matrix in parent frame. Then it can store or access the matrix
## inverse from the frame. Function "set(x)" can store matrix and nullifies
## inverse matrix variable. Function "get()" returns the matrix. And similary
## set_inverse(m) and get_inverse() stores (or returns) inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL						# set inverse variable to NULL
	set <- function(y = matrix()) {				# with function set we set new matrix and nullify inverse
		x <<- y
		inverse <<- NULL	
	}
	get <- function() x					# returns matrix
	set_inverse <- function(m = matrix()) inverse <<- m	# set inverse matrix 
	get_inverse <- function() inverse			# returns inverse
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) # list containg all functions above
}


## function "cacheSolve" returns cached matrix inverse(if it is allready calculated) or
## calculate it, store it by set_inverse and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$get_inverse()				# now inverse is either NULL or chached inverse matrix
	if(!is.null(inverse)) {					# if not NULL
		message("Getting chached inverse matrix.")
		return(inverse)					# returns chached inverse	
	}							# if NULL	
	mat <- x$get()						# stores matrix x in variable mat
	inverse <- solve(mat)					# calculating of inverse matrix
	x$set_inverse(inverse)					# storing of inverse matrix in parent frame
	inverse							# returns chached inverse
}
