## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL					# i stores the cached inverse matrix
    	set <- function(y) {  			# set the value of the matrix
		x <<- y
		i <<- NULL
	}

	get <- function() {    			# get the value of the matrix
		x
	}
	seti <- function(invMtx) {   		# set the value of the inverse matrix
		i <<- invMtx
	}
	geti <- function() {			# get the value of the inverse matrix
		i
	}
	list(set = set, get = get,		# list() stores this 4 functions in the makeCacheMatrix function
	     seti = seti, geti = geti)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$geti()
	if(!is.null(i)) {							# if the inverse is already calculated, it returns the i  
		message("getting cached inverse matrix")
                return(i)
        } 
        i <- solve(x$get())     					# with solve it caches the inverse (note: it simpler and cleaner without data variable mentioned in the example assignment!)
        x$seti(i)
        i 									# return i
}
