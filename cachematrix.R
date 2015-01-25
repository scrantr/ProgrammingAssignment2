
# $Id: cachematrix.R,v 1.5 2015/01/25 07:44:42 richard Exp richard $

#
# Calculate inverse of a matrix, cache calculated value in
# next higher context to avoid repeated recalculation.
# Calling "set" will invalidate the cache variable, readying
# it to receive the result of the new calculation.
#

#
# Constructs a list of functions accessing this context's
# data rather than the caller's context.  The bundle is
# handed to cacheSolve() to mimic "global-ish" or pointer
# referenced variables.
#
makeCacheMatrix <- function(x = matrix()) {

	invmat <- NULL

# define functions whose pointers will be carried down the
# context stack in list(set,get,setinverse,getinverse)
	set        <- function(y) {
		if(!all(x == y)) {    # Make set() a no-op if new and old
			x      <<- y      # matrices are numerically the same.
			invmat <<- NULL   # Someone might call it someday.
		}
	}
	get        <- function() x
	setinverse <- function(solve) invmat <<- solve
	getinverse <- function() invmat

	funclist <- list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse )
	return(funclist)
}

#
# fetch cached inverse of matrix or calculate inverse, cache it,
# then return inverse
#
cacheSolve <- function(funclist, ...) {
	invmat <- funclist$getinverse()   # try to fetch the cached inverse
	if(is.null(invmat)) {             # cached inverse not found - boo!
		message("calculating previously-uncached data")
		data   <- funclist$get()      # get the matrix to be inverted
		invmat <- solve(data, ...)    # calculate uncached inverse
		funclist$setinverse(invmat)   # cache newly-calculated inverse
	} else {
		message("returning data already calculated, cached")
	}
	return(invmat)                    # return something nice to the caller
}

# end of cachematrix.R

