## Coursera : R Programming (rporg-007) : Programming Assignment 2
## Author : Sandeep Girgaonkar
##
## This file contains R functions to create a special matrix object
## that can also cache its own inverse.
##
## Usage:
## m1 <- makeCacheMatrix(matrix(1:4, 2,2)) # create special matrix object
## m2 <- cacheSolve(m1) # calcualte inverse
##


# Function makeCacheMatrix  
#
# Function to create special matrix object that has capability to 
# cache its own inverse. This special matrix object also provides
# methods to get/set matrix value and get/set matrix inverse value.
#
# It is assumed that the matrix value is in n-by-n format so that its
# inverse can be calculated.
#
# [Argument] Matrix 
# Returns - Special matrix object
makeCacheMatrix <- function(m = matrix()) {
	
	# Matrix that stores the inverse. 
	invMatrix <- NULL

	# Function to set the value of matrix
	# [Argument] mVal - Has to be a n-by-n matrix
	# Returns - nothing
      set <- function(mVal) {
		# Note - No validation is done on input parameter.

		# [TBD and out-of-scope] Is call to 'identical' as expensive
		# as calculating matrix inverse ? If yes, we can skip this step
		if(identical(m, mVal) ) {
			#Current matrix and given matrix are same. No need to
			# recalculate any value.
			# message("Both value are same. Returning")
			return(NA)
		}

		#message("Setting value")
		
		# Set the new value for this matrix
            m <<- mVal

		# Since matrix value changed, set matrix inverse to NULL
		# It should be recalculated later 
            invMatrix <<- NULL
      }

	# Function to get the matrix
	# [Argument] None
	# Returns - matrix
      get <- function(){
		# Returns matrix value
		m
	}

	# Function to set the inverse of matrix.
	# [Argument] inv - the inverse matrix
	# Returns - nothing
      setInverse <- function(inv){
		# Note - No validation is done on input parameter.
		invMatrix  <<- inv
	}

	# Function to get the inverse of matrix
	# [Argument] None
	# Returns - matrix
      getInverse <- function(){
		invMatrix
	}

	# List properties
      list(set = set, get = get,
            setInverse = setInverse ,
            getInverse = getInverse )

} #### end function makeCacheMatrix 




# Function cacheSolve 
#
# Function to get the inverse of matrix
# This function will return already cached inverse matrix.
# If the inverse value is not already cached, then it calculates the inverse
# and caches it for future use.
#
# [Argument] Matrix - created using makeCacheMatrix() function (see above)
# Returns - matrix inverse
cacheSolve <- function(x, ...) {
	# Try to get the cached matrix inverse
      m <- x$getInverse()
      if(!is.null(m)) {
		# The inverse value is already cached
		# Just return cached value
            message("getting cached data")
            return(m)
      }

	# The inverse value does not exist. So calculate it.
	# Get matrix value
      data <- x$get()
	# Calculate its inverse
      m <- solve(data, ...)
	# Save / Cache the inverse value for future use
      x$setInverse(m)

	# Return matrix inverse
      m
} #### end function cacheSolve 




