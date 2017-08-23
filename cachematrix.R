## Title: R Cache Matrix
## Author: Jorge Sauma
## Course: R Programming
##         Programming assignment, week 3
## Date: 08-24-2017
## Version 1
##
## Note: The following matrix will be used for testing purposes:
##  
## Matrix:
##       [,1] [,2] [,3]
##  [1,]    1    2    3
##  [2,]    0    1    4
##  [3,]    5    6    0
##
## Inverse:
##       [,1] [,2] [,3]
##  [1,]  -24   18    5
##  [2,]   20  -15   -4
##  [3,]   -5    4    1

## makeCacheMatrix: creates a list with functions used to set and get
## a matrix, and setinverse, getinverse to set and get the inverse
## of this matrix
## Arguments: 
##            mat= Matrix object

makeCacheMatrix <- function(mat = matrix()) {

    ## Is easy to verify that mat is a matrix. If it isn't, return NULL
    if (!is.matrix(mat)){
      message("Argument is not a matrix")
      return()
    }
  
		# mi= matrix inverse
	    mi <- NULL

		# nm= new matrix
		# Note: Set is not used anywhere. Left here for
        # future use
        set <- function(nm) {
                mat <<- nm
                mi <<- NULL
        }

        get <- function() mat

		# extmi: external matrix inverse (calculated outside this function)
        setinverse <- function(extmi) mi <<- extmi
        getinverse <- function() mi

		# This is the list that will be returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve: calculate the inverse of a matrix, 
## or return the result if it was calculated before
## Arguments: 
##            cachedmat= List returned by MakeCacheMatrix 

cacheSolve <- function(cachedmat, ...) {

  ## At least verify that cachedmat is not null. If it is NULL show a message
  if (is.null(cachedmat)) {
    message("Argument is NULL")
    return()
  }
  
  
    ## Return a matrix that is the inverse (mi) of 'mat'
		mi <- cachedmat$getinverse()

        if(!is.null(mi)) {
                message("Inverse already calculated:")
                return(mi)
        }
		
		## Inverse was not cached
		message("Calculating inverse: ")
		
        mat_temp <- cachedmat$get()
        mi <- solve(mat_temp)
        cachedmat$setinverse(mi) # Caching the result
        mi
}
