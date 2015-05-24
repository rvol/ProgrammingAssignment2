#This is my Programming assignment 2 solution.
#It contais a two functions: makeCacheMatrix and cacheSolve

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	  #init
      x_inv <- NULL 
      #set matrix
      set <- function(y) {
		x <<- y
		xinv <<- NULL 
      }
	  #get matrix
      get <- function() x 
      #set up inverse matrix
      set_Inv <- function(inv) x_inv <<- inv
      #get inverse matrix
      get_Inv <- function() x_inv 
      list(set = set, get = get,set_Inv = set_Inv,get_Inv = get_Inv)
  }


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
	  # get the inverse matrix 
	  m <- x$get_Inv() 
	  # is it calculated
      if(!is.null(m)) { 
		return(m) 
      }
      # get matrix 
      data <- x$get() 
      #solve it
      m <- solve(data) 
      x$set_Inv(m) 
      #result
      m
}
