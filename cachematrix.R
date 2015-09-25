## The function pair enable the caching of the inverse of a matrix.
## There might be some value in eliminating repeat / redundant 
## calculations 

## 'makeCacheMatrix' is a factory function that creates
## a special matrix object essentially a list of functions.

makeCacheMatrix <- function(x = matrix()) {
      v <- NULL
      
      # initialises the matrix's inverse values to null
      
      set <- function(y) {
            x <<- y
            v <<- NULL
      }
      
      # 'set' function assigns a new matrix input to the 'x' 
      # object'x' in another environment and resets the 
      #  inverse variable 'v' to 'NULL' in another environment
      # i.e. to function environment above ('makeCacheMatrix')
      
      get <- function() x
      
      # 'get' function returns the special matrix object
      
      setinverse <- function(inverse) v <<- inverse
      
      # assigns arbitary matrix inverse values to variable 'v'
      # in the main function environment
      
      getinverse <- function() v
      
      # returns matrix inverse values stored in variable 'v'
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
      # creates and returns function list for special matrix object
}

## the 'cacheSolve' function calculates the inverse of the 
## matrix contained in the special matrix object created by
## the 'makeCacheMatrix' function. First checking if the inverse 
## has already been calculated for this matrix, if this is the 
## this case it skips the calculation, returns the stored inverse
## matrix and prints a message

cacheSolve <- function(x, ...)    {
      
      v <- x$getinverse()
      
      # retrieves inverse variable which contains either null or
      # previously computed matrix inverse
      
      if(!is.null(v)) {
            message("retrieving matrix inverse")
            return(v)
      }
      
      # if 'v' is not null then message printed and inverse matrix
      # returned, the function terminates
      
      data <- x$get()
      
      # uses the special matrix object's list function 'get'
      # to store the current matrix in 'data'
      
      v <- solve(data, ...)
      
      # stores the inverse matrix in variable v using 
      # solve function on 'data' (the current matrix)
      
      x$setinverse(v)
      
      # uses list function 'setinverse' of special matrix
      # object to store current the matrix's inverse in the 
      # 'v' variable contained in another environment
      
      v
      
      # returns the currently stored matrix's inverse.
}

