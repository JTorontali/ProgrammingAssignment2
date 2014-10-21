##The functions below will create a cache of matrix inversions for retrieval later.
##Calculations which are repeated will be retrieved instead of being recalculated.
##All of this is done to reduce processing of costly computations.


##The function below creates a list of functions that set and retrieve matrices.
##Additionally it will set and retrieve the inverse of the matrices.

makeCacheMatrix <- function(x = matrix()) {  #Initialize function
  m <- NULL  #Establish variable 'm'
  set <- function(y) {  #Initialize sub-function to set matrix 
    x <<- y  #Search parent environment for variable 'x' and set to 'y'
    m <<- NULL  #Search parent environment for variable 'm' and set to NULL 
  }
  get <- function() x  #Initialize sub-funtion to retrieve matrix 
  setinverse <- function(inverse) m <<- inverse  #Initialize sub-function to set matrix inverse 
  getinverse <- function() m  #Initialize sub-function to get matrix inverse 
  list(set = set, get = get,  #create list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


##The function below will check the cache for previous calculations
##If the previous calculation exists, it will retrieve and print the inverse
##If no previous calculculation exists, it will calculate and store the inverse in cache

cacheSolve <- function(x, ...) {  #Initialize function
  m <- x$getinverse()  #Set function output to 'm'
  if(!is.null(m)) {  #Selection criteria
    message("Retrieving Cached Data...")  #Message printed if inverse stored in cache
    return(m)  #Returns Message
  }
  data <- x$get()  #Sets stored cache value to 'data'
  m <- solve(data, ...)  #Define inverse matrix solution
  x$setinverse(m)  #Sets the inverse matrix solution 'm' to the cache
  m  #Returns the solution 'm'
}