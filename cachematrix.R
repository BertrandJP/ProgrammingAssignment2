## Set of functions used to calculate the inverse of a matrix, and cache its result,
## so that it is only calculated once
## 1) Function makeCacheMatrix will enable to set/get the value of the matrix and its inverse (after it has been calculated once)
## 2) Function cacheSolve will enable to calculate and return the inverse of a matrix stored in the result of makeCacheMatrix the first time,
##    and return the cache value for later calls

## Function makeCacheMatrix will enable to set/get the value of the matrix and its inverse (after it has been calculated once)

makeCacheMatrix <- function(x = matrix()) {
  ##takes a matrix as an argument
  ##returns a list of functions to get/set its value, and its inverse
  
  if (class(x)!="matrix") { #verify supplied argument
    stop("argument must be a matrix")
  }
  i <- NULL    #variable to cache the inverse of the matrix
  set <- function(x2) {     #set a new value to matrix x, and resets i
    x <<- x2
    i <<- NULL
  }
  get <- function() x  #gets x
  setInv <- function(i2) i <<- i2  #enables to force the cache value of i
  getInv <- function() i           #gets the cache value of i
  list(set = set, get = get,       #shows the 4 above functions
       setInv = setInv,
       getInv = getInv)
}


## Function cacheSolve will enable to calculate and return the inverse of a matrix stored in the result of makeCacheMatrix the first time,
## and return the cache value for later calls

cacheSolve <- function(x, ...) {
		##takes an argument created with makeCacheMatrix
        ##returns its cache inverse, or calculates it
  
  result <- tryCatch({ #handle exceptions dues to a non valid argument
    i <- x$getInv()    #try to get cache inverse of x
    if(!is.null(i)) {  #check if it is available
      message("getting cached data")
      return(i)        #return cache inverse
    }
    data <- x$get()  
    i <- solve(data) #calculates the inverse of the matrix with 'solve'
    x$setInv(i)      #stores i in cache for future calls
    return(i)       #returns the inverse
  }, error = function(err) {
    #error handling, in case x$get() or x$setInv() failed due to non valid arguments
    print(paste("ERROR: check argument is a valid list created with makeCacheMatrix ", err))
  })
}

##examples to test functions
# c=rbind(c(1, -1/4), c(-1/4, 1))
# m=makeCacheMatrix(c)
# m$get()
# cacheSolve(m)
# cacheSolve(m)
# solve(c)

# l=list(1,2,3)
# cacheSolve(l)