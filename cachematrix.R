## makeCacheMatrix fn takes as input a matrix and caches 
## the long computations of inverse & returns a list to the next fn.



makeCacheMatrix <- function(x = matrix()) {   
  
  m <- NULL # the result of inversion stored here
  # A setter fn,to set a matrix to the object created by makeCacheMatrix fn
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x # return the input matrix
  setinv <- function(inv=matrix()) m <<- inv # set the inversed matrix
  getinv <- function() m # return the inversed matrix
  
  # return a list that contains these functions,so that we could do 
  # flist<-makeCacheMatrix(test)
  # flist$set(new)
  # flist$get
  # flist$setinv
  # flist$getinv
  # where new,test are matrices
  list(set=set,get = get,
       setinv = setinv,
       getinv = getinv)
}


##cacheSolve checks for the cached data otherwise computes and returns the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getinv() #get the inversed matrix from 'x'
  #it ll be null if uncalculated
  if(!is.null(m)) { #if the inversion result exists
    message("getting cached data")
    return(m)
  }
  data <- x$get() # if not,we do x$get to get the matrix object
  m <- solve(data) # we solve it
  x$setinv(m) # we the set it to the object
  m # return the result

}

# Testing
x<-matrix(1:4,2,2)
flist$set(matrix(c(4,16,4,8),nrow=2,ncol=2))
#generate the makeCacheMatrix object with x
flist<-makeCacheMatrix(x)
# calculating the inverse and retrieving them
cacheSolve(flist)
cacheSolve(flist)
