#Croix Snapp's submission 

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates an R object of type makeCacheMatrix(), that stores a matrix,
#and its inverse

makeCacheMatrix <- function(x = matrix()) {
  #makeCacheMatrix builds four functions: set(), get(), setSolve(), and getSolve()
  #These four functions are stored in a list within makeCacheMatrix object's environment
  #and then these functions will be returned within the list to the parent environment 
  #when called. 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Note that x and m are data objects that are used to initialize and define tharguments
  #that get passed to the functions. 
  
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function


#cacheSolve is a function that takes an object of type makeCacheMatrix(), and returns the
#inverse matrix that is stored in the makeCacheMatrix's environment

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

#Now test the functions, by making a numeric vector "a" that has a square number of 
#elements. Then assign the vector to a square matrix. Next, pass A to makeCacheMatrix, and
#assign it to myMatrix. Finally, pass myMatrix to cacheSolve. 

a<-c(1,2,3,4)
A<-matrix(a,2,2,TRUE)
myMatrix<-makeCacheMatrix(A)
cacheSolve(myMatrix)
