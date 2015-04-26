## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() ,the first function, accepts a matrix as input 
## and returns a "list" of it's access functions and reference ID (a place holder in memory)
 
## cacheSolve(), the second function,accepts the special matrix created by the first function 
## and returns the inverse of that matrix . It will either retrive it from cache or use solve(matrix)
## to invert it.

## Write a short comment describing this function
## makes a special matrix and methods for cache  

## it setsup done at the console b4 running the functions 
## m1<-matrix(sample(25),5,5)  #create a square a matrix
## det(m1)   ## calculates the determinate if > 0 then inverse x is possible
## m2 <- makeCacheMatrix(m1) creates special matrix
## cacheSolve(m2)  results 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                   ## set and initiate a place holder for the inverted matrix
  set <- function(y) {                        ## when set is called clears cache and sets to Null in this     
    x <<- y                                  ## functions parent environment (I:E The makeCacheMatrix environment)
    m <<- NULL
  }
  get <- function()                           ## returns the original matrix by referencing its environment 
                                        
  setinvm <- function(invm) m <<- invm        ## places the inverse matrix into m    
  
  
  getinvm <- function() m                     ## returns the inverse matrix or else Null 
  
  list(set = set, get = get,                  ## makes the access functions available to the global Environment 
       setinvm = setinvm,
       getinvm = getinvm)
}


## Write a short comment describing this function

## accepts the list  created by the first function 
 ## and either calculates or retrieves the inverse matrix

cacheSolve <- function(x, ...) {                ## accepts original values 'x' and access functions and environment 
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvm()                          ## The access function retrieve's Either Null or the inverse matrix 'x' 
  if(!is.null(m)) {                         ## If the inverse exist, it is returned  and routine is ended.
    message("getting cached data")
    return(m)
  }
  data <- x$get()                          ## if the inverse matrix did not exist matrix 'x' is retrieved 
  m <- solve(data, ...)                    ## This creates the inverse matrix 
  x$setinvm(m)                             ## inverse matrix is cached. 
  
  m                                        ## dislay it to the console

}
