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
  m <- NULL                                        ## set and initiate a place holder for the inverted matrix
  set <- function(y) {
    x <<- y                                        ## when set is called clears cache and sets to Null         
    m <<- NULL
  }
  get <- function() x                              ## returns the original matrix by environment   
  
  setinvm <- function(invm) m <<- invm             ## places the inverse matrix into m 
  
  
  getinvm <- function() m                         ## returns the inverse matrix or else Null   
  
  list(set = set, get = get,                      ## makes the access functions available by list 
       setinvm = setinvm,
       getinvm = getinvm)
}


## Write a short comment describing this function

## accepts the list  created by the first function 
## and either calculates or retrieves the inverse matrix



cachesolve <- function(x, ...) {               ## accepts original values 'x' and access functions
  ## Return a matrix that is the inverse of 'x
  m <- x$getinvm()
  if(!is.null(m) && all.equal.numeric(solve(m),x$get())) {   ## checks for existance and being identical matrix 'x' 
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()                                       ## matrix 'x'  is retrieved
  m <- solve(data, ...)                                 ## "        "   is inverted  
  x$setinvm(m)
  print("this is our inverse matrix")
  m
}