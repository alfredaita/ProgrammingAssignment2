makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setinvm <- function(invm) m <<- invm
  
  
  getinvm <- function() m
  
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}

cachesolve <- function(x, ...) {
  # print(x)
  m <- x$getinvm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvm(m)
  print("this is our mean")
  m
}
# these lines run the function
##>   l<-makeVector(1:50)   
##>   cachemean(l)    will print and or store etc
##>   l$getmean() 