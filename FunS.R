makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##########################################
## How it works
## Step 1. Create a matrix x1
##    x1<-matrix(c(0.5,-0.25,-1,0.75),2,2)
## Step2. call makeCacheMatrix function
##    m1 <- makeCacheMatrix(x1)
## Step3. Make an inverse matrix by calling cacheSolve function
##    n1 <- cacheSolve(m1)
## Step4. Print the inverse matrix n1
##    n1
###########################################
###########################################
## Example
## > x1<-matrix(c(0.5,-0.25,-1,0.75),2,2)
## > m1 <- makeCacheMatrix(x1)
##> n1 <- cacheSolve(m1)
##> n1
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4