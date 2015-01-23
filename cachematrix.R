
## This program defines two functions makeCacheMatrix and cacheSolve.
## makeCacheMatrix is used to create a separate environment that contains the original matrix passsed as an argument.
## In addition, makeCacheMatrix will also define four functions which will be described below.
## cacheSolve is used to store the cache value for matrix inverse.
## cacheSolve uses the return value of makeCacheMatrix as argument.
## cacheSove will first check if inverse value if cached. If so, then it will return the inverse from cache.
## Otherwise it will calculate inverse and store in cahce i.e. the environment created by makeCacheMatrix




## The function makeCacheMatrix, when called, returns a list of four functions - set,get,getInverse,setInverse
## The function takes a matrix object as argument.
## The return object from this function can be thought of as a pointer to four functions created in a separate environment
## In addition to four funcitons, the environment created by calling makeCacheMatrix will also contain m and x. 
## x is same as the original matrix, the inverse of which is to be calculated.
## m is the cached inverse of x. 
## get function is used in cacheSolve function to retrieve the original matrix.
## set can be called to change the original matrix. When set is called, m is set to NULL to erase the old Inverse from cahce
## getInverse is used to retrieve cached Inverse value
## setInverse is used to set the value of m. m holds cached inverse value for future retrieval.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve acccepts the return value of makeCacheMatrix as argument. 
## First it uses the if statement to check is inverse is already stored (as m in the environment created by the call to makeCaheMatrix)
## If available, inverse value is obtained using getInverse function
## If not, inverse is calculated using the solve function and stored as m using setInverse function.
## finally the cacheSolver returns the inverse (calculated new or returned from cache)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
