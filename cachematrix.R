## Catching the Inverse of a Matrix

## The following functions calculate the inverse of a matrix and
## cache the result so that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.


## Creates a speical "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {   ## x is initialized as a function argument.Default defined as empty matrix
  m <- NULL      ## m is set to NULL, initializing it as an object within the environment to be used by later code in the function
  set <- function(y) {
    x <<- y      ## assign the input matrix y to the variable x in the parent environment
    m <<- NULL   ## Assign the value of NULL to the m object in the parent environment. This clears any value of m that had been cached by a prior execution of makecachematrix().
  }
  get <- function() x   ## defines the getter for matrix x
  setinverse <- function(inverse) m <<- inverse   ## access m and set equal to inverse once setinverse is complete
  getinverse <- function() m      ## return the cache inverse of x
  list(set = set, get = get,      ## assigns each of these functions as an element within a list(), and returns it to the parent environment.Returns a fully formed object of type makeCachematrix() to be used by downstream R code
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {   
  m <- x$getinverse()    ## attempts to retrieve an inverse from the object passed 
  if(!is.null(m)) {      ## checks to see whether the result is NULL. Since makeCachematrix() sets the cached inverse to NULL whenever a new matrix is set into the object, if the value here is not equal to NULL, we have a valid, cached inverse and can return it to the parent environment
    message("getting cached data")
    return(m)
  }
  data <- x$get()       ## If the result of !is.null(m) is FALSE, cacheSolve() gets the matrix from the input object, calculates an inverse(), uses the setinverse() function on the input object, and then returns the value of the inverse to the parent environment by printing the inverse object
  m <- solve(data, ...)
  x$setinverse(m)
  m    
}
