## Using cache to manipulate a matrix

## Create the matrix
makeCacheMatrix <- function(x = matrix()) {
       inv = NULL
       set = function(y) {
              # use `<<-` to assign a value to an object in an environment 
              # different from the current environment. 
              x <<- y
              inv <<- NULL
       }
       get = function() x
       setinv = function(inverse) inv <<- inverse 
       getinv = function() inv
       list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## computes the inverse of the matrix
cacheSolve <- function(x, ...) {
  inv = x$getinv()
      if (!is.null(inv)){
              message("getting cached data")
              return(inv)
       }
       mat.data = x$get()
       inv = solve(mat.data, ...)
       x$setinv(inv)
       return(inv)
}
