# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix = function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function assumes that the matrix is always invertible.

cacheSolve = function(x, ...) {
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  matriz = x$get()
  inv = solve(matriz)
  x$setinverse(inv)
  print(inv)
}

# Sample
# x = matrix(c(1, -1/2, 2/5, 2), nrow = 2, byrow = T)
# m = makeCacheMatrix(x)
# cacheSolve(m)
# update
