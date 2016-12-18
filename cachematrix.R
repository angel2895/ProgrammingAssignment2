
#Many calculations take up a lot of computation time. So such calculations can be made to store in cache and get it from there whenever needed(considering data does not change)
#So two functions have been made
#makeCacheMAtrix function creates a matrix that can cache its inverse 
#it returns a list which is input to next function 

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    # used to assign a value to an object in an environment that is different from the current environment
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}
#this function calculates the inverse using solve()

cacheSolve <- function(x, ...) {
  inv = x$getinv()
#checks cache, and skips computation if inverse exists
  if (!is.null(inv)){
   
    return(inv)
  }
  #else calculates inverse
  mat.data = x$get()
  inv = solve(mat.data, ...)
  #stores inverse in cache using setinv
  x$setinv(inv)
  return(inv)
}
