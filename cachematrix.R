# 'makeCacheMatrix' function is used to cache inverse matrix 'invs' of a matrix 'x'
# This will eliminate recomputing invs ( inverse of x) when the matrix x hasnt changed.
# there are 4 functions within makeCacheMatrix which allow setting and retrieving right values
# set()  --> assign value to x
# get () --> To get the value of matrix x
# setinv() --> assign invesrse ( calculate invserse) 
# getinv ()--> get the value of inverse of x 


makeCacheMatrix <- function(x = matrix()) {
  
    invs<- NULL   

    set <- function(y) {
       x <<- y
       invs <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) invs <<- inverse
    getinv <- function() invs
    list(set = set, get = get, set = setinv,getinv = getinv)
}


# 'cacheSolve' function returns inverse of a matrix that was created with
# function (makeCacheMatrix).
# If invs ( inverse of x) is not NULL then it will return cached inverse value else 
# it will calculate in invs value and cache it. New inverse value is calculated only when
#invs is null i.e eitherinverse of x was not calculated previously ( new matrix) or
# the original matrix x was changed.


cacheSolve <- function(x, ...)
{
  invs <- x$getinv()
  
  if (!is.null(invs)) {
    message('Getting cached inverse value...')
    return(invs)
  } 

  data <- x$get()
  invs <- solve(data, ...)
  x$setinv(invs)
  return(invs)

}



