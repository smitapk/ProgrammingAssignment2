# Thi function is used to cache the inverse matrix 'invs' of a matrix 'x'
# This will eliminate recomputing invs ( inverse of x) unless the original matrix x has changed.
# 4 functions allow 
# set()  --> to assign
# get () --> To get the value of the matrix to be inverted (x) and to
# setinv() --> assign invesrse ( calculate invserse) 
# getinv ()--> To get the value of inverse of x 


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


# This function returns inverse of a matrix that was created with
# function (makeCacheMatrix).
# If the inverse invs of x is not calculated yet is NULL or the matrix x haschanged recently 
# then it will recalculate the inverse of x,
# Else it will return a matrix that is the inverse of 'x'

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



