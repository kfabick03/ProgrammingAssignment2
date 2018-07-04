## makeCacheMatrix function initializes a matrix that will cache the inverse 
## of the matrix to be called later.  cacheSolve function will recall the cached 
## matrix from makeCacheMatrix if it has already been solved. 

## this function will create a matrix for the inverse to be cached in

makeCacheMatrix <- function(x = matrix()) {   #define argument default a matrix
      invmatrix <- NULL                       #initialize invmatrix as NULL
      
      set <- function (y) {                   #set will assign new value to 
            x <<- y                           #matrix in parent environ
            invmatrix <<- NULL                #if new matrix, reset invmatrix 
      }
      get <- function () x                    #get will return the value of matrix arg using annon function
      
      setinverse <- function (inverse) invmatrix <<- inverse  #assign value of invmatrix in setinverse parent env
      getinverse <- function () invmatrix     #uses annon function to get value of invmatrix
      
      list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                                              #generates list to allow for referencing function with $ operator
}


## this function will compute the inverse of the matrix returned by the function
## above and will retrieve the inverse from the above function if it has already
## been calculated.

cacheSolve <- function(x, ...) {          #define arguments of function as input x and other args as needed
    invmatrix <- x$getinverse()           #assign invmatrix the value it had in above function 
    if (!is.null(invmatrix)) {            #if invmatrix was calculated above, ie is not empty
          message ("getting cached data") #send message to user
          return (invmatrix)              #return the cached inverse from function above
    }
    data <- x$get()                       #gets x from the argument in makeCacheMatrix
    invmatrix <- solve (data, ...)        #calculates the inverse of x
    x$setinverse (invmatrix)              #sets the inverse as invmatrix
    invmatrix                             #returns inverse of the matrix
}
