## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: Make an object that will hold a inverse matrix.

# Input:          Matrix (N x N), where N is the order of the square matrix.
# Output:         A object to hold the inverse.
# How to use:     var1 <- makeCacheMatrix(A), where var1 is the object name, and A the matrix to be inverted.
# Object methods: set:         set the matrix values
#                 get:         get the matrix values
#                 setinverse:  set the inverse values
#                 getinverse:  get the inverse values

makeCacheMatrix <- function(x = matrix()) {  # create a function named makeCacheMatrix
  minverse <- NULL                           # variable to store the inverse, initialized by NULL
  set <- function(y) {                       # function inside makeCacheMatrix: Responsible to 
                                             #   set the values of the original matrix
    x <<- y                                  # writing the X values. X is an outside the function 
                                             #   variable, so the <<- is to write properly.
    minverse <<- NULL                        # initializing minverse
  }
  get <- function() x                        # function inside makeCacheMatrix: Responsible to 
                                             #   get the values of the original matrix
  setinverse <- function(solve) minverse <<- solve  # writes the solve variable into the minverse variable.
                                                    #   set the values of minverse.
  getinverse <- function() minverse          # get the values of minverse
  list(set = set, get = get,                 # creates a list of the functions inside makeCacheMatrix
       setinverse = setinverse,              #   To be used as var$method.
       getinverse = getinverse)
  
}

# cacheSolve: Return a matrix that is the inverse of 'x'.

# Input:          Matrix (N x N), where N is the order of the square matrix.
# Output:         the inverse matrix.
# How to use:     var1 <- cacheSolve(A), where var1 is the object name created by makeCacheMatrix,
#                 and A the matrix to be inverted.

cacheSolve <- function(x, ...) {       #function declaration
  minverse <- x$getinverse()           # call getinverse to put the inverse values into minverse variable
  if(!is.null(minverse)) {             # verify if the minverse variable is already defined
    message ("getting cached data")    # print a message if the minverse was not NULL
    return (minverse)                  # return the already calculated inverse matrix
  }
  data <- x$get()                      # get the original x values
  minverse <- solve(data)              # calculates the inverse matrix of x
  x$setinverse(minverse)               # define the new values of the inverse of x
  minverse                             # print the inverse values.
}
