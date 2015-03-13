# This function is only for personal control and experiments.
# Created by Alex A. Fernandes

# testCase(directory, n)
# input:
#        directory, local to write the output files
#        n, order of the matrix to test
# output:
#        none
#

testCase <- function (directory, n){                        # function definition
  setwd(directory)                                          # set the work dir
  A <- matrix (runif(n*n, -9999.5, 9999.5), nrow=n, ncol=n) # creates a random matrix of order n x n
  Ainv <-NULL                                               # initialize Ainv 
  valida <- NULL                                            # initialize valida
  
  Rprof(append = TRUE)                                      # Turn on the profiling
  Ainv <- makeCacheMatrix(A)                                # runs the function to create the object (Ainv) to hold 
                                                            #    on the matriz and inverse matrix
  cacheSolve(Ainv)                                          # runs the function responsible to calculate the inverse 
                                                            #    and store it into the object (Ainv)
  
  for (i in 1:1){                                           # Loop to profile the cache using
    cacheSolve(Ainv)                                        # cache reuse test for time profiling 
                                                            #    (if i 1:m, where m is a greaaat value)
  }
  Rprof(NULL)                                               # turns off the profile
  summaryRprof()                                            # print out the profile in a Rporf.out file into the 
                                                            #    "directory" defined in directory variable
  
  eps <- 10^(-8)                                            # Error control for numerical methods aproximations
  
  valida <- Ainv$get() %*% Ainv$getinverse()                # calculates the matrix times it's inverse. 
                                                            #    The result should be the diagonal matrix of order n.
  valida <- sum(valida)                                     # calculates the sum of all valida elements, 
                                                            #    the result should be n
  if (valida >= n-eps && valida <= n+eps)                   # validates the result of the inverse 
    print ("Correct result")
  else
    print ("validation error, not inverse matrix!")
  print (c(valida, n))                                      # print out the valida result and the correct result.
}