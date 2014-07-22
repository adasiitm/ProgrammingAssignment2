#Cache of the matrix is made, Solve inverses the matrix in this spl function

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#A matrix is created
supervector <- makeCacheMatrix(matrix(c(110,220,330,440), nrow=2, ncol=2))


#Cache is checked for availability of the inv matrix, 
#if not present then cache is read back
cacheSolve<-function(x, ...){
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
    
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}

# Shows the matrix as created previously
#supervector$get()

#The inv of matrix is checked in cache & returned if present, else inv is done.
#cacheSolve(supervector)

# running twice would show the comment "getting cached data"
#cacheSolve(supervector)



# #============Testing====================================
# > supervector$get()
# [,1] [,2]
# [1,]  110  330
# [2,]  220  440
# > cacheSolve(supervector)
# [,1]         [,2]
# [1,] -0.018181818  0.013636364
# [2,]  0.009090909 -0.004545455
# > cacheSolve(supervector)
# getting cached data
# [,1]         [,2]
# [1,] -0.018181818  0.013636364
# [2,]  0.009090909 -0.004545455
