##First function makeCacheMatrix creates a list with four functions for
#storing and retrieving the value of the matrix and its inverse
#The second function cacheSolve takes a numeric matrix, and if it is the
#the same as the last time we called the function, retrieves the value
#of the inverse matrix from cache. If the matrix is different, a new
#inverse matrix is computed


#The function takes a matrix argument and creates a "list" with four
# functions: two for just storing the matrix itself and its inverse
# and another two for setting the new value of the matrix and the new
# value of the inverse matrix
#We set inver (inverse matrix) as NULL everytime we call the function


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The arguments to this function are
# x: the matrix for which we want to compute the inverse
# z: the list of the four functions previously created by makeCacheMatrix
# If z is not specified, we simply create a new vector, in which the 
#the value of the matrix is EMPTY and inver is null
#
#For taking advantage of the previous values of inverse
#stored in the cache "z" must be specified. This can be done by passing
#the LIST created with the above function MakeCacheMatrix
#If the matrix X is different from the stored value in previous uses of
#the function, we compute the new inverse matrix, and store it
#
#If the value of the matrix is the same as the one stored previously
#we take the value of the inverse already stored in cache
#The first time we call this function, or if we don't pass the existing
#LIST with the values already stored in the cache, we force the function
#to compute the inverse matrix
cacheSolve <- function(x, z = makeCacheMatrix(),...) {
  
  
  #we check if there is already a value stored as inverse matrix
  inver <- z$getinverse()
  
  #if there is already a value for the inverse matrix...
  if(!is.null(inver)) {
    #and the matrix we are passing is also the same
    # as the one previously stored
    if(isTRUE(all.equal(z$get(),x))){
      #then we simply take the stored value of
      #inverse and inform the user, exitting
      message("getting cached data for inverse Matrix")
      return(inver)
    }
  }
  #If inver is NULL we must compute the inverse
  #we take the matrix to be computed and compute its inverse
  matrix_to_solve <- x
  inver <- solve(matrix_to_solve, ...)
  z$set(x)
  z$setinverse(inver)
  
  inver
  #list(inver,z$get(),z$getinverse())
  #list(inver,iguales,colocar_set,z$get(),z$getinverse())
  
}