##This program intends to construct an object that is a matrix
#but have some especial functions, one of then is a Solve 
##function that usually calculates the inverse, but this function
##CacheSolve cache the solution and reutilizes it

makeCacheMatrix <- function(x = matrix()) {
    #This function creates the matrix, similar tho the argument x
    inverse <- NULL
    #This enables to set new matrix if the object is alread creates
    #or was created empty
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    #This return the matrix
    get <- function() x
    #This will be utilized by cacheSolve to cache the inverse
    setInverse <- function(inv) inverse <<- inv
    #This will be utilized by cacheSolve to reutilises the inverse
    #if avaliable
    getInverse <- function() inverse
    #Returns a list with functions created
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



#Solve smartly the inverse by storing the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    #See if the inverse is stored
    if(!is.null(inverse)){
        message("getting cached data")
        inverse
    }
    #Calculates the inverse
    else{
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
    }
    
}
