## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {     #first function
        invmat <- NULL                          #sets the initial inverse matrix to Null
        set <- function(y) {                    #set function to run when makeCacheMatrix is called
                mat <<- y                       #superassigns mat variable from value of the entered matrix
                invmat <<- NULL                 #superassigns the inverse matrix variable (invmat) to Null 
        }
        get <- function() mat                   #creates the get function which returns the value of mat variable
        setinv <- function(solve) invmat <<- solve
                                                #creates the setinv function which superassigns the invmat variable
                                                #to the argument solve
        getinv <- function() invmat             #creates the getinv function which returns the value of the inverse 
                                                #of the matrix
        list(set = set, get = get,              #creates the list with the set, get, setinv, and getinv functions
             setinv = setinv,
             getinv = getinv)
             
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- mat$getinv()                  #load the value stored as invmat from the list 
        if(!is.null(invmat)) {                  #if the value is not NA, then
                message("getting cached data")  #display this message
                return(invmat)                  #return the value saved in the list for invmat
        }
        data <- mat$get()                       #if there is no inverse matrix, then store the value as data
        invmat <- solve(data, ...)              #solve the inverse matrix
        mat$setinv(invmat)                      #save the inverse matrix to the list
        invmat                                  #then display the value of the inverse matrix
        
}
