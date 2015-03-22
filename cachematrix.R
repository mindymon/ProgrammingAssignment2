## Put comments here that give an overall description of what your
## functions do

## For R Programming, Assignment #2 we are creating two 
## functions: makeCacheMatrix() that creates a matrix that 
## takes a matrix, calculates and stores its inverse and 
## cacheSolve() that determines whether or not the inverse
## of the matrix passed in has been previously calculated and
## calculates the inverse if it has not.

## Function declaration for makeCacheMatrix that takes 
## in a numeric matrix and stores it in cache
makeCacheMatrix <- function(x = numeric())

{
    ## Initialize myMatrix to NULL
    myMatrix <-NULL
    
    ## set recevies the value of what was passed into makeCacheMatrix()
    set <- function(y)
    {
        ## assign the value of x to y in the makeCacheMatrix() environment 
        x <<-y
        
        ## set the value of myMatrix to NULL in the makeCacheMatrix() environment
        myMatrix <<- NULL
    }
    
    ## assign get to the result of was determined in set
    get <- function() x
    
    ## assign setInverse the result of solve() for what was passed into makeCacheMatrix()
    setInverse <- function(solve) myMatrix <<- myMatrix
    
    ## assign getInverse to the result of 
    getInverse <- function() myMatrix
    
    ## print out the definitions of the functions set, 
    ## get, setInverse and getInverse to the console
    list(set = set, get = get, 
    setInverse = setInverse, getInverse = getInverse)
}


## function declaration for cacheSolve that determines whether or not the inverse
## of the passed in matrix has been calculated and, if not, computes  and returns 
## the inverse
cacheSolve <- function(x, ...) 
{
    ## assign myMatrix variable to the retrieved value of getInverse() 
    ## from makeCacheMatrix()
    ## debug statement for testing value of variable coming in
    ##print(myMatrix)
    
    ## set local value of myMatrix to getInverse of what was passed in
    myMatrix <- x$getInverse()
    
    
    ## check to see if the inverse of myMatrix has been calculated
    ## if it has, print the message and return myMatrix
    if(!is.null(myMatrix))
    {
        message("Getting cached inverse")
        return(myMatrix)
    }
    
    ## local version of matrix used for calculating, set to the value passed into cacheSolve
    localMatrix <- x$get()
    
    ## debug print statement
    ##print (localMatrix)
    
    ## set myMatrix to the inverse of the localMatrix; solve() calculates 
    ## the inverse of the local copy of the matrix
    myMatrix <- solve(localMatrix)
    
    ## set the setInverse value of the matrix in the parent environment
    x$setInverse(myMatrix)
    
    ## return myMatrix 
    myMatrix
}
