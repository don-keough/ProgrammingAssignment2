
##############################################################################
## FUNCTION    : makeCacheMatrix
## ARG1        : an invertible matrix, defaulting to a 1 x 1 matrix
##               of 1 values - the smallest invertible matrix
## DESCRIPTION : a function capable of caching already calculated matrix
##               inverses on unchanged matrices
##############################################################################
makeCacheMatrix <- function( this.matrix = matrix( 1 , nrow = 1 , ncol = 1 ) )
{
    this.inverse <- NULL
    
    set <- function( matrix.value )
    {
        this.matrix  <<- matrix.value
        this.inverse <<- NULL
    }
    
    get        <- function(         ) this.matrix
    setinverse <- function( inverse ) this.inverse <<- inverse
    getinverse <- function(         ) this.inverse
    
    list( set        = set        ,
          get        = get        ,
          setinverse = setinverse ,
          getinverse = getinverse )
}

##############################################################################
## FUNCTION    : cacheSolve
## ARG1        : an invertible matrix
## DESCRIPTION : returns inverse of matrix parameter if invertible
##############################################################################
cacheSolve <- function( matrix.param , ... )
{
    inverse.value <- matrix.param$getinverse()

    if( ! is.null( inverse.value ) )
    {
        message( "getting cached data" )
        return( inverse.value )
    }

    matrix.value <- matrix.param$get()

    inverse.value <- solve( matrix.value ) ## the R solve() method returns the inverse of a matrix

    ## We will use the R solve method to calulate the inverse of a matrix
    ## In case the matrix is not invertible, we will cache/return NULL - suppressing any error messages

    inverse.value <- NULL
    try( inverse.value <- solve( matrix.value ) , silent = TRUE )
    
    matrix.param$setinverse( inverse.value )

    inverse.value
}
