## Dear grader, please find below R functions to create a cache functionality to inversed
## matrices:

## 1- Function n1 is makeCacheMatrix --> provides a storage for a defined square matrix

makeCacheMatrix <- function(x = matrix()) {
		matrixinv <- NULL 	## this is to create an empty matrix that will be used      						    
		                        ## to store inversed matrix when setup in an R object
		
		set <- function(y){    ## Helps to create and set the matrix as computed in the							 
		                       ## function above to be stored in the list
			x <<- y
			matrixinv <<- NULL
		}
		get <- function()x      ## Call the function to retrieve the matrix
		setinverse <- function(inv) matrixinv <<- inv
		getinverse <- function() matrixinv
		invisible(list(set=set,get=get, setinverse= setinverse, getinverse=getinverse)) 					
		                        ## create a list to be used in cacheSolve below to print 							    
		                        ## the inversed matrix [INVISIBLE is to avoid misleading the 							
		                        ## end user of the fonction when using the prompt]
		
}

## 2- Function n2 is cacheSolve --> allows to return the inverse of the matrix previously stored into makeCacheMatrix
cacheSolve <- function(x, ...) {
        
        matrixinv = x$getinverse() ## Assign the  matrix  previsously stored in getinverse to the empty matrix variable
        if(!is.null(matrixinv)){
				return(matrixinv)
}
        InitMatrix = x$get()
        matrixinv = solve(InitMatrix, ...) 
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(matrixinv)
        
        return(matrixinv)
}