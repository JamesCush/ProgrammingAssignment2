# 	Date:		06/18/2015
# 	Class: 		R Programming (rprog-015)
# 	Assignment	#2 
#	Due Date	06/21/2015
# ----------------------------------------------------
# Overall Scope:
# The following are 2 R functions that are able to cache potentially 
# time-consuming computations. This will save on time and increase 
# efficiency, overall. 
# ----------------------------------------------------

# Section 1:
# The first R function, makeCacheMatrix, creates a special "matrix"
# object that can cache its inverse. For this assignment, we will
# assume that the matrix supplied is always invertible.

makeCacheMatrix	<-	function(x = matrix()) 
				{
				m 	<-	NULL
				set <-	function(y) 
						{
						x <<- y
						m <<- NULL
						}
				get		<-	function() x
				setInv 	<-	function(inv) m <<- inv
				getInv 	<-	function() m
				list(set	= set
					,get	= get
					,setInv	= setInv
					,getInv	= getInv)
				}

# Section 2:
# The second R function, cacheSolve, n computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above, in Section 1. 
# If the inverse has already been calculated (and the matrix has 
# not changed), then the cacheSolveolve should retrieve the inverse 
# from the cache.

cacheSolve 	<- function(x, ...) 
			{
			m	<- x$getInv() 
			if	(!is.null(m)) 
				{ 
                message("getting cached data")
                return(m) 
				}
			data	<- x$get() 
			m 		<- solve(data) 
			x$setInv(m) 
			m 
			}

	