## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function( mtrx = matrix() ) {     ## this is a function (with default:  empty matrix)

     invMtrx <-  NULL
     
     ##----------------------------------
     set <-  function (y) {
             if (is.null(y))                   ##-- handle  setting the NULL matrix
             {
               mtrx       <<-    numeric()     ##-- set the argument of the setter to NULL
             }
             else
             {
               mtrx       <<-    y             ##-- set the argument of the setter to the  'x' variable.
             }
             
             invMtrx     <<- NULL              ##-- clear the internal variable
     }
     
     ##----------------------------------
     get  <-  function() { 
        mtrx
     }
  
     ##----------------------------------
     getInverse  <-  function()  invMtrx
     
     ##----------------------------------
     setInverse  <-  function( inverseM )   invMtrx <<-  inverseM      ##-- set the inverse matrix variable
     
     list (
           set  = set, 
           get  = get, 
           setInverse = setInverse,
           getInverse = getInverse
          )
     
}
##--------------------------------------------------------------------




## This function computes the inverse of the special 'matrix' returned by
##   makeCacheMatrix above. 
## If the inverse has already been calculated ( and the matix has not changed), then 
##  the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <-  x$getInverse()
  
     if ( ! is.null(inv)){   ## matrix has already been created
             message (" getting cached matrix")
             return (inv  )
     }
     else 
       {    ##-- inverse matrix must be calculated 'from scratch'
       
       message ("need to calculate a NEW inverse")
       data    <-  x$get()
       
       if ( is.null(data))                ##-- the matrix is set to NULL  (e.g. not yet set to a matrix)
       {
          message (" cannot calculate the inverse of a NULL")
          warning (" returning NULL  as inverse of a NULL matrix")
          x$setInverse (NULL)
          NULL
       }
       else{                                        ##-- matrix  is NOT NULL,  can solve inverse matrix
         
         message("solve the matrix ")
         newInvMtrx <-  solve (data, ...)
         x$setInverse (newInvMtrx)
         
         newInvMtrx
       }
     }
}

## =============================================== TEST SEQUENCE ==================================================



doTheTest <- FALSE

if ( doTheTest )
{

  print ("=====================  START   ========================================")

  myMat1 <-  matrix ( c(1,2,3,5), 2, 2)
  print (myMat1)

  myMat2  <-  matrix ( c(1,1,9,2,0,0,3,3,3) ,3,3)
  print (myMat2)
  

  CheckCacheMatrix  <- makeCacheMatrix(myMat1)         #  -->  creates the matrix   (but not yet the inverse)

  cacheSolve ( CheckCacheMatrix )                              # --> first time calling (after setting the matrix):    

                                                        #his NOW creates the inverse and stores it  
                                                        #(like the creation of the mean in the vector example)
                                                        #   (answer of the function:  did not yet have the inverse, hence NEW)

                                                        #[1]    "CREATING  NEW INVERSE"                              
                                                        #......


  cacheSolve  ( CheckCacheMatrix)                         # ->   calling the  inverse  a  2nd time  (!!!)
                                                        #  (answer:   already had the inverse,  returning the cached version)
                                                        #[1]   "RETURNING THE CACHED (!) INVERSE"             
                                                        #......




####
####   NOW:   overwriting the old matrix with another one.....
####

  CheckCacheMatrix$set( myMat2  )                 # ->  inserting a different matrix  
                                                #  side effect:  --> clearing the internal  Inverse


  m <- cacheSolve ( CheckCacheMatrix)             #  the inverse must be created from scratch because the matrix changed.
  print (m)                                        
                                                #[1]    "CREATING  NEW INVERSE"  
                                                #......

  print ("=====================  END   ========================================")
}
