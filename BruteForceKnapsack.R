#example 1: from Homework 3
b <- c(3,8,9,6,2) #benefit vector
c <- c(12,15,16,16,10) #cost vector
tol <- 55
knapsack2(b,c,tol)

#example 2: from HW 5 Prob. 1
b <- c(29,43,42,27,25)
c <- c(42,65,68,48,32)
tol <- 100

#example 3: Random vectors of length 9
b <- c(3,8,9,6,2,4,5,7,10,11,4,12) #benefit vector
c <- c(12,15,16,16,10,13,20,17,12,16,11,7) #cost vector
tol <- 88
knapsack2(b,c,tol)



knapsack2 <- function(b,c,tol){
  
  strt <- Sys.time() #used to record calculation time 
  
  n <- length(b) # legnth of given benefit and cost vectors
  
  t <- numeric(n) #create a vector with length n
  
  t <- c(rep(0, n)) # make this vector the zero vector of length n
  
  x <- logical(n) # logical tester, a vector of length n containg either TRUE or FALSE, used later
  
  l <- rep(list(0:1), n) #create a list with the vector c(0,1) n times
  
  m <- as.data.frame(expand.grid(l)) # create a matrix with all combinations of c(0,1) with n columns
  
  j <- as.data.frame(matrix(data = 1, nrow = (2^n), ncol = n)) # create a random matrix j to put stuff in
 
  for(i in 1:nrow(m)){ #this creates a solution matrix out of j containing all possible answers 
    if(sum(m[i,]*c) <= tol){ 
      j[i,] <- m[i,] # replaces rows of j with all possible rows of m satisfying sum(m[i,]*c) <= tol
    }
    else {
      j[i,] <- t # if sum(m[i,]*c > tol then replace this row in j with the zero vector (infeasible answer)
    }  
  }
  
  answer <- as.data.frame(matrix(data = 1, nrow = (2^n), ncol = n)) # create a matrix 
  
  for(i in 1:nrow(j)){
    answer[i,] <- j[i,]*b # this fills the matrix we just built with the multiplication of the benefit vector 
  }                       #by each row of the solution matrix
  
  fun <- max(rowSums(answer)) # this vector picks our the max of the row sums of our answer matrix
  #fun is our max benefit amount and where we can find the optimal solution
  
  ans <- numeric(2^n) # creates a vector with 2^n slots
  
  for(i in 1:(2^n)){ 
    ans[i] <- rowSums(answer[i,]) # puts all row sums from answer matrix in a vector
  }
  
  for(i in 1:(2^n)){ #this for loop does two things
    
    if(ans[i] == fun){ # first it finds the index in the row sums vector cooresponding to the max benefit
      
      fin <- j[i,] # this is our solition vecor with 1's and 0's
      
      for(k in k:n){ # this picks out the index for values continaining 1 corresponding to solution vecotor
        if(fin[k] == 1){
          x[k] <- TRUE #this is how we will know what items to take
        }
        else{
          x[k] <- FALSE # false means don't take that index item
        }
      }
      break # ends the loop once a solution is found
    }
    else{
      fin <- "no answer" # just in case no solution exists
    }
  }
  
  f1 <- which(x) #items we should take based on index
  
  f2 <- fun # total benfit based off solution given
  
  fc <- (sum(c*fin)) # total cost based off solution given
  
  ti <- Sys.time()-strt # takes the current time and subtracts the initial time the algoirthm began
  
  return(list("Pick Items" = f1, Profit = f2, Cost = fc, Time = ti)) # list of good stuff
}
