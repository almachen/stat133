#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)
init_test <- bml.init(4,5,0.3)
test_matrix <- matrix(sample(c(0,1,2), 9, replace = TRUE), 3, 3)

bml.init <- function(r, c, p){

  total_spaces <- r*c 

  m<-matrix(sample(c(0,1,2), total_spaces, replace = TRUE, prob = c((1-p), p/2, p/2)), r, c)  
  
   return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

#move the red cars


move_red <-function(m){
	if(ncol(m)>=2){

    blocked<-(m==1)*(m[,c(2:ncol(m),1)]!=0)
    red<-m*(m==1)
    blue<-m*(m==2)
    red_moved<-blue+red*blocked+(red*!blocked)[,c(ncol(m),1:(ncol(m)-1))]
  } else {
    red_moved<-m
  }
  return (red_moved)
}

b <- matrix(2, 1, 3)
c <- matrix(0, 2, 3)
test_blue_car <- rbind(b, c)

move_blue<-function(m){
	if(nrow(m)>=2){
    blocked2<-(m==2)*(m[c(nrow(m),1:(nrow(m)-1)),]!=0)
    red<-m*(m==1)
    blue<-m*(m==2)
    blue_moved<-red+blue*blocked2+(blue*!blocked2)[c(2:(nrow(m)),1),]
  } else {
    blue_moved<-m
}
return (blue_moved)
}

bml.step <- function(m){
  
  m_new <- move_blue(move_red(m)) 
  colnames(m_new) <- c() 
  rownames(m_new) <- c()
  
  if (all(m_new == m)){
    grid.new <- FALSE 
  }
  else{
    grid.new <- TRUE 
  }

  return(list(m_new, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
	counter <- 0 
	m1<-bml.init(r, c, p)
  
	while(counter < 1000){ 
		result<-bml.step(m1)
    m1 <- result [[1]]
		
      if (result[[2]] == FALSE){
        #stop("Gridlock achieved")
        return (list("Number of steps taken: " = counter, "Gridlock matrix:" =m1))
       }
		counter <-counter +1
		
	}

	return (list("Number of steps taken: "=counter, "No gridlock within 1000 steps, matrix:"=m1))

}


