# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){



  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

  	m <- matrix(,n.doctors, n.days)
  	m[, 1] <- initial.doctors 
  	for (i in 2:n.days){
  		sample_doctors <- sample(1:n.doctors, 2, replace = FALSE)
  		j <- sample_doctors[1]
  		k <- sample_doctors[2]

  		if (initial.doctors[j] == 1 && initial.doctors[k] == 0){
  			initial.doctors[k] <- sample(c(0,1), 1, replace = TRUE, p = c((1-p), p))
  			m[, i]<- initial.doctors
  		}
  		else if (initial.doctors[j] == 0 && initial.doctors[k] == 1){
  			initial.doctors[j] <- sample(c(0,1), 1, replace = TRUE, p = c((1-p), p))
  			m[, i]<- initial.doctors
  		}
  		else { #(initial.doctors[j] == initial.doctors[k])
  			m[,i]<-initial.doctors	
  		}

  	}

return (m) 
}

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)

initial.doctors10<-c(rep.int(1, 10), rep.int(0, 90))

count.doctors <- function(m){
	v <- c()
	for(i in 1:ncol(m)){
		v<- c(v, length(m[,i][m[,i] ==1]))  
	}
return(v) 
}

# p = 0.2, 0.4, 0.5, 0.6, 0.8

matrix0.2 <- sim.doctors(initial.doctors10, 100, 500, 0.2)
count0.2 <- count.doctors(matrix0.2) 

matrix0.4 <- sim.doctors(initial.doctors10, 100, 500, 0.4)
count0.4 <- count.doctors(matrix0.4) 

matrix0.5 <- sim.doctors(initial.doctors10, 100, 500, 0.5)
count0.5 <- count.doctors(matrix0.5) 

matrix0.6 <- sim.doctors(initial.doctors10, 100, 500, 0.6)
count0.6 <- count.doctors(matrix0.6) 

matrix0.8 <- sim.doctors(initial.doctors10, 100, 500, 0.8)
count0.8 <- count.doctors(matrix0.8) 

plot(1:500, count0.2, xlab = "# days", ylab = "# doctors who adopted drug", 
	main = "Drug Adoption vs Density for various p values", type = "l",
	col = "firebrick", lwd = 3, ylim = c(0, 100))
lines(1:500, count0.4, type = "l", col = "darkorange", lwd = 3)
lines(1:500, count0.5, type = "l", col = "gold2", lwd = 3)
lines(1:500, count0.6, type = "l", col = "darkgreen", lwd = 3)
lines(1:500, count0.8, type = "l", col = "darkslateblue", lwd = 3)

colors <- c("firebrick", "darkorange", "gold2", "darkgreen", "darkslateblue")
legend(10, 100, c(0.2, 0.4, 0.5, 0.6, 0.8), col = colors, lwd=3, 
	title = "p value")