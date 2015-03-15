#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.

steps.till.gridlock <- function(d){
	result <- c()
	size <- c(10, 20, 50)
	
	num_free_flow <- c() 

	for(j in 1:length(size)){

			counter <- 0
			v1 <-replicate(1000, bml.sim(size[j], size[j], d)[[1]])  
			v <- c() 
			
			for (i in 1:length(v1)){
				if (v1[i] == 1000){
					counter <- counter +1 
				}
			}
			num_free_flow<- c(num_free_flow, counter)
		
			for (i in 1:length(v1)){
				if (v1[i] != 1000){
					v <- c(v, v1[i])

				}
			}

			if (length(v) == 0){
				v<-c(0)
			}

			result<- c(result, mean(v)) 
		 
			
	}
	return (list(result, num_free_flow))
}

#MASTER TABLE FOR DENSITY = 0.3
master_matrix0.3<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.3)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.3[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.3)
steps <- result[[1]]
master_matrix0.3[, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.3[, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.3[ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.3[ , 5] <- percent_freeflow 


#MASTER TABLE FOR DENSITY = 0.4
master_matrix0.4<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.4)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.4[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.4)
steps <- result[[1]]
master_matrix0.4[, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.4[, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.4[ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.4[ , 5] <- percent_freeflow 



#MASTER TABLE FOR DENSITY = 0.5 
master_matrix0.5<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.5)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.5[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.5)
steps <- result[[1]]
master_matrix0.5 [, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.5 [, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.5 [ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.5 [ , 5] <- percent_freeflow 




#MASTER TABLE FOR DENSITY = 0.7 
master_matrix0.7<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.7)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.7[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.7)
steps <- result[[1]]
master_matrix0.7 [, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.7 [, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.7 [ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.7 [ , 5] <- percent_freeflow 




#MASTER TABLE FOR DENSITY = 0.8 
master_matrix0.8<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.8)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.8[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.8)
steps <- result[[1]]
master_matrix0.8[, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.8[, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.8[ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.8[ , 5] <- percent_freeflow 



#MASTER TABLE FOR DENSITY = 0.9 
master_matrix0.9<- matrix(, 3, 5)
size <- c(10, 20, 50)
colnames(master_matrix0.9)<-c("size", "steps till gridlock", "# free flowing", "%free flowing", "% gridlock")

#column 1
master_matrix0.9[,1] <- c(10, 20, 50) 

#column 2
result <-steps.till.gridlock(0.9)
steps <- result[[1]]
master_matrix0.9[, 2] <- steps 

#column 3
num_free <- result[[2]] 
master_matrix0.9[, 3] <- num_free

#column 4
percent_gridlock <- num_free / c(1000, 1000, 1000) *100
master_matrix0.9[ , 4] <- percent_gridlock

#column 5
percent_freeflow <- 100- percent_gridlock
master_matrix0.9[ , 5] <- percent_freeflow




#-------------------------------------------------------------------

#GRAPHS 

#graph #1 is % free flowing vs density
p_values <- c(0.3, 0.4, 0.5, 0.7, 0.8, 0.9)
p_values2<- c(0.4, 0.5, 0.7, 0.8, 0.9)
indicator <- c(1:6)


colors <- c("mediumseagreen", "brown1", "mediumpurple4") #10, 20, 50

free_flowing10<- c(100, 96.7, 59.9, 0.2, 0.1, 0)
free_flowing20<- c(100, 97.5, 20.7, 0, 0, 0)
free_flowing50<- c(100, 87.4, 4.3, 0, 0, 0)

#lines
plot(p_values, free_flowing10, type = "l", col = "mediumseagreen", xlab = "density", ylab = "% free flowing", main = "%free flowing vs density", lty = 1, lwd = 3)
par(new = T)
plot(p_values, free_flowing20, type = "l", col = "brown1", xlab = "density", ylab = "% free flowing", main = "%free flowing vs density", lty = 2, lwd = 3)
par(new = T)
plot(p_values, free_flowing50, type = "l", col = "mediumpurple4", xlab = "density", ylab = "% free flowing", main = "%free flowing vs density", lty = 3, lwd =3)
legend(0.8, 90, c(10, 20, 50), col = colors, lty = c(1,2,3), lwd = 3, title = "matrix size")


#graph #2 is % gridlock vs density
gridlock_10 <- 100-free_flowing10
gridlock_20 <- 100-free_flowing20
gridlock_50 <- 100-free_flowing50 

plot(p_values, gridlock_10, type = "l", col = "mediumseagreen", xlab = "density", ylab = "% gridlock", main = "%gridlock vs density", lty = 1, lwd = 3)
par(new = T)
plot(p_values, gridlock_20, type = "l", col = "brown1", xlab = "density", ylab = "% gridlock", main = "%gridlock vs density", lty = 2, lwd = 3)
par(new = T)
plot(p_values, gridlock_50, type = "l", col = "mediumpurple4", xlab = "density", ylab = "% gridlock", main = "%gridlock vs density", lty = 3, lwd =3)
legend(0.8, 90, c(10, 20, 50), col = colors, lty = c(1,2,3), lwd = 3, title = "matrix size")


#graph #3 is avg num steps taken till gridlock
steps_10 <- c(master_matrix0.4[1,2],
 master_matrix0.5[1,2], master_matrix0.7[1,2], master_matrix0.8[1,2],
 master_matrix0.9[1,2])

steps_20<-c(master_matrix0.4[2,2],
 master_matrix0.5[2,2], master_matrix0.7[2,2], master_matrix0.8[2,2],
 master_matrix0.9[2,2])

steps_50 <- c(master_matrix0.4[3,2],
 master_matrix0.5[3,2], master_matrix0.7[3,2], master_matrix0.8[3,2],
 master_matrix0.9[3,2])

df10 <- data.frame(x = p_values2, y = steps_10)
df20 <- data.frame(x = p_values2, y = steps_20)
df50 <- data.frame(x = p_values2, y = steps_50)

minX = min(df10$x, df20$x, df50$x)
minY = min(df10$y, df20$y, df50$y)
maxX = max(df10$x, df20$x, df50$x)
maxY = max(df10$y, df20$y, df50$y)


plot(df10, type = "l", col = "mediumseagreen", ylim = c(minY, maxY), xlab = "density", ylab = "# steps taken", main = "avg # of steps taken until gridlock", lwd = 3)
lines(df20, col = "brown1", lty = 2, lwd = 3)
lines(df50, col = "mediumpurple4", lty = 3, lwd =3)
legend(0.8, 450, c(10, 20, 50), col = colors, lty = c(1,2,3), lwd = 3, title = "matrix size")

#vary the shape

gridlock20 <- function(d){
	result <- c()
	size <- c(10, 20, 50)
	
	num_free_flow <- c() 

	for(j in 1:length(size)){

			counter <- 0
			v1 <-replicate(1000, bml.sim(20, 20, d)[[1]])  
			v <- c() 
			
			for (i in 1:length(v1)){
				if (v1[i] == 1000){
					counter <- counter +1 
				}
			}
			num_free_flow<- c(num_free_flow, counter)
		
			for (i in 1:length(v1)){
				if (v1[i] != 1000){
					v <- c(v, v1[i])

				}
			}

			if (length(v) == 0){
				v<-c(0)
			}

			result<- c(result, mean(v)) 
		 
			
	}
	return (list(result, num_free_flow))
}


gridlock16 <- function(d){
	result <- c()
	size <- c(10, 20, 50)
	
	num_free_flow <- c() 

	for(j in 1:length(size)){

			counter <- 0
			v1 <-replicate(1000, bml.sim(16, 25, d)[[1]])  
			v <- c() 
			
			for (i in 1:length(v1)){
				if (v1[i] == 1000){
					counter <- counter +1 
				}
			}
			num_free_flow<- c(num_free_flow, counter)
		
			for (i in 1:length(v1)){
				if (v1[i] != 1000){
					v <- c(v, v1[i])

				}
			}

			if (length(v) == 0){
				v<-c(0)
			}

			result<- c(result, mean(v)) 
		 
			
	}
	return (list(result, num_free_flow))
}

gridlock10 <- function(d){
	result <- c()
	size <- c(10, 20, 50)
	
	num_free_flow <- c() 

	for(j in 1:length(size)){

			counter <- 0
			v1 <-replicate(1000, bml.sim(10, 40, d)[[1]])  
			v <- c() 
			
			for (i in 1:length(v1)){
				if (v1[i] == 1000){
					counter <- counter +1 
				}
			}
			num_free_flow<- c(num_free_flow, counter)
		
			for (i in 1:length(v1)){
				if (v1[i] != 1000){
					v <- c(v, v1[i])

				}
			}

			if (length(v) == 0){
				v<-c(0)
			}

			result<- c(result, mean(v)) 
		 
			
	}
	return (list(result, num_free_flow))
}

gridlock5 <- function(d){
	result <- c()
	size <- c(10, 20, 50)
	
	num_free_flow <- c() 

	for(j in 1:length(size)){

			counter <- 0
			v1 <-replicate(1000, bml.sim(5, 80, d)[[1]])  
			v <- c() 
			
			for (i in 1:length(v1)){
				if (v1[i] == 1000){
					counter <- counter +1 
				}
			}
			num_free_flow<- c(num_free_flow, counter)
		
			for (i in 1:length(v1)){
				if (v1[i] != 1000){
					v <- c(v, v1[i])

				}
			}

			if (length(v) == 0){
				v<-c(0)
			}

			result<- c(result, mean(v)) 
		 
			
	}
	return (list(result, num_free_flow))
}

#MASTER TABLE FOR DENSITY = 0.6
master_matrix0.6<- matrix(, 4, 4)
colnames(master_matrix0.6)<-c("size", "length/width ratio", "%free flowing", "steps till gridlock")

#column 1
master_matrix0.6[,1] <- c("20x20", "16x25", "10x40", "5x80")

#column 2
master_matrix0.6[, 2] <- c(1, 25/16, 4, 16)

#stuff
master_matrix0.6[1,3] <- 0.23
master_matrix0.6[1,4] <- 81.17
master_matrix0.6[2,3] <- 45.7
master_matrix0.6[2,4] <- 107.6
master_matrix0.6[3,3] <- 64.6
master_matrix0.6[3,4] <- 246.3
master_matrix0.6[4,3] <- 99.9



plot(c(1, 1.56, 4, 16), c(0.23, 45.7, 64.6, 99.9), xlab = "length to width ratio of grid", ylab = "% free flowing", main = "how shape affects % free flowing at 0.6 density", type = "l", col = "cadetblue", lwd = "3")





