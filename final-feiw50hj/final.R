#################################################################
# Statistics 133, Lecture 2, Spring 2015
# Final Exam, Monday May 11th, 11:30 am - 14:30 pm

# Please read all instructions carefully.
#################################################################
# IMPORTANT
# Enter your personal information here, between the quotation marks:
name <- "Alma Chen"
github.name <- "almachen"
email.address <- "almachen@berkeley.edu"

#################################################################
# The exam has a total of 100 points and for each task 
# the point is given in square brackets.
# The exam has 6 parts and the total number of points by part is:
# 15 pts : Part I, general R commands
# 20 pts : Part II, plotting
# 15 pts : Part III, apply and by statements
# 15 pts : Part IV, functions 
# 15 pts : Part V, simulations
# 20 pts : Part VI, string manipulation and regular expressions
#100 pts in Total

# If you spend about 30 min or so reading instructions and the like
# this gives you 1.5 min per point -- the exam is LONG ON PURPOSE!
# Please just work your way through it the best you can, prioritizing
# things you can easily do and remember everyone else is also
# racing the clock and will be graded on a curve.

#################################################################
# DO NOT manually set the path anywhere in this script, instead:

# RIGHT NOW!!   In the RStudio menu select:
# Session > Set Working Directory > To Source File Location

# NOTE!
# All "load" statements are included in the file already,
# DO NOT change them in any way or add a path to them,
# it will prevent us from being able to run your script.

# NOTE!
# There are a few calls to the function set.seed()
# DO NOT change these in any way.

#################################################################

#### PART I : General R commands [15 pts]

# [1 pt]
# Create [x], a numeric vector of length 1200 with 
# entries: 3, 6, 9, 12, etc
x <- 3*(1:1200) 

  
# [2 pts]
# Create [y], a list of length 10 where list k
# stores the output of the k-times multiplication
# table, including values up to 1000 so
# y[[1]] is 1, 2, 3, ..., 1000
# y[[2]] is 2, 4, 6, ..., 1000
# y[[3]] is 3, 6, 9, ..., 999

y <- list() 
for ( k in 1:10){
  y[[k]] <- k*(1:1200) 
}


# [1 pt]
# Create [z], a logical vector of length 5000 with entries
# TRUE and FALSE, where each value has
#  a probability 0.3 of being TRUE and 0.7 of being FALSE
# Hint: you can e.g. use [sample()], you will need to set all 4 arguments
set.seed(42)
logic <- c(TRUE, FALSE)
z <- sample(logic, 5000, rep = TRUE, prob = c(0.3, 0.7) )
  
  
# [1 pt]
# Create [v], a numeric vector with the results of 100 rolls of a dice,
# i.e. each element is one of the numbers 1 through 6, with equal probability.
  set.seed(31415)
die <- c(1:6)
v <- sample(die, 100, rep = TRUE)


# [1 pt]
# Create [w], a character vector of length 26 with entries:
# "aa", "ab", "ac", "ad", ..., "az"
# Hint: the variable [letters] is an inbuilt constant in R 
# and stores the alphabet (try typing letters)
set.seed(2718)
w <- paste("a", letters, sep = '')

  
# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Normal random variables (hint: rnorm) with mean 8
# (arrange the values by column, as per default)
set.seed(344)
rnorm_sample <- rnorm(100, 8)
m <- matrix(rnorm_sample, 10, 10) 

  
# For the next few tasks you will use the data frame ChickWeight
# This data frame comes with R so you do not need to load any data.
# To get the size of the data frame type
# dim(ChickWeight)
# To see the first few lines type
# head(ChickWeight)
# There are 4 variables in the dataframe:
# > names(ChickWeight)
# [1] "weight" "Time"   "Chick"  "Diet"  

# [1 pt]
# Create [cw1] a subset of ChickWeight with Chicks on 
# Diet 1 at time 0
cw1diet <- subset(ChickWeight, ChickWeight$Time == 0)
cw1 <- subset(cw1diet, cw1diet$Diet == 1)

  
# [1 pt]
# Create [cw2] a subset of ChickWeight with only Chicks that
# weighed more than 41 at Time 0
cw2 <- subset(cw1diet, cw1diet$weight > 41)

  
# [1 pt]
# Create [cw3] a single number, the average chick weight at time 0
cw3 <- mean(cw1diet$weight)
  

# [1 pt]
# Create [cw4] a random subset of 200 ChickWeight observations (any 200)
random <- sample(1:nrow(ChickWeight), 200)
cw4 <- ChickWeight[random, ]
  

# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 

# [1 pt]
# Create a table [t1] of the smoking history of the mothers using the variables
# [smoke] for the rows of the table and  [number] for the columns.
# 
 t1 <- table(infants$smoke)
#t1 <- as.table(as.matrix(t1))


# [1 pt] 
# Now create a table [t2] which is the same table as before except restricted
# to mothers with College education (use [ed])
# 

 t2 <- table(infants$smoke[infants$ed == "College"])


# [2 pts] 
# Calculate [bdiff], the difference in average birthweight [column bwt] between babies born to mothers
# that are 35 years or older and mothers that are under 35 years of age [use column age for mothers age]
#  
bdiff <- mean(infants$bwt[infants$age >= 35], na.rm = T) - mean(infants$bwt[infants$age < 35], na.rm = T) 

#################################################################
#### PART II : Plotting [20 pts]

##### Cars [8 pts total, 2+3+3]
# We will now use the dataset "mtcars" which is icluded in the R package.
# To look at the dataframe you can just type "mtcars" at the prompt
# It is a data frame of size 32x12

# [2 pts]
# Make a box plot of [mpg] by [cyl] (so 3 boxplots in one plot)
boxplot(mtcars$mpg~ mtcars$cyl)


# [3 pts]
# Use the function [lm()] to fit a regression model of [qsec] (1/4 mile time) regressed on [hp] (horsepower)
# Make a scatterplot of [qsec] versus [hp] and add the regression line to it
# Hint: the regression coefficients are stored in [coef] in the lm object and you can use [abline()] to
# add a line to a plot
plot(mtcars$qsec ~ mtcars$hp)
model <- lm(mtcars$qsec ~ mtcars$hp)
abline(coef(model)[1], coef(model)[2])

# [3 pt]
# Make a scatterplot of [disp] (on y-axis) versus [wt] 
# where disp is the engine displacement in cu. in. and wt is the weight of the car in lb/1000.
# Color the plotting symbol by [am] (automatic or manual) (any 2 colors)
# Edit the labels on the x-axis and y-axis to state what the variables are.
# Hint: first do the plot without adding the colors.

plot(mtcars$disp ~ mtcars$wt)
plot(mtcars$disp ~ mtcars$wt, xlab = "weight of car", ylab = "engine displacement", col = mtcars$am)
#I think the color is white 

##### We will now use the World Bank Data [12 pts]
load("WorldBank.RData")

# [4 pts]
# Make a scatterplot of life expectancy ["life.expectancy"] vs. fertility ["fertility.rate"]
# The plotting symbol should be a small dot (.)
# Please color the symbols by region (pick any color for each region)
# You do _NOT_ need to add a legend saying which color is which region.
# Put on custom made x-axis and y-axis labels that fully describe the variables
# Add a vertical line at fertility=2.1 (the rate needed to maintain constant population size)

plot(WorldBank$life.expectancy ~ WorldBank$fertility.rate, pch = ".", col = WorldBank$region, xlab = "Fertility Rate", ylab = "Life Expectancy")
abline(v = 2.1)

# [4 pts]
# Do you see patterns in the plot?  You just plotted these variables for all years between 1960 and 2014.
# Please redo the plot, but this time put two plots side by side (hint: before plotting set par(mfrow=...) )
# The left plot should include only data from 1960, the right one only from 2014.
#do 2013 instead of 2014
data1 = WorldBank[WorldBank$year == 1960,]
data2 = WorldBank[WorldBank$year == 2013, ]

par(mfrow  = c(1, 2) ) 

plot(data1$life.expectancy ~ data1$fertility.rate, pch = ".", col = data1$region, xlab = "Fertility Rate 1960", ylab = "Life Expectancy 1960")
abline(v = 2.1)

plot(data2$life.expectancy ~ data2$fertility.rate, pch = ".", col = data1$region, xlab = "Fertility Rate 2013", ylab = "Life Expectancy 2013")
abline(v = 2.1)

#[4 pts]
# Make a histogram of GDP only for observations where the lending rating is "IDA"
# The width of the bars should be approximately 250 (use breaks to set how many bars)
# Add an x-axis label and a title.

par(mfrow  = c(1, 1) ) 
## inga edited, data3 doesn't exist, changed to data2 to get partial credit...
#hist(data3$GDP, breaks = 28, xlab = "GDP", main = "GDP of countries with IDA")
hist(data2$GDP, breaks = 28, xlab = "GDP", main = "GDP of countries with IDA")

#################################################################
##### PART III : apply statements [15 pts]

# For the next few tasks you will use the list rain
# (list of length 5, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("rainfallCO.rda")

# [2 pts]
# Create [max.rain], a vector where each entry is the _maximum_ element of the
# corresponding vector in the list rain

max.rain <- sapply(rain, max) 

    
# [5 pts]
# Create [max.diff.rain], a vector of length 5 where each entry is the 
# __maximum absolute difference__ in rainfall on two consecutive days
# Hint: you can use the function [diff()] to get the difference between entry i and i+1 in a vector 
# and the function [abs()] for absolute value
max.diff.rain <- sapply(rain, function(x) max(abs(diff(x))))


# [5 pts]
# Create [prop.rain], a vector of length 5 where each entry is the 
# number of rain days (i.e. rain > 0) as a function of total days

prop.rain <- sapply(rain, function(x) sum((x) !=0))
  

# [3 pts]
# Make a plot with 6 panels and plot the histogram of the rainfall for each weather station
# in a separate panel (there will be one empty panel)
# use an apply statment to the the plotting
par(mfrow=c(2,3))

# inga edited, was apply, need sapply, caused error
#apply(rain, function(x) hist(x))
sapply(rain, function(x) hist(x))



#################################################################
##### PART IV : functions [20 pts]

# [6 pts]
# Write a function, [GenNorm()] which generates a vector of random normal
# plots a histogram, if desired.
# Input :
#   mean (the mean of the normal), default 0
#   sd (the standard deviation), default 1
#   n (the number of observations), default 1000
#   plot.hist (a logical variable, should we plot a histogram), default TRUE
# Output: 
#   a vector of n normal random variables with mean=mean and sd=sd
#   an optional plot

GenNorm <- function(avg = 0, stdev = 1, num = 1000, plot.hist = FALSE){
  
  v <- rnorm(n = num, mean = avg, sd = stdev)
  if (plot.hist == TRUE){
    hist(v)
  }
  return (v) 

}


# [6 pts]
# Write a function [standardizeVar] with
# Input :
#     [m] : a numeric matrix
#     [cols] : TRUE or FALSE (default TRUE)
# Output :
#     a matrix of the same size as [m] where if cols==TRUE the columns have been standardized
#     and if cols==FALSE the rows have been standardized
#     we standardize by subtracting the mean of the column/row and dividing by the sd of the column/row


standardizeVar <- function(m, cols=TRUE){
  if (cols == TRUE){
    for (i in 1:ncol(m)){
      cols_avg = mean(m[,i])
      cols_sd = sd(m[,i]) 
      j = c(1:nrow(m)) 
      current_col <- m[,i]
      m [, i] <- (current_col - cols_avg) / cols_sd
                  
    
      
    }
  }
  
  
  else{
    for (i in 1:nrow(m)){
      row_avg = mean(m[i,])
      row_sd = sd(m[i,])  
      j = c(1:ncol(m)) 
      current_row <- m[i, ]
      m [i, ] <- (current_row - row_avg) / row_sd
      
    }
  }
  
  return (m)
}


# [8 pts]
# Write a function [PermDiff()] that takes
# Inputs
#    [cases] : a numeric vector 
#    [controls] : a numeric vector 
#    [k] : a scalar, the number of permutations, default=5000

#    Assume that [cases] was of length n_cases and [controls] of length m
#    Now select at random n_cases individuals from the whole sample (i.e. all k+m observations)
#    and assign them to be cases, and the remaining m individuals as controls
#    Calculate the value [mean(cases)-mean(controls)] for each of these new samples

# Output
#    A vector of length k (the scalar k) where each element is the value 
#    mean(cases)-mean(controls) for one permutation



PermDiff <- function(cases, controls, k=5000){
  n_cases = length(cases) 
  cases_controls <- c(cases, controls)
  v <- c() 
  
  for (i in (1:k)){
    shuffle <- sample(cases_controls, length(cases_controls)) 
    new_cases <- shuffle[1: n_cases]
    new_controls <- shuffle[(n_cases +1): length(shuffle)] 
    v[i] <- mean(new_cases) - mean(controls) 
  } 
  return (v) 
  
 }



#################################################################
##### PART V : simulations [15 pts]

# leave this here:
set.seed(123456)

# Simulate a simplified lottery and return the number of people who share the
# jackpot.

# Setup:
# Each player can pick 3 numbers from the numbers 1 through 19 (no duplicates)
# There are [k] players that buy a ticket.
# All player with the winning numbers share the jackpot

# Write a function NumJackpot with:

# Input
#    k : number of players
#    B : number of lottery games

# Output:
#    n.jackpot : vector of length B where each entry is the number of people
#                who shared the jackpot in that game

# So in each of the B games you have to :
#    generate the winning numbers
#    set a counter for jackpot winners to 0
#    generate a ticket for each player in turn, if they had the winning numbers 
#    increase the counter by 1

NumJackpot <- function(k, B){
  ticket_pool <- c(1:19) 
  
  n.jackpot <- c() 
  
  for (j in (1:B)){ # play B games 
    jackpot_winners = 0 # set number of winners to 0 at the beginning of each game 
    
  player_tickets <- list() 
  for (i in (1:k)){
    player_tickets[[i]] <- sample(ticket_pool, 3, rep = FALSE) #players have their 3 tickets 
  }
  jackpot <- sample(ticket_pool, 3, rep = FALSE) #3 jackpot numbers are generated 
  
 
  for (i in (1:k)){ #checks each player to see if they won jackpot, increase jackpot_winners 
    if (sum(player_tickets[[i]] %in% jackpot) == 3 ) {
      jackpot_winners = jackpot_winners + 1 
    } 
} 

n.jackpot <- c(n.jackpot, jackpot_winners)
}
return (n.jackpot)
}

# For B = 1000 and each value of k = 100, 500, 1000, 5000
# Plot a histogram of the output from NumJackpot (i.e. four histograms)
#njack1 <- NumJackpot(100, 1000) # inga
#njack2 <- NumJackpot(500, 1000)
#njack3 <- NumJackpot(1000, 1000)
#njack4 <- NumJackpot(5000, 1000)
njack1 <- NumJackpot(100, 100)
njack2 <- NumJackpot(500, 100)
njack3 <- NumJackpot(1000, 100)
njack4 <- NumJackpot(5000, 100)

hist(njack1)
hist(njack2)
hist(njack3)
hist(njack4) 

#################################################################
##### PART VI : string manipulation and regular expressions [20 pts]

phrases <- c("stone", "steel", "sled", "Star", "silly", "cat", "dog", "catcat", "why?", "really!", "how much?", "atatat", "bar", "car")

# [2 pts]
# Create a vector [text1] that lists the elements in phrases that start
# with "st" (case doesn't matter)
ind1 <- grep ("\\<(st|St)", phrases)
text1 <- phrases[ind1]


# [1 pt]
# Create a vector [text2] that lists the elements in phrases that have 
# a match to "ar", _at the end of the phrase_ 
ind2 <- grep("ar\\>", phrases)
text2 <- phrases[ind2]


# [2 pts]
# Create a vector [text3] that lists the elements in phrases that have 
# a match to any multiple of "ta"
ind3 <- grep("ta", phrases)
text3 <- phrases[ind3]

# [2 pts]
# Create a vector [text4] that has the first 3 characters of each element in phrases

text4 <- substring(phrases, 1, 3) 


# [2 pts]
# Create a vector [text5] that has all the elements in phrases that have a punctuation mark 
ind5 <- grep("[[:punct:]]", phrases)
text5 <- phrases[ind5]


# [2 pts]
# Create a vector [phrases2] where you have replaced the first instance of the letter "a" in each word with "@"
phrases2 <- sub("a", "@", phrases)


# [2 pts]
dna <- c("AGGATGATT", "AGCCTTAGC", "AGAGAGCT", "AGTTTCGTA", "CGTGGTGC", "CTAAGTGAC", "GTGGGACC", "GGTAGAGAC", "TAGATTACC")
# Create a vector [match1] with the index for all matches to "A*T" or "G*T"
## inga commented out, command causes an error
#indmatch1 <- grep(("A.T"|"G.T"), dna) 

#match1 <- dna[indmatch1]

# [2 pts]
# Create a vector [dna2] where you have removed all entries whose length is not a multiple of 3
 inddna2 <- c() 
  for (i in (1:length(dna))){
   inddna2[i] <- nchar(dna[i]) %% 3 == 0
 }
  dna2 <- dna[inddna2]

#################################################################

