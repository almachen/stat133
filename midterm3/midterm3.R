# Midterm 3

# Write a function called numStarElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "*" symbol
#
# and return the following
#   <num.star>: an integer indicating how many elements of <chvec> contain the "*"
#     symbol. For example: numStarElements(c('star', 'st*r', '***')) should return 2

startest <-  c("star", "st*r", "****", "*dfd*")
numStarElements <- function(chvec) {
	indices <- grep("\\*", chvec)
	return (length(indices)) 
}



# Write a function called numDigits that counts the number of (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the number of digits in chvec)

digitstest <- c("sdf3klhsdf934") 
digitstest1<- c("1z3p ! 21")
digitstest2 <- c("abcdefg")

numDigits <- function(chvec){
	chvec <- unlist(strsplit(chvec, "")) 
	indices <- grep("[[:digit:]]", chvec)
	total <- length(indices)
	return(total)  
}



# Some test cases:
# all.equal(numDigits("1z3p ! 21"), 4)
# all.equal(numDigits("abcdefg"), 0)



# Write a function called hisToTheir that converts every instance of him
# in a string to them; every instance of he to they and every instance of 
# his to their. You can assume everything is lower case. Be careful not to 
# replace words that contain him/he/his (eg. you don't want to replace the 
# with tthey). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <theirchvec>: The same character vector with the required substitutions.

histest <- c("he went to the store his mother gave him tthey himm he")
hisToTheir <- function(chvec) {
	chvec <- tolower(chvec) # convert to lower case 
	space <- c(" ")
	chvec <- paste(space, chvec, sep = "")
	chvec <- paste(chvec, space, sep = "")
	chvec <- gsub(" he ", " they ", chvec)
	chvec <- gsub(" him ", " them ", chvec)
	chvec <- gsub(" his ", " their ", chvec)
	theirchvec <- substr(chvec, 2, nchar(chvec)-1)
	return(theirchvec)

}
#A test case
all.equal(
  hisToTheir("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)



# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

testletter <- c("aabbccccdddd")
mostCommonLetter <- function(chvec){
	chvec <- tolower(chvec) 
	chvec <- gsub("[[:digit:]]", "", chvec)
	chvec <- gsub("[[:punct:]]", "", chvec)
	chvec <- gsub("[[:space:]]", "", chvec)
	chvec <- unlist(strsplit(chvec, ""))
	chvec <- chvec[chvec != ""] 
	tab <- table(chvec) 
	letter <- max(as.vector(tab)) 
	return(letter)

}