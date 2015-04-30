###########################################################
#### Stat 133 Midterm 4

# leave this here:
set.seed(123456)


#### Simulation
# Write a function, [dice_sum()], that takes as input:
# [k] : the number of dice rolled
# [B] : the number of rolls
# and returns:
# [dsum] : a vector of length B where each element is the sum of a roll of k dice

# So if k=1 pick a number between 1 and 6 at random B times and return them,
# if k=2 then in each roll you pick twice a number between 1 and 6 at random,
# calculate their sum, do this B times and return
# and so on.

# We've set the default inputs to k=2 and B=100

dice_sum <- function(k=2, B=100){
	dsum <- c()
	rollk <- c() 
	for (i in 1:B){
		for (j in 1:k) { # finds sum of multple dice rolls 
			
			each_roll <- sample(1:6, 1, replace = TRUE)
			rollk <- c(rollk, each_roll)
			sum_rolls <- sum(rollk)
		}
		dsum <- c(dsum, sum_rolls) 
	} 

return (dsum[length(dsum)]) 
}

#### String manipulation

phrases <- c("dog", "doggy", "den", "good boy", "Really?", "How much?", "Only $8", "dogdogdog", "Oh god")

# Create a vector [text1] that lists the elements in phrases 
# where the SECOND TO LAST character is "o" (lower case o).


sub_phrases = c() 
for (i in 1:length(phrases)){
	sub_phrases <- c(sub_phrases, substring(phrases[i], 1, nchar(phrases[i])-1))
}

indices1 <- grep("o\\>", sub_phrases)
text1 <- phrases[indices1]

# Create a vector [text2] that lists the elements in phrases that
# START with the letter "d"
first_letter = c() 
for (i in 1:length(phrases)){
	first_letter <- c(first_letter, substring(phrases[i], 1, 1))
}
indices2<- grep("d", first_letter) 
text2 <- phrases[indices2]

# Create a variable [no.punct] that equals the number of phrases
#with a punctuation mark in it.
no.punct <- length(grep("[[:punct:]]", phrases))

# Create a vector [even] that is of length 1000 and has the entries
# "even2", "even4", ...
# with no separation between the word and the letter
even_vec <- c(1:1000)*2

even <- paste("even", even_vec, sep='')


# Start with [hotelCal] which is a character string, create 
# a _vector_ (not list) [hotelCal.split] which 
# stores the words of [hotelCal] each as a separate element.
# Also, convert all upper case letters to lower case.
# Please remove all punctuation marks.

hotelCal <- "On a dark desert highway, cool wind in my hair. Warm smell of colitas, rising up through the air. Up ahead in the distance, I saw a shimmering light. My head grew heavy and my sight grew dim I had to stop for the night.  There she stood in the doorway; I heard the mission bell.  And I was thinking to myself: 'This could be heaven or this could be hell'. Then she lit up a candle and she showed me the way."

hotelCal.split <- unlist(strsplit(hotelCal, " "))
hotelCal.split <- hotelCal.split[hotelCal.split != ""] # remove empty string
hotelCal.lower <- tolower(hotelCal.split)
hotelCal.nopunct <- gsub("[[:punct:]]", "", hotelCal.lower)


# Write a function called updateDate. Your function should take the following
# arguments
#   <dates>: a character vector of dates of the form "month, year" (e.g. "May, 2001")
#   <old.yr>: a string indicating the year for which elements will be updated
#     (e.g. "2002")
#
# and return the following
#   <updated.dates>: a character vector of dates where <old.yr> has been replaced by
#     '2015'. This vector should only contain the elements whose date has been
#     updated. For example updateDate(c('May, 2008', 'June, 2011'), '2008') should
#     return 'May, 2015'.
#dates_test <- c('May, 2008', 'June, 2011') 
updateDate <- function(dates, old.yr) {
old_indices <- grep(old.yr, dates)
  updated.dates <- gsub(old.yr, '2015', dates[old_indices])
  return(updated.dates)
}


# Write a function called [abbreviate] that takes in a vector of strings and returns
# a vector of the same length with only the first [k] characters from the orignal vector entries.
#abbre_test <- c("sdlfh", "alwehr", "wlekrh", "awel;kr")
abbreviate <- function(vector, k){
	abbre <- substring(vector, 1, k)
	return (abbre)

}