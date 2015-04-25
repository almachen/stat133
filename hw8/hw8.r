#simpleboot
#boot.lm
xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  y_vec <- c() 

  y_vec <- c(y_vec, sample(y, length(y), rep= TRUE)) 
  

 # y_vec <- (tapply(x, y, function(z) {sample(z, rep = TRUE)}) ) 
  return (y_vec)  
  

}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values

  err_vec <- sample(err, length(err), rep = FALSE) 
  y <- fit + err_vec
 return (y) 
 

}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  
 df = data.frame(x,y)

  if (degree == 1) {
    
    model <- lm(y~x, df)
    coeff <- as.vector(model$coefficients)
  } 

  else { 
    model <- lm(y~x +I(x^2), df)
    coeff <- as.vector(model$coefficients)
  } 
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  

 
  ### Use fitModel to fit a model to this bootstrap Y 
 x <- data [ ,1]
 y <- data [ ,2] 
 

 if (is.null(fit)){ # this doesn't output length 50 
  output <- genBootY(x, y)
 }
 else {
  y_fit <- fit [ ,1]
  y_res <- fit[, 2]
  output <- genBootR(y_fit, y_res)
 }

 model <- fitModel(x, output, degree = degree) 
 return (model) 

}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  


  coeff <- list() 
  fit <- matrix(,length(data$x),2) 
  x <- data[ , 1]
  y <- data[ , 2]
  fit_values <- lm(y ~ x)$fitted.values
  residuals <- lm(y ~ x)$residuals 
  fit[, 1] <- fit_values
  fit[, 2] <- residuals 
  #50 rows 2 columns

  coeff[[1]] <- t(replicate(B, oneBoot(data, fit = NULL, degree = 1)))
  coeff[[2]] <- t(replicate(B, oneBoot(data, fit = NULL, degree = 2)))
  coeff[[3]] <- t(replicate(B, oneBoot(data, fit = fit, degree = 1)))
  coeff[[4]] <- t(replicate(B, oneBoot(data, fit = fit, degree = 2)))
  

  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data

  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out

  # curve order? 
  #the curve for the trueCoeff 

  plot ( y ~ x, type = "p")

  if (ncol(coeff) == 3){

    #mapply(curve, )

  mapply( function(a, b, c) {
    curve(a*(x^2) + b*x + c, add = TRUE, 
      col = rgb (0, 0, 0.8, alpha = 0.2))},
    coeff[, 3], coeff[, 2], coeff[ , 1])  

  curve(trueCoeff[3]*x^2 + trueCoeff[2]*x + trueCoeff[1], add = TRUE,
    col = "firebrick", lwd = 3)
  }

  else { 

  #col2 = slope, col1 = intercept
    

    mapply(abline, a = coeff[ ,1], b = coeff[ ,2], 
      col = rgb (0, 0.5, 0.5, alpha = .2))
    

  curve(trueCoeff[1]*x^2 + trueCoeff[2]*x + trueCoeff[3], add = TRUE, col = "firebrick", lwd = 3)
  }

  }
 



### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData) # 
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}
