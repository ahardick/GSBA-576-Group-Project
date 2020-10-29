#######TEST FIRST CHANGE#################
##########BOOLEAN EXPRESSIONS,###########
##############WHILE LOOPS,###############
####AND IMPLEMENTING GRADIENT DESCENT####
########Test Second Change##############

#########################
###BOOLEAN EXPRESSIONS###
#########################

#BOOLEAN EXPRESSIONS ALLOW YOU TO CHECK 
#LOGICAL STATEMENTS MAKING COMPARISONS BETWEEN
#VARIABLES.  THE OUTPUT OF A LOGICAL STATEMENT 
#IS A BOOLEAN VARIABLE THAT CAN TAKE ON 
#THE VALUES OF EITHER TRUE (1) OR FALSE (0).

#TRY RUNNING THESE LINES:
5>0 #A TRUE STATEMENT
-1>3 #A FALSE STATEMENT

#A BASIC IF-THEN CONDITIONAL STATEMENT
x <- 5
if (x>0){
  print("Positive Number!")
}

#A BASIC IF-THEN STATEMENT WITH ELSE CONDITION
x <- -1
if (x>0){
  print("Positive Number!")
} else {
  print("Not a Positive Number!")  
}

##AN EXAMPLE USING THE "AND" OPERATOR &&
x <- 4
if (x<=10 && x>=5){
  print("x is between 5 and 10 inclusive!")
} else {
  print("x is not between 5 and 10 inclusive!")  
}

##AN EXAMPLE USING THE "OR" OPERATOR ||
x <- 4
if (x>5 || x<2){
  print("x is either greater than 5 OR smaller than 2")
} else {
  print("x must be in between 2 and 5 incusive")  
}

#############################################
##A BASIC WHILE LOOP WITH LOGICAL CONDITION##
#############################################

x<-0
#KEEPS PRINTING THE VALUE OF x AS LONG AS x<5
while (x<5) {
  print(x) #prints x within loop (othwerwise, R supresses the value)
  x <- x + 1
}

#############################################
#A MORE GENERAL EXAMPLE W/ ITERATION COUNTER#
#############################################

x <- 0
INC <- .001 #INCREMENT BY WHICH TO INCREASE x AT ANY ITERATION
ITER <- 0 #ITERATION COUNTER
LIMIT <- 500 #SETS LIMIT FOR x VALUE

#KEEPS PRINTING THE VALUE OF x AS LONG AS x<LIMIT IS TRUE
while (x<LIMIT) {
  ITER <- ITER+1
  writeLines(paste("Iteration:", ITER)) #ALLOWS YOU TO PRINT WITHIN LOOP
  writeLines(paste("x:", x)) #ALLOWS YOU TO PRINT WITHIN LOOP
  x <- x + INC #INCREASE x BY INC EACH ITERATION
}

##################################
#WHAT DO YOU THINK THIS ONE DOES?#
##################################

x <- 0 #INITIALIZED VALUE OF x
while (x<10) {
  if (x<=5){
    print(x)
    print("x is less than or equal to 5")
    x <- x + 1
  } else {
    print(x)
    print("x is greater than 5")
    x <- x + 1
  }
} 

###############################
##AN EXAMPLE WITH USER INPUT:##
###############################

#THE != SIGN IS THE BOOLEAN OPERATOR FOR NOT EQUAL TO
#THE DOUBLE EQUALS SIGN == IS THE BOOLEAN OPERATOR FOR EQUALITY

CORRECT <- FALSE
while (CORRECT==FALSE) {   
  print("What is 2+2?")
  #NEXT LINE TAKES INPUT FROM USER IN THE CONSOLE WINDOW
  RESPONSE<- readline(prompt="Please, enter your ANSWER: ")
  if (RESPONSE!=4){
    print("Sorry, your answer is incorrect.  Try again:")  
  } else {
    print("Good job!  You got it right!")
    CORRECT <- TRUE
  }
}

###################################################
####IMPLEMENTING THE GRADIENT DESCENT ALGORITHM####
###################################################

library(datasets) #preloaded datasets library
data(mtcars) #call the mtcars dataset
head(mtcars) #show first 6 observations
plot(mtcars$disp, mtcars$mpg) #plot displacement against mpg

#LET'S CONSIDER A SIMPLE EXAMPLE WHERE WE ESTIMATE:
#mpg=B0+B1*disp+u OR MORE GENERALLY, y=B0+B1*x+u

#LET'S WRITE A FUNCTION THAT IMPLEMENTS THE FULL BATCH
#GRADIENT DESCENT ALGORITHM.

#THE FUNCTION WILL TAKE 6 ARGUMENTS AS INPUTS:
# the x-data
# the y-data
# initialized guess vector for (B0, B1)
# the learning rate
# stopping rule tolerance
# maximum iterations

gradientDesc <- function(x, y, guess, learn_rate, tolerance, max_iter) {
  plot(x, y, pch = 20) #plot the data
  
  ######################
  ####INITIALIZATION####
  ######################
  
  n<-length(y) #NUMBER OF OBSERVATIONS
  b_0 <- guess[1] #INITIALIZE GUESS FOR INTERCEPT
  b_1 <- guess[2] #INITIALIZE GUESS FOR SLOPE COEFFICIENT
  y_hat <- b_0 + b_1 * x #FITTED VALUES AT INITIAL GUESS
  MSE <- sum((y - y_hat) ^ 2) / n #MSE AT INITIAL GUESS
  RMSE <- sqrt(MSE) #RMSE AT INITIAL GUESS
  converged <- FALSE #SET CONVERGENCE INDICATOR TO "FALSE"
  iterations <- 0 #SET ITERATION COUNT AT 0
  
  ##################
  ####ITERATIONS####
  ##################
  
  while(converged == FALSE) {
    
    ###################
    ####UPDATE RULE####
    ###################
    
    #COMPUTES GRADIENT ELEMENT BY ELEMENT AND UPDATES#
    b_1_new <- b_1 - learn_rate * (-1)*((1 / n) * (sum((y - y_hat) * x)))
    b_0_new <- b_0 - learn_rate * (-1)*((1 / n) * (sum(y - y_hat)))
    ##STORES NEW VALUES FOR NEW GUESS##
    b_1 <- b_1_new
    b_0 <- b_0_new
    y_hat <- b_0 + b_1 * x #COMPUTES FITTED VALUES AT NEW GUESS
    MSE_new <- sum((y - y_hat) ^ 2) / n #COMPUTES MSE AT NEW GUESS
    RMSE_new <- sqrt(MSE_new) #COMPUTES RMSE AT NEW GUESS
    
    #####################
    ####STOPPING RULE####
    #####################
    
    if(abs(RMSE - RMSE_new) < tolerance) {
    abline(b_0, b_1, col='red', lwd=2.5) 
    converged <- TRUE
    return(paste("Optimal b_0:", b_0, "Optimal b_1:", b_1, "Iterations:", iterations, "RMSE:", RMSE_new))
    break
    } else {
    print(c(iterations, RMSE_new)) #print the iteration # and RMSE at each iteration
    iterations <- iterations + 1
    RMSE<-RMSE_new
      if(iterations > max_iter) { 
        abline(b_0, b_1, col='red', lwd=2.5)
        converged <- TRUE
        return(paste("Optimal b_0:", b_0, "Optimal b_1:", b_1, "Max Iterations:", max_iter, "RMSE:", RMSE_new))
      }
    }
  }
}

lr <- 2.9e-5 #SET LEARNING RATE
tol <- 1e-10 #SET TOLERANCE FOR STOPPING RULE
maxit <- 2500000 #SET MAXIMUM ITERATIONS FOR OVERRIDE OF STOPPING RULE

guess <- c(0,0) #DETERMINISTIC GUESS FOR (B0, B1)
#guess <- c(runif(1,-100,100), runif(1,-100,100)) #A RANDOM GUESS

#RUN GRADIENT DESCENT BY CALLING THE FUNCTION THAT WE WROTE
gradientDesc(mtcars$disp, mtcars$mpg, guess, lr, tol, maxit)

#COMPARE RESULTS TO ONE-STEP PSEUDOINVERSE SOLUTION:
M1 <- lm(mpg ~ disp, mtcars)
M1$coefficients

#PLOT PSEUDOINVERSE REGRESSION LINE ON TOP OF GRADIENT DESCENT ESTIMATE
abline(M1$coefficients[1], M1$coefficients[2], col='darkblue', lwd=2.5)



