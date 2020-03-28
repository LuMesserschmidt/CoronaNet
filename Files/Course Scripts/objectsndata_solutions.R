# Objects in R: Basic Calculations ----

#' In order to assign the value of 5 to an object with name x do 

x <- 5
# Which corresponds to
x = 5


#' Assign the value of 10 to the object x
x <- 10

#' Multiply x with 5 and save it as b
b <- x * 5


#' Print b by simply typing its name in the script**
b


#' If we want to delete certain objects we can do so with the function 
#' rm() (remove). rm(list=ls()) removes all objects currently saved in the list 
#' environment.  
rm(x) # Deletes an object
rm(list=ls()) # all objects are removed


# Everything in R Is an Object ----
#' Try to understand the different meanigs of x!

# Basic object creation
x <- 1
x

# Not working why
x(2)

# Making it work
x <- function(i) i^2

x

x(2)

# Vectors ----

# Examples
a <- c(1, 2, 3, 4)

#' Do you remember a different (easier) way to build the vector a?
a <- c(1:5)

#' What happens if we add 1 to a?
b <- a + 1
b

#' R is object-oriented programming --> accordingly, it follows standard math rules.
c <- sqrt(a + b * 2)
c


#' Important R functions for vectors:
#' Do you understand all of these functions?
 
# Important R functions for vectors:
# Basic functions:
sort(a)
length(a)
min(a)
max(a)
sum(a)
prod(a)

# Creating special vectors:
rep(1, 20)
seq(50)
5:15
seq(4, 20, 2)

## Logical Operators and Logical Vectors ----
#' Do you understand all of these operators?

x <- 10
y <- 9

x == y # x is equal to y
x > y  # x is bigger then y
x <=  y # HOW DO YOU TYPE: x is smaller or equal to y
x != y # x is NOT equal to y


#' The contents of R vectors do not need to be numeric. 
#' A simple example of a different type are *character* vectors. 
#' For handling them, the contents simply need to be
#' enclosed in quotation marks:
cities = c("Friedrichshafen", "Paris", "Tokio", "Tettnang", "Mailand")
cities


#' Another useful type are **logical vectors**. 
#' Each element can only take one of two values: "TRUE" or "FALSE". 
#' Internally, "FALSE" corresponds to 0 and "TRUE" to 1. 

a <- TRUE; b <- FALSE # note the functionality of the semicolon

a | b  # Either a or b is TRUE
a & b  # Both a and b are TRUE

a <- c(7, 5, 2, 6, 9, 4, 1, 3)
b <- a < 3 | a >= 6
b


#' **Summary**: Vectors can have three different classes: "numeric", "logical", "character":

x <- c(1050, 35, 2.3, 2 ^ 2, -10, sqrt(81)) # numeric vector
y <- c(TRUE, FALSE, TRUE, FALSE) # logical vector
z <- c("zero", "four", "nine") # character vector


#' One minor (seldomly important) addendum: Within numeric, 
#' there is a difference between 1 and 1.0. The former is a pure integer, 
#' the latter is an object called double.

# create a string of double-precision values
dbl_var <- c(1, 2.5, 4.5)  
typeof(dbl_var)

# placing an L after the values creates a string of integers
int_var <- c(1L, 6L, 10L)
typeof(int_var)

## Naming and Indexing Vectors ----

#' Complete the empty spaces!

# Create a vector "kicker.skills":
kicker.skills <- c(0.2, 4, 3, 10, 5,-2)

# Create a string vector of names:
players <- c("Katja", "Hans", "Theresa", "Sabine", "Katrin", "Marco")

# Assign names to vector and display vector:
names(kicker.skills) <- players
kicker.skills

# Indices by number:
kicker.skills[2] # GIVE second entry
kicker.skills[2:4] # SELECT 2nd to 4th entry

# Indices by name:
kicker.skills["Marco"] # WHAT is the level of Marco's kicker skills?

# Logical indices:
# SUBSET for an all-star team, only kicker players with skill level above 2
kicker.skills[kicker.skills >2]

# Who is the worst kicker player?
kicker.skills[kicker.skills == min(kicker.skills)] # Do you understand every part of this line?


# Data Frames ----

# Define one x vector for the days:
days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# When are Eugen, Julia, and Lena in the office?
Eugen <-
  c(0, 0, 1, 1, 0)
Julia <- c(1, 1, 0, 0, 1)
Lena <- c(0, 0, 0, 1, 1)

presence <- data.frame(days, Eugen, Julia, Lena)

#' To index certain entries in a data frame use `[]` behind the
#'  name of a data frame object. 
#'  Specifically: `data.frame.name[ROWnumber(s) , COLUMN-number/-name(s)]`. 
#' Complete the empty spaces!

# Show the entry in the 4th row and the second column of presence
presence[4, 2]

# Show all entries in the third row 
presence[3,]

# Show all entries of the third column
presence[,3]

# Show all entries of the variable Julia
presence[ , "Julia"]

# Show all entries of the variables days, Julia and Lena
presence[ ,c("days", "Julia", "Lena")]


#' Complete the empty spaces!

# Accessing a single variable:
presence$Eugen

# GENERATE a new variable total.presence which sums up the total of people who 
# are present on each day:
presence$total.presence <- presence$Eugen + presence$Julia + presence$Lena
presence

# The same but using "with":
presence$total.presence2 <- with(presence, Eugen+Julia+Lena)


#' 
#' Sometimes, we do not want to work with a whole data set but only 
#' with a subset. This can be easily achieved with the command 
#' subset(df, criterion), where criterion is a logical expression which evaluates 
#' to TRUE for the rows which are to be selected. 

#' Complete the empty spaces!

# Subset: all days in which Lena is present
subset(presence, Lena==1)




# Other ----

## Factors ----

#' What is the difference betwenn xf1 and xf2
x <- c(3, 2, 2, 3, 1, 2, 3, 2, 1, 2)

xf1 <- factor(x, labels = c("bad", "okay", "good"), ordered = TRUE)
xf2 <- factor(x, labels = c("bad", "okay", "good"))

#' Practice more

# variable gender with 20 "male" entries and 30 "female" entries
gender <- c(rep("male", 20 ), rep("female", 30)) # COMPLETE MISSING PARTS
gender <- factor(gender, levels= c("male", "female"))    # CREATE gender as factor
            
            
# R now treats gender as a nominal variable
summary(gender) 



## Matrices ----

#' Complete the empty spaces
v <- c("AG.5", "G2", "AG.5", "G1", "AG.5", "G3")
A <- matrix(v, nrow = 2)

# GENERATE matrix A.row in binding vec1 and vec2 AND generate matrix A.col in binding column wise:
vec1 <- c("AG.5", "AG.5", "AG.5")
vec2 <- c("G2", "G1", "G3")

A.row <- rbind(vec1, vec2) 
A.col <- cbind(vec1, vec2)

# Giving names to rows and columns:
colnames(A.col) <- c("Katrin", "Hans")
rownames(A.col) <- c("Monday", "Wednesday", "Friday")
A.col

#' Core rule for indexing all two dimensional objects in R:  [row , column]
#' Complete the empty spaces

# EXTRACT value of first column and 2nd row 
A.col[2, 1]

# GIVE all meal choices from Hans:
A.col[ , "Hans"]

# WHAT does Katrin have for lunch on Monday? call Katrin and Monday by name
A.col["Wednesday" , "Katrin"]


# WHAT does Katrin have on Monday and Friday?
A.col[c("Monday", "Friday") , "Katrin"]


#' Some quick matrix algebra

# Element wise multiplication (not matrix multiplication but multiplying elements at same place)
A <- matrix(c(2, -4, -1, 5, 7, 0), nrow = 2)
B <- matrix(c(2, 1, 0, 3, -1, 5), nrow = 2)
A * B

# Transpose:
(C <- t(B))

# Matrix multiplication:
(D <- A %*% C)

# Inverse:
solve(D)

## Lists ----

#' A list is a generic collection of objects. 
#' Unlike vectors, components can be of different types.

# Generate a list object:
mylist <- list(A = seq(8, 36, 4),
               product = "Fruechtekorb",
               idm = diag(3))

# Print whole list:
mylist

# Vector of names:
names(mylist)

# Print component "A":
mylist$A
