v1 <- c(3,8,4,5,0,11)
v2 <- c(4,11,0,8,1,3)

add.result <- v1 + v2
print(add.result)

# Vector subtraction. 
sub.result <- v1-v2 
print(sub.result)

# Vector multiplication.
multi.result <- v1*v2 
print(multi.result)

# Vector division.
divi.result <- v1/v2 
print(divi.result)

sort.result <- sort(v1) 
print(sort.result)

revsort.result <- sort(v1, decreasing = TRUE) 
print(revsort.result)

# Create two 2x3 matrices.
matrix1 <- matrix(c(3, 9, -1, 4, 2, 6), nrow = 2) 
print(matrix1)
matrix2 <- matrix(c(5, 2, 0, 9, 3, 4), nrow = 2) 
print(matrix2)

# Add the matrices.
result <- matrix1 + matrix2 
cat("Result of addition","\n") 
print(result)

# Subtract the matrices
result <- matrix1 - matrix2
cat("Result of subtraction","\n") 
print(result)

# Multiply the matrices. 
result <- matrix1 * matrix2
cat("Result of multiplication","\n") 
print(result)

# Divide the matrices 
result <- matrix1 / matrix2
cat("Result of division","\n") 
print(result)

# Create two vectors of different lengths. 
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2)) 
print(result)

# Create two vectors of different lengths. 
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
array1 <- array(c(vector1,vector2),dim = c(3,3,2))

# Create two vectors of different lengths. 
vector3 <- c(9,1,0)
vector4 <- c(6,0,11,3,14,1,2,6,9)
array2 <- array(c(vector1,vector2),dim = c(3,3,2))

# create matrices from these arrays. 
matrix1 <- array1[,,2]
matrix2 <- array2[,,2]

# Add the matrices.
result <- matrix1+matrix2 
print(result)

# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
new.array <- array(c(vector1,vector2),dim = c(3,3,2)) 
print(new.array)

# Use apply to calculate the sum of the rows across all the matrices.
result <- apply(new.array, c(1), sum) 
print(result)

# Create the data frame.
emp.data <- data.frame(
emp_id = c (1:5),
emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
salary = c(623.3,515.2,611.0,729.0,843.25),
start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11", "2015-03-27")),
stringsAsFactors = FALSE
)

# Print the summary.
print(summary(emp.data))

# Create two lists.
list1 <- list(1,2,3)
list2 <- list("Sun","Mon","Tue")

# Merge the two lists. 
merged.list <- c(list1,list2)
print(merged.list)

#importing of data in R programming
data <- read.csv("C:/Users/visha/Desktop/ml/1csv.csv")
print(data)

print(is.data.frame(data))
print(ncol(data)) 
print(nrow(data))

# Create a data frame.
retval <- subset(data, as.Date(start_date) > as.Date("2014-01- 01"))

# Write filtered data into a new file. 
write.csv(retval,"output.csv") 
newdata <- read.csv("output.csv") 
print(newdata)

#loop 
I<-1
While(i<=10){
 If(i==5){
  I<-i+1
  Next
 }
Print(i)
I<-i+1
}

for (i in 1: 4)
{
 print(i ^ 2)
}

# R program to illustrate while loop 
result <- c("Hello World")
i <- 1
# test expression 
while (i < 6) {
 print(result)
 # update expression 
 i = i + 1
}
# whose factorial will be calculated 
n < - 5
# assigning the factorial variable 
# and iteration variable to 1 
factorial < - 1
i < - 1
# using while loop 
while (i <= n)
{
factorial = factorial * i
 i = i + 1
}
# displaying the factorial 
print(factorial)

#Vectorization
v <- c(1, 2, 3, 4, 5)
v <- v * 2

#Missing Values
#is.na function using check the NA missing value 
#na.omit function using remove the NA missing value 
print(is.na(v))
v <- na.omit(v)
v<-c(1,2,3,NA,5)

#Data Manipulations
#Summary
summary(df)
#Sorting
df_sorted <- df[order(df$Age), ]
#Subsetting
df_subset <- subset(df, Age > 30)
#Merging and Joining
df_merged <- merge(df, df2, by = "Name")
#left_join
library(dplyr)
df_joined <- left_join(df, df2, by = "Name")