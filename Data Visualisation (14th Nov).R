# Q1.Import the LungCapData into R and attach it
a=read.csv("C:/Users/admin/Desktop/data.csv",header=TRUE)
a

# Q2. Find the class and typeof Age and Height
typeof(a$Age)
class(a$Age)
typeof(a$Height)
class(a$Height)

# Q3. Find the summary of the dataset
summary(a)

# Q4.Find the strength of the relationship between Age and Height (Hint:Using Correlation)
corr = cor.test(a$Age,a$Height, method = "pearson")
corr

# Q5.Perform a scatterplot for the above (Qn.4)
plot(a$Age,a$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height",pch=19)

# Q6. Add a title to the plot and Label x-axis and y-axis
x=plot(a$Age,a$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height",pch=16)

# Q7.Rotate the values on the y-axis and also change the limits of x-axis and y-axis
Y=plot(a$Age,a$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height",pch=16,las=1)

Age=a$Age
Height=a$Height
plot(Age,Height,xlim=c(0,20),ylim=c(0,90))

# Q8. Remove the axes of the plot and relabel these axes
plot(a$Age,a$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height",pch=16,las=1,ann = FALSE)
plot(a$Age,a$Height,main="Age - Height Correlation",xlab="Age (yrs)",ylab="Height (cms)",pch=16,las=1)


# Q9.Change the size of the plotting characters
plot(a$Age,a$Height,main="Age - Height Correlation",xlab="Age (yrs)",ylab="Height (cms)",pch=16,las=1,cex.main=2)

# Q10.Change the color of the characters to red
plot(a$Age,a$Height,main="Age - Height Correlation",xlab="Age (yrs)",ylab="Height (cms)",pch=16,las=1,cex.main=2,col.main="red", col.lab="red")

# Q11.Draw the regression line to the plot predicting height using age

lm(Height ~ Age)

z=plot(Age,Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height",pch=16,las=1,cex=2,col="green")
abline(lm(Height ~ Age))

# Q12.Change the color of the line to blue and the width of the line
plot(x = a$Age, y = a$Height,
     xlab = "Age",
     ylab = "Height",
     ylim = c(45, 75),
     xlim = c(6, 20),
     las = 1,
     axes = FALSE,
     cex = 3,
     col = "yellow",
     abline(lm(a$Height~a$Age),col = "blue", lwd=5),
     main = "Scatter plot for LungCapData")


# Q13.Change the font of the plotting characters
plot(x = Age, y = Height,
     xlab = "Age",
     ylab = "Height",
     ylim = c(45, 75),
     xlim = c(6, 20),
     las = 1,
     axes = FALSE,
     pch = 3,
     cex = 2,
     col = "yellow",
     abline(lm(Height~Age),col = "black", lwd=5),
     main = "Scatter plot for LungCapData")


# Q14.Change the color of the plotting characters
z=plot(Age,Height,main="Relationship bw Age and Age",xlab="Age",ylab="Height",pch=4,las=2,cex=10,col="blue",col.main="green",col.lab="red")
abline(lm(Height ~ Age),lwd= 3)

# Q15.Identify gender on the plot for the age male and female
library(dplyr)
df=tbl_df(a)
par(mfrow=c(1,2))
n = filter(df,Gender=="male")
plot(n$Age,n$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height")
m=filter(df,Gender=="female")
plot(m$Age,m$Height,main="Relationship bw Age and Height",xlab="Age",ylab="Height")

