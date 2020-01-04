X = c(1,1,1,1,1,3,3,3,3,3,5,5,5,5,5,7,7,7,7,7)
Y = c(433,435,434,441,457,519,511,513,520,537,598,584,606,605,607, 666,674,683,685,692)
data = data.frame(X,Y)

# We now get a summary of simple statistics for the data
Mean = colMeans(data)
Std_Dev = apply(data, MARGIN=2, sd)
r = cor(X,Y)

# We now plot the points and SAVE it as a PDF
# Make sure to add the PATH to the location where the plot is to be saved
pdf('C:/Users/RichardT/Documents/Statistics/Stats_Rcode/Chap2_output.pdf')
plot(X,Y,main="Plot of Memory Set (X) vs Reaction Time (Y)")
dev.off()

# We now perform a regression analysis on the data
reg1=lm(Y~X)

# We now perform an ANOVA on the data
aov1=aov(Y~X)

# We now print the data and all the results
print(data)
print(Mean)
print(Std_Dev)
print(r)
summary(reg1)
print(summary(aov1))