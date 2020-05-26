# Regression Example: Retroactive Interference# NOTE: Install and load package "ppcor" in order to use the "spcor" function.
# We first arrange the data into the Predictors (X and T) and
# Regressor (Y)# In this example the predictors are Number of Learning Trials (X)
# and Number of interpolated lists (T)

X=c(2,2,2,4,4,4,8,8,8,2,2,2,4,4,4,8,8,8)
T=c(2,4,8,2,4,8,2,4,8,2,4,8,2,4,8,2,4,8)

# The Regressors are the number of words correctly recalled (Y)
Y=c(35,21,6,40,34,18,61,58,46,39,31,8,52,42,26,73,66,52)

# Create data frame
data=data.frame(X,T,Y)

Mean=colMeans(data)
Std_Dev = apply(data, MARGIN=2, sd)

# We now perform an orthogonal multiple regression analysis on the data
multi_reg1=lm(Y~X+T,data=data)

# We now compute the predicted values and the residuals
Y_hat=predict(lm(Y~X+T))
Residual=round(residuals(multi_reg1),2)

# We now compute the sum of squares of the residuals
SS_residual=sum(Residual^2)

# We now compute the correlation matrix between the variables
r_mat=cor(data)
Corr=round(r_mat,4)

library(ppcor)
# We now compute the semi-partial coefficients and create a plot
# Make sure to add the PATH to the location where the plot is to be saved
pdf('C:/Users/RichardT/Documents/Statistics/Stats_Rcode/Chap3_output.pdf')
semi_r = spcor(data)

# Plotting the semi-partial correlations
barplot(semi_r$estimate, beside=TRUE, horiz=TRUE, 
        col=c("red","black","blue"),
        xlab="Semi-Partial Person r", xlim=c(-1,1))
legend("bottomleft", legend=c("X","T","Y"), fill=c("red","black","blue"))

semi_part=data.frame(X=semi_r$estimate[3,1]^2, T=semi_r$estimate[3,2]^2)
dev.off()

print(Mean)
print(Std_Dev)
print(data.frame(Y,Y_hat,Residual))
print(Corr)
print(semi_part)
print(SS_residual)
print(summary(multi_reg1))
print(anova(multi_reg1))
