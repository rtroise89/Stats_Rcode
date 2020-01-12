# Regression Approach: ANOVA One-factor between subjects, S(A)
# Imagery and Memory

# We have 1 Factor, A, with 2 levels: Experimental Group and
# Control Group.

# We have 5 subjects per group. Therefore 5 x 2 = 10 subjects total.
# We collect the data for each level of Factor A
Expt=c(1,2,5,6,6)
Control=c(8,8,9,11,14)

# We now combine the observations into one long column (score).
score=c(Expt,Control)

# We generate a second column (group), that identifies the group
# for each score.
levels=factor(c(rep("Expt",5),rep("Control",5)))

# We now use the means of the respective groups as the predictors
Predictors=c(rep(mean(Expt),5),rep(mean(Control),5))

# We now form a data frame for the Regression approach
data_reg=data.frame(Predictors,score)
r=cor(Predictors,score)

# Now we perform the regression analysis on the data
reg1=lm(score~Predictors)

# We now form a data frame with the dependent variable and the
# factors for the ANOVA.
data=data.frame(score=score,group=levels)

# We now perform an ANOVA
aov1=aov(score~levels)

# We now print the data and all the results
print(data_reg)
print(model.tables(aov(score~levels),"means"),digits=3)
print(r)
print(summary(reg1))
print(summary(aov1))