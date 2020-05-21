# ANOVA One-factor between subjects, S(A)
# Another example: Images...

# We have 1 Factor, A, with 3 levels: Built Image, Given
# Image and Control.

# We have 5 subjects per group. Therefore 5 x 3 = 15 subjects total.
# We collect the data for each level of Factor A

Built=c(22,17,24,23,24)
Given=c(13,9,14,18,21)
Control=c(9,7,10,13,16)

# We now combine the observations into one long column (score).
score=c(Built,Given,Control)

# We generate a second column (group), that identifies the group
# for each score.
levels=factor(c(rep("Built",5),rep("Given",5),rep("Control",5)))

# We now form a data frame with the dependent variable and
# the factors.
data=data.frame(score=score,group=levels)

# We now generate the ANOVA table based on the linear model
aov1=aov(score~levels)

# We now print the data and all the results
print(data)
print(model.tables(aov(score~levels),"means"),digits=3)
print(summary(aov1))