# ANOVA One-factor between subjects, S(A)
# Face Perception

# We have 1 Factor, A, with 5 levels: Group 1, Group 2, Group 3,
# Group 4, Group 5

# We have 8 subjects per group. Therefore 5 x 8 = 40 subjects total.
# We collect the data for each level of Factor A

G_1=c(40,44,45,46,39,46,42,42)
G_2=c(53,46,50,45,55,52,50,49)
G_3=c(46,45,48,48,51,45,44,49)
G_4=c(52,50,53,49,47,53,55,49)
G_5=c(52,49,49,45,52,45,52,48)

# We now combine the observations into one long column (score).
score=c(G_1,G_2,G_3,G_4,G_5)

# We generate a second column (levels), that identifies the group for each score.
levels=factor(c(rep("G_1",8),rep("G_2",8),rep("G_3",8),
                rep("G_4",8),rep("G_5",8)))

# We now form a data frame with the dependent variable and
# the factors.
data=data.frame(score=score,group=levels)

# We now generate the ANOVA table based on the linear model
aov1=aov(score~levels)

# We now print the data and all the results
print(data)
print(model.tables(aov(score~levels),"means"),digits=3)
print(summary(aov1))