# ANOVA One-factor within subjects, SxA
# Drugs and Reaction Time

# We have 1 Factor, A, with 3 levels in Factor A and 6
# subjects.

# The 4 levels of Factor A are: Drug A, Placebo, Drug B.
# Therefore there are 3 groups with the same 5 observations
# (subjects) per group.

# We collect the data for each subjects for all levels of
# Factor A.
sub_1=c(124,108,104)
sub_2=c(105,107,100)
sub_3=c(107,90,100)
sub_4=c(109,89,93)
sub_5=c(94,105,89)
sub_6=c(121,71,84)

#We now combine the observations into one long column (score).
score=c(sub_1,sub_2,sub_3,sub_4,sub_5,sub_6)

# We now prepare the labels for the 4x5 scores according to the factor levels:
# Drug_A Placebo Drub_B, Drug_A Placebo Drug_B........etc for
# Factor A
Drug=gl(3,1,6*3*1, labels=c("Drug_A","Placebo","Drug_B"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5....., sub_6 sub_6 etc for Factor B.
Subject=gl(6,3*1,6*3*1, labels=c("sub _1", "sub_2", "sub_3",
                                 "sub_4", "sub_5", "sub_6"))

# We now form a data frame with the dependent variable and the
# factors.
data = data.frame(score = score, Drug = factor(Drug), Subject =
                    factor(Subject))

# Anova when "Subject" is considered as a random factor.
aov1=aov(score~Drug+Error(Subject),data=data)
# We now print the data and all the results
print(data)
summary(aov1)
print(model.tables(aov(score~Drug+Subject),"means"),digits=3)