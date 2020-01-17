# ANOVA One-factor within subjects, SxA
# Numerical Example

# We have 1 Factor, A, with 4 levels in Factor A and 4
# subjects.
# The 4 levels of Factor A are: a1, a2, a3, a4.
# Therefore there are 4 groups with the same 5 observations
# (subjects) per group.

# We collect the data for each subject for all levels of
# Factor A.
sub_1=c(5,4,1,8)
sub_2=c(7,4,1,10)
sub_3=c(12,9,8,16)
sub_4=c(4,9,6,9)
sub_5=c(8,9,5,13)

# We now combine the observations into one long column (score).
score=c(sub_1,sub_2,sub_3,sub_4,sub_5)
# We now prepare the labels for the 4x5 scores according to the
# factor levels:
# a1 a2 a3 a4, a1 a2 a3 a4........etc for Factor A
Fact_A=gl(4,1,5*4*1, labels=c("a1","a2","a3","a4"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5.....etc for Subjects
Subject=gl(5,4*1,5*4*1, labels=c("sub_1", "sub_2", "sub_3",
                                 "sub_4", "sub_5"))

# We now form a data frame with the dependent variable and the factors.
data=data.frame(score = score,Factor_A = factor(Fact_A),
                Subject = factor(Subject))

# Anova when "Subject" is considered as a random factor.
aov1=aov(score~Fact_A+Error(Subject),data=data)

# We now print the data and all the results

print(data)
summary(aov1)
print(model.tables(aov(score~Fact_A+Subject),"means"))