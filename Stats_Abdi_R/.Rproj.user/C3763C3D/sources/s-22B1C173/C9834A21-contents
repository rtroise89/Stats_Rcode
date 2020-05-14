# ANOVA Two-factor within subjects, SxA
# Plungin' - Deep Sea Diving Example

# We have 2 Factors, A (Learning), with 2 levels and Factor
# B(Testing) with 2 levels and 5 subjects.
# The 2 levels of Factor A and B are: On Land and Underwater.
# Therefore there are 4 groups with the same 5 observations
#(subjects) per group.

# We collect the data for each subject for all levels of Factor
# A and Factor B for each subject.
b1=c(34,37,27,43,44, 14,21,31,27,32)
b2=c(18,21,25,37,34, 22,25,33,33,42)

# We now combine the observations into one long column (score).
score=c(b1,b2)

# We now prepare the labels for the 4x5 scores according to the
# factor levels:
# a1 a2, a1 a2........etc for Factor A
Learning=gl(2,5*1,5*4*1, labels=c("a1","a2"))

# b1 b2, b1 b2....... etc for Factor B
Testing=gl(2,2*5*1,5*4*1,labels=c("b1","b2"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5.....etc for Subjects
Subject=gl(5,1,5*4*1, labels=c("sub_1", "sub_2", "sub_3",
                               "sub_4", "sub_5"))

# We now form a data frame with the dependent variable and the
# factors.
data = data.frame(score = score, Learning = factor(Learning), Testing = factor(Testing), Subject = factor(Subject))

# We now perform an anova when "Subject" is considered as a random factor.
aov1 = aov(score ~ (Learning * Testing) + Error(Subject /
                                                  (Learning * Testing)), data = data)
# We now print the data and all the results
print(summary(aov(score~Learning*Testing*Subject)))
print(summary(aov1))
print(model.tables(aov(score ~ Learning * Testing * Subject,
                       data = data), "means"), digits = 3)