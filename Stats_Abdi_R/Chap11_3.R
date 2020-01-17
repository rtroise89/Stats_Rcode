# ANOVA One-factor within subjects, SxA
# Proactive Interference

# We have 1 Factor, A (Rank), with 6 levels in Factor A and 8
# subjects.
# The 6 levels of Factor A are: rank_1, rank_2, rank_3, rank_4,
# rank_5, rank_6
# Therefore there are 6 groups with the same 8 observations
# (subjects) per group.

# We collect the data for each subjects for all levels of
# Factor A.
sub_1=c(17,13,12,12,11,11)
sub_2=c(14,18,13,18,11,12)
sub_3=c(17,16,13,11,15,14)
sub_4=c(18,16,11,10,12,10)
sub_5=c(17,12,13,10,11,13)
sub_6=c(16,13,13,11,11,11)
sub_7=c(14,12,10,10,10,10)
sub_8=c(16,17,15,11,13,11)

# We now combine the observations into one long column (score).
score=c(sub_1,sub_2,sub_3,sub_4,sub_5,sub_6,sub_7,sub_8)

# We now prepare the labels for the 6x8 scores according to the
# factor levels:
# rank_1 rank_2 rank_3 rank_4 rank_5 rank_6........etc for
# Factor A
Rank=gl(6,1,8*6*1, labels = c("rank_1", "rank_2", "rank_3", "rank4", "rank_5", "rank_6"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5.....etc for Subjects
Subject=gl(8,6*1,8*6*1, labels=c("sub _1", "sub_2", "sub_3",
                                 "sub_4", "sub_5", "sub_6", "sub_7", "sub_8"))

# We now form a data frame with the dependent variable and the
# factors.
data = data.frame(score = score, Rank = factor(Rank), Subject =
                    factor(Subject))

# Anova when "Subject" is considered as a random factor.
aov1=aov(score~Rank+Error(Subject),data=data)

# We now print the data and all the results
print(data)
print(summary(aov1))
print(model.tables(aov(score ~ Rank + Subject, data =
                         data),"means"),digits=3)