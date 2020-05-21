# ANOVA Two-factor Partially Repeated Measures, S(A)xB
# Bat and Hat Example

# We have 2 Factors, A (Age), with 2 levels and Factor B
# (Phonological Similarity) with 2 levels and 10 subjects.

# The 2 levels of Factor A are: Five Years and Twelve Years
# The 2 levels of Factor B are: Similar and Dissimilar

# The Subjects are nested in Age and crossed with Phonological
# Similarity.
# Therefore there are 4 groups with 5 observations (subjects)
# per group.

# We collect the data for each subjects for all levels of
# Factor A and Factor B for each subject.
b1=c(15,23,12,16,14, 39,31,40,32,38)
b2=c(13,19,10,16,12, 29,15,30,26,30)

# We now combine the observations into one long column (score).
score=c(b1,b2)

# We now prepare the labels for the 4x5 scores according to the
# factor levels:
# a1 a2, a1 a2........etc for Factor A
Age=gl(2,5*1,5*4*1, labels=c("a1","a2"))

# b1 b2, b1 b2....... etc for Factor B
Phono_Sim=gl(2,2*5*1,5*4*1,labels=c("b1","b2"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5.....etc for Subjects

Subject=gl(10,1,5*4*1, labels = c("sub_1", "sub_2", "sub_3",
                                  "sub_4", "sub_5", "sub_6", "sub_7", "sub_8", "sub_9",
                                  "sub_10"))

# We now form a data frame with the dependent variable and the
# factors.
data = data.frame(score = score, Age = factor(Age), Phono_Sim =
                    factor(Phono_Sim), Subject=factor(Subject))

# Anova when "Subject" is considered as a random factor.
aov1 = aov(score ~ (Age * Phono_Sim) + Error(Subject / (Age *
                                                          Phono_Sim) + Age), data=data)
# We now print the data and all the results
print(data)
print(summary(aov1))
print(model.tables(aov(score ~ Age * Phono_Sim * Subject, data
                       = data), "means"), digits = 3)