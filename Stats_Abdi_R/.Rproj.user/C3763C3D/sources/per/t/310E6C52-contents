# ANOVA One-factor between subjects, S(A)
# Romeo and Juliet

# We have 1 Factor, A, with 4 levels: No Context, Context Before,
# Context After, Partial Context

# We have 5 subjects per group. Therefore 5 x 4 = 20 subjects total.
# We collect the data for each level of Factor A

No_cont=c(3,3,2,4,3)
Cont_before=c(5,9,8,4,9)
Cont_after=c(2,4,5,4,1)
Part_cont=c(5,4,3,5,4)

# We now combine the observations into one long column (score).
score=c(No_cont,Cont_before, Cont_after, Part_cont)

# We generate a second column (levels), that identifies the group for
# each score.
levels=factor(c(rep("No_cont",5),rep("Cont_before",5),
                rep("Cont_after",5),rep("Part_cont",5)))

# We now form a data frame with the dependent variable and the
# factors.
data=data.frame(score=score,group=levels)

# We now generate the ANOVA table based on the linear model
aov1=aov(score~levels)

# We now print the data and all the results
print(data)
print(model.tables(aov(score~levels),"means"),digits=3)
print(summary(aov1))