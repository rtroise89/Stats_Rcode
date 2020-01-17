# ANOVA Two-factor Nested factorial Design, SxA(B)
# Faces in Space

# Factor B is Typicality. Factor A(B) is Faces nested in
# Typicality. There are 4 subjects in the experiment.

# Therefore there are 4 groups with 5 observations (subjects)
# per group.

# We collect the data for each subjects for all levels of
# Factor A and Factor B for each subject.
a_b1=c(20,22,25,24,19, 9,8,21,21,21, 18,20,18,21,33,
       5,14,16,22,23)
a_b2=c(37,37,43,48,45, 34,35,35,37,39, 35,39,39,37,40,
       38,49,51,50,52)

# We now combine the observations into one long column (score).
score=c(a_b1,a_b2)

# We now prepare the labels for the 4x5x2 scores according to
# the factor levels:
# a1 a2 a3 a4 a5, a1 a2 a3 a4 a5........etc for Factor A
  Face=gl(5,1,5*4*2, labels=c("a1","a2","a3","a4","a5"))

# b1 b2, b1 b2....... etc for Factor B
Typicality=gl(2,4*5*1,5*4*2,labels=c("Atypical","Typical"))

# sub_1 sub_1......., sub_2 sub_2......,sub_3 sub_3 ....,sub_4
# sub_4 ....., sub_5 sub_5.....etc for Subjects
Subject=gl(4, 5*1, 5*4*2, labels = c("sub_1", "sub_2", "sub_3",
                                     "sub_4"))

# We now form a data frame with the dependent variable and the # factors.
data = data.frame(score = score, Face = factor(Face),
                  Typicality = factor(Typicality))

# Anova when "Subject" is considered as a random factor.
aov1 = aov(score ~ (Subject + Face%in%Typicality + Typicality +
                      Typicality:Subject))
Df = summary(aov(score ~ (Subject + Face%in%Typicality +
                            Typicality + Typicality:Subject)))[[1]]$Df
Sum_Sq = summary(aov(score ~ (Subject + Face%in%Typicality +
                                Typicality + Typicality:Subject)))[[1]]$Sum
MS = summary(aov(score ~ (Subject + Face%in%Typicality +
                            Typicality + Typicality:Subject)))[[1]]$Mean
F = summary(aov(score ~ (Subject + Face%in%Typicality +
                           Typicality + Typicality:Subject)))[[1]]$F
F[2]=NA
Pr = summary(aov(score ~ (Subject + Face%in%Typicality +
                            Typicality + Typicality:Subject)))[[1]]$Pr
Pr[2]=NA
Source_names = c("Subject", "Typicality", "Face(Typicality)",
                 "Subject * Typicality", "Error:Face * Subject(Typicality)")

Anova_table = data.frame(Names = Source_names, Df=Df, Sum.Sq=Sum_Sq, Mean.Sq=MS,
                        F.Value=F, "Pr>F" = Pr)

# We now print the data and all the results
print(data)
print(Anova_table)
print("The 'Typicality' factor has a Quasi F or F'. This F' has
not been displayed in the Anova table and has to be
calculated separately")
print(model.tables(aov1,"means"),digits=3)