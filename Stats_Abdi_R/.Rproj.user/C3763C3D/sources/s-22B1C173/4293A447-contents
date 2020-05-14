# ANOVA Two-factor between subjects, S(AxB)
# Cute Cued Recall; Tulving and Pearlstone

# We have 2 Factors, A & B, with 3 levels in Factor A and 2 levels
# in Factor B. Therefore there are 3 x 2 = 6 groups with 10
# observations (subjects) per group.

# We collect the data for each level of Factor B across all the
# levels of Factor A.

# Factor A: Length of Word List
# (a1=12 words, a2=24 words, a3=48 words)
# Factor B: Type of Recall
# (b1=Free Recall, b2= Cued Recall)
library(gmodels)

free_recall=c(11,13,17, 9,18,20, 13,19,22, 9,13,13, 8,8,21,
              7,15,16, 12,13,23, 11,9,19, 10,8,20, 10,14,19)
cued_recall=c(12,13,32, 12,21,31, 7,20,27, 9,15,30, 9,17,29,
              10,14,30, 12,13,33, 10,14,25, 7,16,25, 12,7,28)

# We now combine the observations into one long column (score).
score=c(free_recall,cued_recall)

# We now prepare the labels for the 3 x 2 x 10 scores according to
# the factor levels:
# Factor A --- 12 words 24 words 48 words, 12 words 24 words
# 48 words, ... etc.
list_length=gl(3,1,2*3*10, labels=c("12 Words","24 Words","48
                                    Words"))

# Factor B --- Free Recall Free Recall , Cued Recall Cued
# Recall etc.
recall_type=gl(2,3*10,2*3*10, labels=c("Free Recall","Cued
                                       Recall"))

# We generate a second column (group), that identifies the group
# for each score.
group=gl(2*3,10,2*3*10, labels=c("a1b1", "a2b1", "a3b1", "a1b2",
                                 "a2b2", "a3b2"))

# We now form a data frame with the dependent variable and the factors.
data=data.frame(score = score, Factor_A = factor(list_length),
                Factor_B = factor(recall_type), Group = group)

# We now define the contrasts
Linear=c(-1,0,1)
Quadratic=c(1,-2,1)
a1_vs_a2_a3=c(-2,1,1)
a2_vs_a3=c(0,1,-1)
AB_contrast=c(-2,2,1,-1,1,-1)

# We now perform the ANOVA on the data.
aov1=aov(score~list_length*recall_type, data=data)
interaction=list_length:recall_type

# We now organize the results
Df_Linear=summary(aov(score~list_length+recall_type+interaction,
                      contrasts=list(list_length=make.contrasts(Linear))),split =
                    list(list_length = list("Linear" = 1)))[[1]]$Df
Df_Quadratic=summary(aov(score~list_length+recall_type+
                           interaction,contrasts= list(list_length= make.contrasts(
                             Quadratic))), split = list(list_length = list("Quadratic" =
                                                                             1)))[[1]]$Df
Df_a1_vs_a2_a3=summary(aov(score~list_length+recall_type+
                             interaction, contrasts=list(list_length = make.contrasts(
                               a1_vs_a2_a3))),split = list(list_length = list("a1_vs_a2_a3" =
                                                                                1)))[[1]]$Df
Df_a2_vs_a3=summary(aov(score~list_length+recall_type+
                          interaction,contrasts = list(list_length = make.contrasts(
                            a2_vs_a3))), split = list(list_length = list("a2_vs_a3" =
                                                                           1)))[[1]]$Df
Df_AB_contrast=summary(aov(score~list_length+recall_type+
                             interaction,contrasts = list(interaction = make.contrasts(
                               AB_contrast))),split = list(interaction = list("AB_contrast" =
                                                                                1)))[[1]]$Df
Df_Cont = data.frame(rbind(Df_Linear, Df_Quadratic,
                           Df_a1_vs_a2_a3, Df_a2_vs_a3, Df_AB_contrast))
SS_Linear=summary(aov(score~list_length+recall_type+interaction,
                      contrasts=list(list_length=make.contrasts(Linear))),split =
                    list(list_length = list("Linear" = 1)))[[1]]$Sum
SS_Quadratic=summary(aov(score~list_length+recall_type+
                           interaction, contrasts=list(list_length =
                                                         make.contrasts(Quadratic))), split = list(list_length =
                                                                                                     list("Quadratic" = 1)))[[1]]$Sum
SS_a1_vs_a2_a3=summary(aov(score~list_length+recall_type+
                             interaction,contrasts = list(list_length =
                                                            make.contrasts(a1_vs_a2_a3))), split = list(list_length =
                                                                                                          list("a1_vs_a2_a3" = 1)))[[1]]$Sum
SS_a2_vs_a3 = summary(aov(score~list_length + recall_type +
                            interaction,contrasts = list(list_length =
                                                           make.contrasts(a2_vs_a3))), split = list(list_length =
                                                                                                      list("a2_vs_a3" = 1)))[[1]]$Sum
SS_AB_contrast = summary(aov(score~list_length + recall_type +
                               interaction, contrasts = list(interaction =
                                                               make.contrasts(AB_contrast))), split = list(interaction =
                                                                                                             list("AB_contrast" = 1)))[[1]]$Sum
SS_Cont = data.frame(rbind(SS_Linear, SS_Quadratic,
                           SS_a1_vs_a2_a3, SS_a2_vs_a3, SS_AB_contrast))
MS_Linear = summary(aov(score~list_length + recall_type +
                          interaction, contrasts = list(list_length =
                                                          make.contrasts(Linear))), split = list(list_length =
                                                                                                   list("Linear" = 1)))[[1]]$Mean
MS_Quadratic = summary(aov(score~list_length + recall_type +
                             interaction, contrasts = list(list_length =
                                                             make.contrasts(Quadratic))), split = list(list_length =
                                                                                                         list("Quadratic" = 1)))[[1]]$Mean
MS_a1_vs_a2_a3 = summary(aov(score~list_length + recall_type +
                               interaction, contrasts = list(list_length =
                                                               make.contrasts(a1_vs_a2_a3))), split = list(list_length =
                                                                                                             list("a1_vs_a2_a3" = 1)))[[1]]$Mean
MS_a2_vs_a3 = summary(aov(score~list_length + recall_type +
                            interaction, contrasts = list(list_length =
                                                            make.contrasts(a2_vs_a3))), split = list(list_length =
                                                                                                       list("a2_vs_a3" = 1)))[[1]]$Mean
MS_AB_contrast = summary(aov(score~list_length + recall_type +
                               interaction, contrasts = list(interaction =
                                                               make.contrasts(AB_contrast))),split = list(interaction =
                                                                                                            list("AB_contrast" = 1)))[[1]]$Mean
MS_Cont=data.frame(rbind(MS_Linear, MS_Quadratic, MS_a1_vs_a2_a3,
                         MS_a2_vs_a3, MS_AB_contrast))
F_Linear = summary(aov(score~list_length + recall_type +
                         interaction, contrasts = list(list_length =
                                                         make.contrasts(Linear))), split = list(list_length =
                                                                                                  list("Linear" = 1)))[[1]]$F
F_Quadratic = summary(aov(score~list_length + recall_type +
                            interaction, contrasts = list(list_length =
                                                            make.contrasts(Quadratic))), split = list(list_length =
                                                                                                        list("Quadratic" = 1)))[[1]]$F
F_a1_vs_a2_a3 = summary(aov(score~list_length + recall_type +
                              interaction, contrasts = list(list_length =
                                                              make.contrasts(a1_vs_a2_a3))), split = list(list_length =
                                                                                                            list("a1_vs_a2_a3" = 1)))[[1]]$F
F_a2_vs_a3 = summary(aov(score~list_length+recall_type +
                           interaction,contrasts = list(list_length =
                                                          make.contrasts(a2_vs_a3))), split = list(list_length =
                                                                                                     list("a2_vs_a3" = 1)))[[1]]$F
F_AB_contrast = summary(aov(score~list_length + recall_type +
                              interaction, contrasts = list(interaction =
                                                              make.contrasts(AB_contrast))), split = list(interaction =
                                                                                                            list("AB_contrast" = 1)))[[1]]$F
F_Cont = data.frame(rbind(F_Linear, F_Quadratic, F_a1_vs_a2_a3,
                          F_a2_vs_a3, F_AB_contrast))
Pr_Linear = summary(aov(score~list_length + recall_type +
                          interaction, contrasts = list(list_length =
                                                          make.contrasts(Linear))), split = list(list_length =
                                                                                                   list("Linear" = 1)))[[1]]$Pr
Pr_Quadratic = summary(aov(score~list_length + recall_type +
                             interaction, contrasts = list(list_length =
                                                             make.contrasts(Quadratic))), split = list(list_length =
                                                                                                         list("Quadratic" = 1)))[[1]]$Pr
Pr_a1_vs_a2_a3 = summary(aov(score~list_length + recall_type +
                               interaction, contrasts = list(list_length =
                                                               make.contrasts(a1_vs_a2_a3))), split = list(list_length =
                                                                                                             list("a1_vs_a2_a3" = 1)))[[1]]$Pr
Pr_a2_vs_a3 = summary(aov(score~list_length + recall_type +
                            interaction, contrasts = list(list_length =
                                                            make.contrasts(a2_vs_a3))), split = list(list_length =
                                                                                                       list("a2_vs_a3" = 1)))[[1]]$Pr
Pr_AB_contrast = summary(aov(score~list_length + recall_type +
                               interaction, contrasts = list(interaction =
                                                               make.contrasts(AB_contrast))),split = list(interaction =
                                                                                                            list("AB_contrast" = 1)))[[1]]$Pr
Pr_Cont = data.frame(rbind(Pr_Linear, Pr_Quadratic,
                           Pr_a1_vs_a2_a3, Pr_a2_vs_a3, Pr_AB_contrast))
Contrast_names=c("Linear", "Quadratic", "a1 vs a2 &a3", "a2 vs
                 a3","AB")
Cont_mat=rbind("Linear"=Linear,"Quadratic"=Quadratic,"a1 vs a2
               &a3"=a1_vs_a2_a3,"a2 vs a3"=a2_vs_a3,"AB"=AB_contrast)
Contrasts=data.frame(G1=Cont_mat[,1], G2 = Cont_mat[,2], G3 =
                       Cont_mat[,3], G4 = Cont_mat[,4], G5 = Cont_mat[,5], G6 =
                       Cont_mat[,6])
Contrast_Summary=data.frame(Contrast = Contrast_names, DF =
                              c(Df_Cont[1:4,2], Df_Cont[5,4]), Contrast_SS =
                              c(SS_Cont[1:4,2], SS_Cont[5,4]), Mean_Square =
                              c(MS_Cont[1:4,2], MS_Cont[5,4]), F_Value = c(F_Cont[1:4,2],
                                                                           F_Cont[5,4]), Pr=c(Pr_Cont[1:4,2], Pr_Cont[5,4]))
# Now we print the data and all the results
print(data)
print(model.tables(aov1,"means"),digits=3)
summary(aov1)
print(Contrasts)
print(Contrast_Summary)