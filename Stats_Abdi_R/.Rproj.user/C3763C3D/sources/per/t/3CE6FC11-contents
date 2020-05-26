#ANOVA One-factor between subjects S(A)

# Romeo and Juliet: Orthogonal Multiple Regression
# Install and load package "Design" and "gregmisc".

# NOTE 1: Arrange your levels in alphabetical order, else R
# will do it for you

# We have 1 Factor, A, with 4 levels:
# Context Before,
# Partial Context,
# Context After,
# No Context

# We have 5 subjects per group. Therefore 5 x 4 = 20 subjects
# total.
library(gmodels)

# We collect the data for each level of Factor A
a1_Cont_before=c(5,9,8,4,9)
a2_Part_cont=c(5,4,3,5,4)
a3_Cont_after=c(2,4,5,4,1)
a4_No_cont=c(3,3,2,4,3)

# We now combine the observations into one long column (score).
score=c(a1_Cont_before,a2_Part_cont, a3_Cont_after, a4_No_cont)

# We generate a second column (levels), that identifies the group
# for each score.
levels=factor(c(rep("a1_Cont_before",5),rep("a2_Part_cont",5),
                rep("a3_Cont_after",5),rep("a4_No_cont",5)))

# We now form a data frame with the dependent variable and
# the factors.
data=data.frame(score=score,group=levels)

# We now perfom an S(A) anova on the data
aov1=aov(score~levels)

# We now define the Orthogonal contrasts
C_1=c(1,1,-1,-1)
C_2=c(1,-1,0,0)
C_3=c(0,0,1,-1)

# We create a model matrix and include the contrasts as separate
# exploratory variables.
cont_coeff=cbind(C_1,C_2,C_3)
model_matrix=model.matrix(~C(levels,cont_coeff,base=1))
data_reg=data.frame(score,psi_1=model_matrix[,2],psi_2=
                      model_matrix[,3],psi_3=model_matrix[,4])
psi_1=model_matrix[,2]
psi_2=model_matrix[,3]
psi_3=model_matrix[,4]

# Now we perform an orthogonal multiple regression analysis on the data
multi_reg1=ols(score~psi_1+psi_2+psi_3)

# We now organize the results
Df_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Df
Df_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Df
Df_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Df
Df_psi=data.frame(rbind(Df_psi_1,Df_psi_2,Df_psi_3))
SS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Sum
SS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Sum
SS_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Sum
SS_psi=data.frame(rbind(SS_psi_1,SS_psi_2,SS_psi_3))
MS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Mean
MS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Mean
MS_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Mean
MS_psi=data.frame(rbind(MS_psi_1,MS_psi_2,MS_psi_3))
F_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_1))),split = list(levels =
                                                                                       list("C_1" = 1)))[[1]]$F
F_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_2))),split = list(levels =
                                                                                       list("C_2" = 1)))[[1]]$F
F_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_3))),split = list(levels =
                                                                                       list("C_3" = 1)))[[1]]$F
F_psi=data.frame(rbind(F_psi_1,F_psi_2,F_psi_3))
Pr_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Pr
Pr_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Pr
Pr_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Pr
Pr_psi=data.frame(rbind(Pr_psi_1,Pr_psi_2,Pr_psi_3))
Contrast_names=c("Psi_1","Psi_2","Psi_3")
Cont_mat=rbind("Psi_1"=C_1,"Psi_2"=C_2,"Psi_3"=C_3)
Contrasts=data.frame(G1=Cont_mat[,1],G2=Cont_mat[,2],
                     G3=Cont_mat[,3],G4=Cont_mat[,4])
Contrast_Summary=data.frame(Contrast=Contrast_names,DF=
                              Df_psi[,2],Contrast_SS=SS_psi[,2],Mean_Square=MS_psi[,2],
                            F_Value=F_psi[,2],Pr=Pr_psi[,2])
Contrast=c("Psi_1","Psi_2","Psi_3")
Contrast_Summary=data.frame(Contrast=Contrast,DF=Df_psi[,2],
                            Contrast_SS=SS_psi[,2],Mean_Square=MS_psi[,2],F_Value=
                              F_psi[,2],Pr=Pr_psi[,2])

# Now we compute the semi-partial coefficients
# Make sure to add the PATH to the location where the plot is to be saved
pdf('/R_scripts/08_Planned_Non_Ortho_Cont/semi_part_corr.pdf')
semi_part=plot(anova(multi_reg1),what='partial R2')
dev.off()
# Now we print the data and all the results
print(data)
print(data_reg)
summary.aov(aov1)
print(Contrast_Summary)
print(multi_reg1)
print(semi_part)