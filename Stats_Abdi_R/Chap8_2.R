# ANOVA One-factor between subjects S(A)
# Romeo and Juliet - Non-Orthogonal Contrasts with Sidak
# correction

# NOTE 1: Install and load package 'gregmisc' in order to use
# make.contrasts

# NOTE 2: make.contrasts will augment an incomplete set of
# orthogonal contrasts with "filler" contrasts

# NOTE 3: Arrange your levels in alphabetical order, else R
# will do it for you

# We have 1 Factor, A, with 4 levels:
# Context Before,
# Partial Context,
# Context After,
# No Context

# We have 5 subjects per group. Therefore 5 x 4 = 20 subjects
# total.
library(gmodels)
library(lmreg)

# We collect the data for each level of Factor A
a1_Cont_before=c(5,9,8,4,9)
a2_Part_cont=c(5,4,3,5,4)
a3_Cont_after=c(2,4,5,4,1)
a4_No_cont=c(3,3,2,4,3)

# We now combine the observations into one long column (score).
score=c(a1_Cont_before,a2_Part_cont, a3_Cont_after, a4_No_cont)

# We generate a second column (levels) that identifies the group
# for each score.
levels=factor(c(rep("a1_Cont_before",5),rep("a2_Part_cont",5),
                rep("a3_Cont_after",5),rep("a4_No_cont",5)))

# We now form a data frame with the dependent variable and the
# factors.
data=data.frame(score=score,group=levels)

# We now define the non-orthogonal contrasts
C_1=c(1,1,1,-3)
C_2=c(0,0,1,-1)
C_3=c(3,-1,-1,-1)
C_4=c(1,-1,0,0)

# We now perform the test for multiple comparisons using "Sidak"
# correction.
# The means with different letters are significantly different.

# NOTE: The source for R script "multcomp" has to be specified.
means=tapply(score,levels,mean)
#source("?/R_scripts/08_Planned_Non_Ortho_Cont/
#       multcomp.R")
multi_comp=multcomp(as.vector(means),5,16,2.350,conf.level=
                     .05,type= "Sidak",decreasing=TRUE)

# We now perfom on ANOVA on the data
aov5=aov(score~levels)

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
Df_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))), split = list(levels =
                                                                                         list("C_4" = 1)))[[1]]$Df
Df_psi=data.frame(rbind(Df_psi_1,Df_psi_2,Df_psi_3,
                        Df_psi_4))
SS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Sum
SS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Sum
SS_psi_3=summary(aov(score~levels,contrasts=
                       list(levels=make.contrasts(C_3))),split =
                   list(levels = list("C_3" = 1)))[[1]]$Sum
SS_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels =
                                                                                        list("C_4" = 1)))[[1]]$Sum
SS_psi=data.frame(rbind(SS_psi_1,SS_psi_2,SS_psi_3,SS_psi_4))
MS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Mean
MS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Mean
MS_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Mean
MS_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels =
                                                                                        list("C_4" = 1)))[[1]]$Mean
MS_psi=data.frame(rbind(MS_psi_1,MS_psi_2,MS_psi_3,MS_psi_4))
F_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_1))),split = list(levels =
                                                                                       list("C_1" = 1)))[[1]]$F
F_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_2))),split = list(levels =
                                                                                       list("C_2" = 1)))[[1]]$F
F_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_3))),split = list(levels =
                                                                                       list("C_3" = 1)))[[1]]$F
F_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_4))),split = list(levels =
                                                                                       list("C_4" = 1)))[[1]]$F
F_psi=data.frame(rbind(F_psi_1,F_psi_2,F_psi_3,F_psi_4))
Pr_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels =
                                                                                        list("C_1" = 1)))[[1]]$Pr
Pr_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels =
                                                                                        list("C_2" = 1)))[[1]]$Pr
Pr_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels =
                                                                                        list("C_3" = 1)))[[1]]$Pr
Pr_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels =
                                                                                        list("C_4" = 1)))[[1]]$Pr
Pr_psi=data.frame(rbind(Pr_psi_1,Pr_psi_2,Pr_psi_3,Pr_psi_4))
Contrast_names=c("Psi_1","Psi_2","Psi_3","Psi_4")
Cont_mat=rbind("Psi_1"=C_1,"Psi_2"=C_2,"Psi_3"=C_3,"Psi_4"=C_4)
Contrasts=data.frame(G1=Cont_mat[,1],G2=Cont_mat[,2],
                     G3=Cont_mat[,3],G4=Cont_mat[,4])
Contrast_Summary=data.frame(Contrast=Contrast_names,DF=Df_psi[,2],
                            Contrast_SS=SS_psi[,2],Mean_Square=MS_psi[,2],F_Value=F_psi[,2],
                            Pr=Pr_psi[,2])

# We now print the data and all the results
print(data)
summary(aov5)
print(Contrasts)
print(Contrast_Summary)
print(multi_comp)
print('Means with the same letter are not significantly different')