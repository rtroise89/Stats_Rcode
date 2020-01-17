# Taking off with Loftus: Post Hoc Comparisons
# Newman_Keul's Test

# ANOVA One-factor between subjects S(A)

# NOTE 1: Install package 'gregmisc' in order to use
# make.contrasts

# NOTE 2: make.contrasts will augment an incomplete set of
# orthogonal contrasts with "filler" contrasts

# NOTE 3: Arrange your levels in alphabetical order, else R
# will do it for you

# We have 1 Factor, A, with 5 levels: Hit, Smash, Collide,
# Bump, Contact

# We have 10 subjects per group. Therefore 10 x 5 = 50 subjects
# total.
library(gmodels)
library(lmreg)

# We collect the data for each level of Factor A
a1_Contact=c(21,20,26,46,35,13,41,30,42,26)
a2_Hit=c(23,30,34,51,20,38,34,44,41,35)
a3_Bump=c(35,35,52,29,54,32,30,42,50,21)
a4_Collide=c(44,40,33,45,45,30,46,34,49,44)
a5_Smash=c(39,44,51,47,50,45,39,51,39,55)

# We now combine the observations into one long column (score).
score=c(a1_Contact,a2_Hit, a3_Bump, a4_Collide,a5_Smash)

# We generate a second column (levels), that identifies the
# group for each score.

levels=factor(c(rep("a1_Contact",10),rep("a2_Hit",10),
                rep("a3_Bump",10),rep("a4_Collide",10),rep("a5_Smash",10)))

# We now form a data frame with the dependent variable and the
# factors.
data=data.frame(score=score,group=levels)

# We now define the pairwise comparisons
C_1=c(1,-1,0,0,0)
C_2=c(1,0,-1,0,0)
C_3=c(1,0,0,-1,0)
C_4=c(1,0,0,0,-1)
C_5=c(0,1,-1,0,0)
C_6=c(0,1,0,-1,0)
C_7=c(0,1,0,0,-1)
C_8=c(0,0,1,-1,0)
C_9=c(0,0,1,0,-1)
C_10=c(0,0,0,1,-1)

# We now perform the test for multiple comparisons using
# "Newman Keul's" correction.

# The means with different letters are significantly different.
# NOTE: The source for R script "multcomp" has to be specified.
means=tapply(score,levels,mean)
source("/home/anjali/Desktop/R_scripts/09_Post_Hoc_Comp/
       multcomp.R")
multi_comp=multcomp(as.vector(means),10,45,80,conf.level=.05,
                   type= "NewmanKeuls",decreasing=TRUE)

# We now perfom on ANOVA on the data
aov5=aov(score~levels)

# We now organize the results
Df_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels = list("C_1"
                                                                                                    = 1)))[[1]]$Df
Df_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels = list("C_2"
                                                                                                    = 1)))[[1]]$Df
Df_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels = list("C_3"
                                                                                                    = 1)))[[1]]$Df
Df_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels = list("C_4"
                                                                                                    = 1)))[[1]]$Df
Df_psi_5=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_5))),split = list(levels = list("C_5"
                                                                                                    = 1)))[[1]]$Df
Df_psi_6=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_6))),split = list(levels = list("C_6"
                                                                                                    = 1)))[[1]]$Df
Df_psi_7=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_7))),split = list(levels = list("C_7"
                                                                                                    = 1)))[[1]]$Df
Df_psi_8=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_8))),split = list(levels = list("C_8"
                                                                                                    = 1)))[[1]]$Df
Df_psi_9=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_9))),split = list(levels = list("C_9"
                                                                                                    = 1)))[[1]]$Df
Df_psi_10=summary(aov(score~levels,contrasts=list(levels=
                                                    make.contrasts(C_10))),split = list(levels = list("C_10"
                                                                                                      = 1)))[[1]]$Df
Df_psi=data.frame(rbind(Df_psi_1,Df_psi_2,Df_psi_3,Df_psi_4,
                        Df_psi_5,Df_psi_6,Df_psi_7,Df_psi_8,Df_psi_9,Df_psi_10))
SS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels = list("C_1"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels = list("C_2"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels = list("C_3"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels = list("C_4"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_5=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_5))),split = list(levels = list("C_5"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_6=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_6))),split = list(levels = list("C_6"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_7=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_7))),split = list(levels = list("C_7"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_8=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_8))),split = list(levels = list("C_8"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_9=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_9))),split = list(levels = list("C_9"
                                                                                                    = 1)))[[1]]$Sum
SS_psi_10=summary(aov(score~levels,contrasts=list(levels=
                                                    make.contrasts(C_10))),split = list(levels = list("C_10"
                                                                                                      = 1)))[[1]]$Sum
SS_psi=data.frame(rbind(SS_psi_1,SS_psi_2,SS_psi_3,SS_psi_4,
                        SS_psi_5,SS_psi_6,SS_psi_7,SS_psi_8,SS_psi_9,SS_psi_10))

MS_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels = list("C_1"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels = list("C_2"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels = list("C_3"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels = list("C_4"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_5=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_5))),split = list(levels = list("C_5"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_6=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_6))),split = list(levels = list("C_6"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_7=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_7))),split = list(levels = list("C_7"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_8=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_8))),split = list(levels = list("C_8"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_9=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_9))),split = list(levels = list("C_9"
                                                                                                    = 1)))[[1]]$Mean
MS_psi_10=summary(aov(score~levels,contrasts=list(levels=
                                                    make.contrasts(C_10))),split = list(levels = list("C_10"
                                                                                                      = 1)))[[1]]$Mean
MS_psi=data.frame(rbind(MS_psi_1,MS_psi_2,MS_psi_3,MS_psi_4,
                        MS_psi_5,MS_psi_6,MS_psi_7,MS_psi_8,MS_psi_9,MS_psi_10))
F_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_1))),split = list(levels = list("C_1"
                                                                                                   = 1)))[[1]]$F
F_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_2))),split = list(levels = list("C_2"
                                                                                                   = 1)))[[1]]$F
F_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_3))),split = list(levels = list("C_3"
                                                                                                   = 1)))[[1]]$F
F_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_4))),split = list(levels = list("C_4"
                                                                                                   = 1)))[[1]]$F
F_psi_5=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_5))),split = list(levels = list("C_5"
                                                                                                   = 1)))[[1]]$F
F_psi_6=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_6))),split = list(levels = list("C_6"
                                                                                                   = 1)))[[1]]$F
F_psi_7=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_7))),split = list(levels = list("C_7"
                                                                                                   = 1)))[[1]]$F
F_psi_8=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_8))),split = list(levels = list("C_8"
                                                                                                   = 1)))[[1]]$F
F_psi_9=summary(aov(score~levels,contrasts=list(levels=
                                                  make.contrasts(C_9))),split = list(levels = list("C_9"
                                                                                                   = 1)))[[1]]$F
F_psi_10=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_10))),split = list(levels = list("C_10"
                                                                                                     = 1)))[[1]]$F
F_psi=data.frame(rbind(F_psi_1,F_psi_2,F_psi_3,F_psi_4,
                       F_psi_5,F_psi_6,F_psi_7,F_psi_8,F_psi_9,F_psi_10))
Pr_psi_1=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_1))),split = list(levels = list("C_1"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_2=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_2))),split = list(levels = list("C_2"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_3=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_3))),split = list(levels = list("C_3"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_4=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_4))),split = list(levels = list("C_4"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_5=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_5))),split = list(levels = list("C_5"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_6=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_6))),split = list(levels = list("C_6"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_7=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_7))),split = list(levels = list("C_7"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_8=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_8))),split = list(levels = list("C_8"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_9=summary(aov(score~levels,contrasts=list(levels=
                                                   make.contrasts(C_9))),split = list(levels = list("C_9"
                                                                                                    = 1)))[[1]]$Pr
Pr_psi_10=summary(aov(score~levels,contrasts=list(levels=
                                                    make.contrasts(C_10))),split = list(levels = list("C_10"
                                                                                                      = 1)))[[1]]$Pr
Pr_psi=data.frame(rbind(Pr_psi_1,Pr_psi_2,Pr_psi_3,Pr_psi_4,
                        Pr_psi_5,Pr_psi_6,Pr_psi_7,Pr_psi_8,Pr_psi_9,Pr_psi_10))
Contrast_names=c("Psi_1","Psi_2","Psi_3","Psi_4","Psi_5","Psi_6",
                 "Psi_7","Psi_8","Psi_9","Psi_10")
Cont_mat=rbind("Psi_1"=C_1,"Psi_2"=C_2,"Psi_3"=C_3,"Psi_4"=C_4,
               "Psi_5"=C_5,"Psi_6"=C_6,"Psi_7"=C_7,"Psi_8"=C_8,"Psi_9"=C_9,
               "Psi_10"=C_10)
Contrasts=data.frame(G1=Cont_mat[,1], G2=Cont_mat[,2],
                     G3=Cont_mat[,3], G4=Cont_mat[,4], G5=Cont_mat[,5])

Contrast_Summary=data.frame(Contrast= Contrast_names,
                            DF=Df_psi[,2], Contrast_SS=SS_psi[,2],
                            Mean_Square=MS_psi[,2], F_Value=F_psi[,2], Pr=Pr_psi[,2])
# We now print the data and all the results
print(data)
print(multi_comp)
print('Means with the same letter are not significant')
summary(aov5)
print(Contrasts)
print(Contrast_Summary)