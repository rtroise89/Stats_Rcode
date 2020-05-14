# ANOVA One-factor between subjects S(A)
# Smith's experiment on context effects

# NOTE 1: Install package 'gregmisc' and 'Design' in order to
# use make.contrasts and 'ols'

# NOTE 2: make.contrasts will augment an incomplete set of
# orthogonal contrasts with "filler" contrasts

# NOTE 3: Arrange your levels in alphabetical order, else R will
# do it for you

# We have 1 Factor, A, with 5 levels: Same, Different, Imagery,
# Photo, Placebo

# We have 10 subjects per group. Therefore 10 x 5 = 50 subjects total.
# We collect the data for each level of Factor A (in alphabetical
# order!!!)
library(gmodels)

a1_Same=c(25,26,17,15,14,17,14,20,11,21)
a2_Different=c(11,21,9,6,7,14,12,4,7,19)
a3_Imagery=c(14,15,29,10,12,22,14,20,22,12)
a4_Photo=c(25,15,23,21,18,24,14,27,12,11)
a5_Placebo=c(8,20,10,7,15,7,1,17,11,4)

# We now combine the observations into one long column (score).
score=c(a1_Same,a2_Different,a3_Imagery,a4_Photo,a5_Placebo)

# We generate a second column (group), that identifies the group
# for each score.

levels=factor(c(rep("a1_Same",10),rep("a2_Different",10),
                rep("a3_Imagery",10),rep("a4_Photo",10),rep("a5_Placebo",10)))
data=data.frame(score=score,group=levels)

# We now form the set of orthogonal contrasts
psi_1=c(2,-3,2,2,-3)
psi_2=c(2,0,-1,-1,0)
psi_3=c(0,0,1,-1,0)
psi_4=c(0,1,0,0,-1)

# We now form a matrix of contrast coefficients
cont_coeff=(rbind(psi_1,psi_2,psi_3,psi_4))

# We create a model matrix and include the contrasts as separate
# exploratory variables.

Contrasts=cbind(psi_1,psi_2,psi_3,psi_4)
model_matrix=model.matrix(~C(levels,Contrasts,base=1))
data_reg=data.frame(score,Psi_1=model_matrix[,2],Psi_2=model_matrix[,3],
                    Psi_3=model_matrix[,4],Psi_4=model_matrix[,5])

Psi_1=model_matrix[,2]
Psi_2=model_matrix[,3]
Psi_3=model_matrix[,4]
Psi_4=model_matrix[,5]

# Now we perform an orthogonal multiple regression analysis on
# the data
multi_reg1=lm(score~Psi_1+Psi_2+Psi_3+Psi_4)

# We now perform an ANOVA on the contrasts
aov2=aov(score~levels,contrasts=list(levels=make.contrasts(cont_coeff)))

aov1=aov(score~levels)

print(data)
print(data_reg)
print(multi_reg1)
summary(aov2, split = list(levels = list("psi_1" = 1,
summary(aov1)