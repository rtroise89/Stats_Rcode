 # ANOVA Two-factor between subjects, S(AxB)
   # Projective Tests and Test Administrators
   # We have 2 Factors, A & B, with 4 levels in Factor A and
   # 5 levels in Factor B.
   # Therefore there are 4 x 5 = 20 groups with 2 observations
   # (subjects) per group.
   # Factor A: Test Administrator
   # (a1=1,a2=2,a3=3,a4=4)
   # Factor B: Order
   # (b1=I, b2= II, b3= III, b4=IV, b5=V)
   # We collect the data for each level of Factor B across all the
   # levels of Factor A.
   I=c(127,117,111,108, 121,109,111,100)
   II=c(117,113, 111, 100, 109,113,101,92)
   III=c(107,108,99,92, 101,104,91,90)
   IV=c(98,95,95,87, 94,93,89,77)
   V=c(97,96,89,89, 89,92,83,85)
   
   # We now combine the observations into one long column (score).
     score=c(I,II,III,IV,V)
     # We now prepare the labels for the 4x5x2 scores according to
       # the factor levels:
      # Admin_1 Admin_2 Admin_3 Admin_4, Admin_1 Admin_2 Admin_3
        # Admin_4........etc for Factor A
        Test_Admin=gl(4,1,5*4*2, labels = c("Admin_1", "Admin_2",
                                             "Admin_3", "Admin_4"))
      # I I I......., II II ......,III III ....,IV IV ....., V V.....
        # etc for Factor B.
        Order=gl(5,4*2,5*4*2, labels=c("I","II","III","IV","V"))
      # We now form a data frame with the dependent variable and the
        # factors.
        data = data.frame(score = score, Factor_A = factor(Test_Admin),
                           Factor_B=factor(Order))
      # We now perform the ANOVA on the data.
        aov1=aov(score~Test_Admin*Order, data=data)
      # Model III when both A and B are random
        aov2 = aov(score~Test_Admin + Order + Error(Test_Admin:Order),
                    data = data)
      # We now print the data and all the results
        print(data)
        print(summary(aov1))
        print(model.tables(aov1,"means"),digits=3)
        print(summary(aov2))