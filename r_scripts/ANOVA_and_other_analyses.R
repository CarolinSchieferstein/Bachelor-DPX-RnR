# ANOVA in R: von statmethods.net
# 
# welche Variablen?
#   Reaktionszeit in Abhängigkeit von Trialtyp
# * UV = Trialtype ; AV = RT
# * gilt nur für die Hits (kann mans separat noch für die Incorrects machen)
# 
# 1. Daten nach Reactiontype aufteilen
# 2. ANOVA TT (within Faktor) → RT
# 3. t-Tests zwischen den 4 Bedigungen (→ Alpha-Kumulierung!)
# 
# oder Reactiontype als IV?
#   nachher mit RST-PQ Werten als Moderator? (oder einfach erstmal generell Korrelation)

# One Within Factor
fit <- aov(y~A+Error(Subject/A),data=mydataframe)

layout(matrix(c(1,2,3,4),2,2)) # optional layout
plot(fit) # diagnostic plots


# Two-way Interaction Plot
attach(mtcars)
gears <- factor(gears)
cyl <- factor(cyl)
interaction.plot(cyl, gear, mpg, type="b", col=c(1:3),
                 leg.bty="o", leg.bg="beige", lwd=2, pch=c(18,24,22),
                 xlab="Number of Cylinders",
                 ylab="Mean Miles Per Gallon",
                 main="Interaction Plot") # --> könnte noch interessant werden

# Plot Means with Error Bars
library(gplots)
attach(mtcars)
cyl <- factor(cyl)
plotmeans(mpg~cyl,xlab="Number of Cylinders",
          ylab="Miles Per Gallon", 
          main="Mean Plot\nwith 95% CI") # --> für kommende und die Grafik die ich schon hab


#####ANOVA Assumptions

# Detect Outliers in the MTCARS Data
library(mvoutlier)
outliers <- aq.plot(mtcars[c("mpg","disp","hp","drat","wt","qsec")])
outliers # show list of outliers 

# Q-Q Plot for variable MPG
attach(mtcars)
qqnorm(mpg)
qqline(mpg) # --> meint das einenTest auf Normalverteilung der Daten?

# Bartlett Test of Homogeneity of Variances
bartlett.test(y~G, data=mydata)

# Figner-Killeen Test of Homogeneity of Variances
fligner.test(y~G, data=mydata) # --> G wäre eine grouping Vartiable also brauche ich das nicht oder?
  

##### ***
# WAS IST DAS HIER?? ***
# cor(x, use=, method= )
# [ x=Matrix/dataframe ; use=complete.obs löscht potentielle NA ; method= pearson, spearman or kendall ]
# --> welche Methode ist am geeignetsten?
  
# Correlations with significance levels
library(Hmisc)
rcorr(x, type="pearson") # type can be pearson or spearman

#mtcars is a data frame
rcorr(as.matrix(mtcars))  # --> braucht Hmsic package, nur für Matrizen

# Correlation matrix from mtcars
# with mpg, cyl, and disp as rows
# and hp, drat, and wt as columns
x <- mtcars[1:3]
y <- mtcars[4:6]
cor(x, y) 

################ T-TEST

# one sample t-test
t.test(y,mu=3) # Ho: mu=3 


################## FREQUENCIES            https://www.statmethods.net/stats/frequencies.html

# 2-Way Frequency Table
attach(mydata)
mytable <- table(A,B) # A will be rows, B will be columns
mytable # print table

margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)

prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages  --> wahrscheinlich das was ich für die FB-Skalenwerte wollte
