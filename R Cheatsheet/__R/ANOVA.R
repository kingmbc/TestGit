#http://www.cookbook-r.com/Statistical_analysis/ANOVA/
data <- read.table(header=T, text='
                subject sex   age before after
                   1   F   old    9.5   7.1
                   2   M   old   10.3  11.0
                   3   M   old    7.5   5.8
                   4   F   old   12.4   8.8
                   5   M   old   10.2   8.6
                   6   M   old   11.0   8.0
                   7   M young    9.1   3.0
                   8   F young    7.9   5.2
                   9   F   old    6.6   3.4
                   10   M young    7.7   4.0
                   11   M young    9.4   5.3
                   12   M   old   11.6  11.3
                   13   M young    9.9   4.6
                   14   F young    8.6   6.4
                   15   F young   14.3  13.5
                   16   F   old    9.2   4.7
                   17   M young    9.8   5.1
                   18   F   old    9.9   7.3
                   19   F young   13.0   9.5
                   20   M young   10.2   5.4
                   21   M young    9.0   3.7
                   22   F young    7.9   6.2
                   23   M   old   10.1  10.0
                   24   M young    9.0   1.7
                   25   M young    8.6   2.9
                   26   M young    9.4   3.2
                   27   M young    9.7   4.7
                   28   M young    9.3   4.9
                   29   F young   10.7   9.8
                   30   M   old    9.3   9.4
                   ')

################################################# One way between ANOVA
# One way between:
# IV: sex
# DV: before
aov.before.sex <- aov(before ~ sex, data=data)
summary(aov.before.sex)
#             Df Sum Sq Mean Sq F value Pr(>F)
# sex          1  1.529   1.529   0.573 0.4554
# Residuals   28 74.701   2.668 

# Show the means
model.tables(aov.before.sex, "means")
# Tables of means
# Grand mean
# 9.703333
#
#  sex 
#      F      M
#     10  9.532
# rep 11 19.000

################################################# Two way between ANOVA
# 2x2 between:
# IV: sex
# IV: age
# DV: after
# These two calls are equivalent
aov.after.sex.age <- aov(after ~ sex*age, data=data)
aov.after.sex.age <- aov(after ~ sex + age + sex:age, data=data)
summary(aov.after.sex.age)
#             Df  Sum Sq Mean Sq F value   Pr(>F)    
# sex          1  16.078  16.078  4.0384 0.054962 .  
# age          1  38.961  38.961  9.7862 0.004301 ** 
# sex:age      1  89.611  89.611 22.5085  6.6e-05 ***
# Residuals   26 103.512   3.981                     
# ---
# Signif. codes:  0 ?€?***?€? 0.001 ?€?**?€? 0.01 ?€?*?€? 0.05 ?€?.?€? 0.1 ?€? ?€? 1 

# Show the means
model.tables(aov.after.sex.age, "means")
# Tables of means
# Grand mean
#          
# 6.483333 
# 
#  sex 
#          F      M
#      7.445  5.926
# rep 11.000 19.000
# 
#  age 
#      young    old
#      5.556  7.874
# rep 18.000 12.000
# 
#  sex:age 
#      age
# sex   young  old   
#   F    8.433  6.260
#   rep  6.000  5.000
#   M    4.042  9.157
#   rep 12.000  7.000

TukeyHSD(aov.after.sex.age)
