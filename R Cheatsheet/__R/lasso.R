# http://stats.stackexchange.com/questions/58531/using-lasso-from-lars-or-glmnet-package-in-r-for-variable-selection

install.packages('lars')
install.packages('glmnet')
require(lars)
require(glmnet)

########LASSO regression vs linear regression 비교 실습
## http://sosal.kr/868
data(diabetes)
attach(diabetes)

#- 종속변수 목록 
colnames(x)
#[1] "age" "sex" "bmi" "map" "tc"  "ldl" "hdl" "tch" "ltg" "glu"
# 이 변수들에 따라 당뇨 y값이 바뀝니다.

# 일반적인 linear regression model에 의한 coefficient
coef_lm <- lm(y~x)
coef_lm

# Lasso regression으로 s에 따른 coefficient 보기
object <- lars(x,y,type="lasso")
plot(object)

# 색이 있는 선들은 각각 독립변수의 regression coefficients를 뜻한다.
# x축은 s대신 shrinkage의 비율을 나타내고, 그 비율에 따라서 독립변수들의 coefficient가 달라진다.
# 물론 x축에 따라서 독립변수의 ±가 달라질 수도 있다.

# 결국 s에 따라서 coefficient가 변하거나, 혹은 값이 0이 되어 독립변수가 제거되어, 
#subset selection이 되는 모습을 확인할 수 있다.

# s=4, 3개의 독립 변수만 사용할 때 coefficients
coef_lars <- coef(object, s=4)
coef_lars

# s=3, 2개의 독립 변수만 사용할 때 coefficients
coef_lars <- coef(object, s=3)
coef_lars

# CP CP-λ Curve를 이용하여 최적의 regression model 선정하기
#Detail: http://mstudioblackboard.tudelft.nl/duwind/Wind%20energy%20online%20reader/Static_pages/Cp_lamda_curve.htm

plot(object, plottype="Cp")
#plottype="Cp"를 이용하면 |beta|/max|beta|값에 따른 Coefficint의 power를 알 수 있다.
#위 그래프에선 8번째의 Cp가 가장 좋다. 하지만 Cp가 절대적이진 않다고 한다.

round(object$beta,4) 

# Cp를 활용한 최적의 coefficient set
coef(object,s=9)

#---------------- 2016.01.21 추가
#Lasso에서 cross validation을 이용하여 최적의 모델 찾아내기
#(10 fold - Cross-validation이란, 전체 데이터에서 10개의 샘플로 쪼갠다음, 9개의 셈플로 모델을 만들고, 
#나머지 1개의 샘플로 모델을 테스트하는 방법이다. 
#10개의 샘플로 9/1로 나누는 모든 조합에 대해서 테스트를 하여, Error를 도출해낸다.)

cv <- lars::cv.lars(x, y, plot.it = FALSE, mode = "step")
idx <- which.max(cv$cv - cv$cv.error <= min(cv$cv))
coef(lars::lars(x, y))[idx,]
coef(lars::lars(x, y))[idx,]


###########################################################################
#glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models
#https://cran.r-project.org/web/packages/glmnet/index.html

#cvfit <- cv.glmnet(x, y)
#coef(cvfit, s = "lambda.1se")

#오랜만에 Lasso 써보려고 하다가 glmnet 내용 추가해봅니다.
coef(cvfit, s = "lambda.1se")
coef(cvfit, s = "lambda.min")

#lambda.min은 Cross-validation에서 에러가 최소로 나타나는 람다값을 찾아준다.

#lambda.1se는 Standard error가 가장 Regularized 된 모델이 되는 람다값을 찾아준다.
#(한글로 전달하기 힘들기 때문에, 전체 문장을 인용해드리면 다음과 같습니다.)

#lambda.min is the value of λ that gives minimum mean cross-validated error. 
#The other λ saved is lambda.1se, which gives the most regularized model such that error is within one standard error of the minimum. 
#To use that, we only need to replace lambda.min with lambda.1se above.






