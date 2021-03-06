############################################################
# 작 성 자 : 조 정 호
# 일 시 : 2014. 10. 08.
# 범 위 : SPSS AMOS 구조방정식 -> R을 이용한 확인적 요인분석
############################################################
############################################################
# 확인적요인분석을 하기 위한 패키지 설치
############################################################
install.packages("lavaan", dependencies=TRUE);
library(lavaan);
############################################################# 확인적요인분석에 쓰이는 대표적인 데이터
############################################################
data(HolzingerSwineford1939);
str(HolzingerSwineford1939);
head(HolzingerSwineford1939); # HolzingerSwineford1939 data의 앞부분을 확인해보니,
example(cfa);
###########################################################*#example,(cfa);
# 1. lavaan에는 저자의 논문도 좋고,
# 2. lavaan의 cfa()에 관한 예제를 살펴보면 됩니다.
############################################################
# data설명
# 두개의 다른 학교에서 7,8학년의 시험점수를 통한 정신적인 능력에 대한 데이터이다.
# 변수설명
# 명목변수
# id, sex, ageyr(age,yearpart), agemo(age,monthpart), school(Patuer or Grant-White), grade(grade),
# 연속변수
# x1(Visual perception), x2(Cubes), x3(Lozenges), x4(Paragraph comprehension), x5(Setence completion)
# x6(Word meaning), x7(Speeded addition), x8(Speeded countin of dots), 
# x9(Speeded descrimination straight and curved capitals)
# ageyr 학생의 나이
# agemo 학생이 다닌 개월수
# grade 학생의 학년
############################################################
########################################################################################################
# HozingerSwineford1939 데이터의 각 변수의 분포 파악
########################################################################################################
# 성별변수에 대한 분포 파악
table(HolzingerSwineford1939$sex); 
# 학생의 나이변수에 대한 분포 파악
table(HolzingerSwineford1939$ageyr); 
# 학생의 학교다닌 개월수에 대한 분포 파악
table(HolzingerSwineford1939$agemo); 
# 학생이 다닌 학교에 대한 분포 파악
table(HolzingerSwineford1939$school); 
# 학생의 학년에 대한 분포 파악
table(HolzingerSwineford1939$grade); 
############################################################
############################################
## The famous Holzinger and Swineford (1939) example
############################################################
HS.model <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9 ';
fit <- cfa(HS.model, data=HolzingerSwineford1939);
summary(fit, fit.measures=TRUE) ;
# 확인적요인분석 결과에 대한 요약통계랭