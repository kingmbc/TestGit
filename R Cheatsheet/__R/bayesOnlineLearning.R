# http://freesearch.pe.kr/archives/4497
# 어떻게 하면 싱싱한 데이터를 모형에 바로 적용할 수 있을까? – Bayesian Online Leaning
install.packages('devtools')
library(devtools)
install_github('haven-jeon/BOPR')

pacman::p_load(BOPR, ggplot2, pROC)

idx  <- sample(1:nrow(credit_approval))
first_train_set  <- credit_approval[idx[1:200],]
second_train_set  <- credit_approval[idx[201:400],]
test_set <- credit_approval[idx[401:690],]

bopr_mdl <- BOPR(A16 ~ A1 + A4 + A5 + A7 + A9 + A10 + A12 + A13 , first_train_set, epsilon = 0.03)
test_set$pred_1  <- predict(bopr_mdl, test_set)[,1]
ggplot(test_set, aes(pred_1)) + geom_density(aes(fill=factor(A16)), alpha=0.6) + xlim(0,1) + ggtitle("ε : 0.03")
roc(test_set$A16, test_set$pred_1)

bopr_mdl_up  <- online_leraning(bopr_mdl, second_train_set)
test_set$pred_2  <- predict(bopr_mdl_up, test_set)[,1]
ggplot(test_set, aes(pred_2)) + geom_density(aes(fill=factor(A16)), alpha=0.6) + xlim(0,1) + ggtitle("ε : 0.03, with  online learning")
roc(test_set$A16, test_set$pred_2)

bopr_mdl <- BOPR(A16 ~ A1 + A4 + A5 + A7 + A9 + A10 + A12 + A13 , first_train_set, epsilon = 0.07)
# test_set$pred_1  <- predict(bopr_mdl, test_set)[,1]
# ggplot(test_set, aes(pred_1)) + geom_density(aes(fill=factor(A16)), alpha=0.6) + xlim(0,1) + ggtitle("ε : 0.07")
# roc(test_set$A16, test_set$pred_1)

bopr_mdl_up  <- online_leraning(bopr_mdl, second_train_set)
test_set$pred_2  <- predict(bopr_mdl_up, test_set)[,1]
ggplot(test_set, aes(pred_2)) + geom_density(aes(fill=factor(A16)), alpha=0.6) + xlim(0,1) + ggtitle("ε : 0.07, with  online learning")
