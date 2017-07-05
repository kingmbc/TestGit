install.packages("bnlearn");
require(bnlearn);
data(asia);
head(asia);
tail(asia);
summary(asia);

res = empty.graph(names(asia))
modelstring(res) = "[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]"
plot(res)

net.data <- bn.fit(hc(asia, debug = TRUE), asia, debug=TRUE);
net.data;
############################### net.data
#  Bayesian network parameters
#  
#  Parameters of node A (multinomial distribution)
#  Conditional probability table:
#    no    yes 
#  0.9916 0.0084 
#  
#  Parameters of node S (multinomial distribution)
#  Conditional probability table:
#    no   yes 
#  0.497 0.503 
#  
#  Parameters of node T (multinomial distribution)  
#  Conditional probability table:    
#    no    yes 
#  0.9912 0.0088 
#  
#  Parameters of node L (multinomial distribution)  
#  Conditional probability table:    
#    S
#  L             no        yes
#  no  0.98631791 0.88230616
#  yes 0.01368209 0.11769384
#  
#  Parameters of node B (multinomial distribution)  
#  Conditional probability table:    
#    S
#  B            no       yes
#  no  0.7006036 0.2823062
#  yes 0.2993964 0.7176938
#  
#  Parameters of node E (multinomial distribution)  
#  Conditional probability table:    
#    , , L = no
#  
#  T
#  E     no yes
#  no   1   0
#  yes  0   1
#  
#  , , L = yes
#  
#  T
#  E     no yes
#  no   0   0
#  yes  1   1
#  
#  
#  Parameters of node X (multinomial distribution)  
#  Conditional probability table:    
#    E
#  X              no         yes
#  no  0.956587473 0.005405405
#  yes 0.043412527 0.994594595
#  
#  Parameters of node D (multinomial distribution)  
#  Conditional probability table:    
#    , , E = no
#  
#  B
#  D             no        yes
#  no  0.90017286 0.21373057
#  yes 0.09982714 0.78626943
#  
#  , , E = yes
#  
#  B
#  D             no        yes
#  no  0.27737226 0.14592275
#  yes 0.72262774 0.85407725
#  ##################################################



?hc
?bn.fit

cpquery(net.data, (T == "yes"), TRUE); ##�ƹ� ������ ������ �� ���� ������, ���� Ȯ����?
cpquery(net.data, (L == "yes"), TRUE); ##�ƹ� ������ ������ �� ���� ������, ��� Ȯ����?
cpquery(net.data, (B == "yes"), TRUE); ##�ƹ� ������ ������ �� ���� ������, ������� Ȯ����?

cpquery(net.data, (T == "yes"), (A == "yes" & S == "no"), debug=TRUE); #�ƽþ� �湮, ������ ==> ���� Ȯ��?
cpquery(net.data, (L == "yes"), (A == "yes" & S == "no"), debug=TRUE); #�ƽþ� �湮, ������ ==> ��� Ȯ��?
cpquery(net.data, (B == "yes"), (A == "yes" & S == "no"), debug=TRUE); #�ƽþ� �湮, ������ ==> ������� Ȯ��?

#�ƽþ� �湮, ������, ȣ�� ��� ����, X-Ray �׽�Ʈ ��� �缺 ==> ������� Ȯ��?
cpquery(net.data, (T == "yes"), (A == "yes" & S == "no" & D == "no" & X == "yes"), debug=TRUE);
cpquery(net.data, (L == "yes"), (A == "yes" & S == "no" & D == "no" & X == "yes"), debug=TRUE);
cpquery(net.data, (B == "yes"), (A == "yes" & S == "no" & D == "no" & X == "yes"), debug=TRUE);


## discrete Bayesian network (it is the same with ordinal nodes).
data(learning.test)
head(learning.test);
fitted = bn.fit(hc(learning.test), learning.test)
# the result should be around 0.025.
cpquery(fitted, (B == "b"), (A == "a"))
# for a single observation, predict the value of a single
# variable conditional on the others.
var = names(learning.test)
obs = 2
str = paste("(", names(learning.test)[-3], "=='",
            sapply(learning.test[obs,-3], as.character), "')",
            sep = "", collapse = " & ")
str
str2 = paste("(", names(learning.test)[3], "=='",
             as.character(learning.test[obs, 3]), "')", sep = "")
str2
cpquery(fitted, eval(parse(text = str2)), eval(parse(text = str)))
# do the same with likelihood weighting
cpquery(fitted, event = eval(parse(text = str2)),
        evidence = as.list(learning.test[2, -3]), method = "lw")
# conditional distribution of A given C == "c".
table(cpdist(fitted, "A", (C == "c")))

## Gaussian Bayesian network.
data(gaussian.test)
fitted = bn.fit(hc(gaussian.test), gaussian.test)
# the result should be around 0.04.
cpquery(fitted,
        event = ((A >= 0) & (A <= 1)) & ((B >= 0) & (B <= 3)),
        evidence = (C + D < 10))
