# code to install arules package
install.packages("arules")
library(arules)
data(package = "arules")

data(Groceries) # load data
?Groceries # show info

class(Groceries)
inspect(Groceries[1:3])

summary(Groceries)

###################### Association Rule Learning #########################
## support as 0.01
## confidence threshold as 0.5

rules <- apriori(Groceries,
                 parameter = list(support = 0.01, confidence = 0.5, maxlen = 5))
summary(rules)                 

rules <- sort(rules, by = "support") # sort by support
inspect(rules[1:5]) # inspect top 5 rules


qual <- quality(rules) # extract quality measures
# compute p(A) and p(B)
pA <- qual$support/qual$confidence
pB <- qual$confidence/qual$lift
# compute lift upper and lower bounds
U <- apply(cbind(1/pA, 1/pB), 1, min)
L <- apply(cbind(1/pA + 1/pB - 1/(pA*pB), 0.01/(pA*pB), 0.5/pB, 0), 1, max)
sLift <- (qual$lift - L)/(U - L) # standardized lift
final_quality <- data.frame(rule = labels(rules),
           sLift, Lift = qual$lift, L, U) # print rules and associated sLift
sort(final_quality, )