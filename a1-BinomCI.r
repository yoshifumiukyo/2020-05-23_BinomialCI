############################################################
# R-project                                                #
# Program      : a1-BinomCI.r                              #
# Protocol     :                                           #
# Date         :                                           #
# Last         :                                           #
# Programmer   : yoshifumi ukyo                            #
#                                                          #
############################################################
# [Ver.0000]                                               #
# Memorandom   :                                           #
#                                                          #
############################################################


#----- clean-up working directory 
rm(list = (ls(all = TRUE)))
#----- library assignment 
base_dir <- ""
setwd(base_dir)


############################################################
# normal approximation                                     #
############################################################

x <- 12 
n <- 16
p <- x/n 

low <- p - qnorm(p = 0.975) * sqrt(p * (1 - p)/n)
upp <- p + qnorm(p = 0.975) * sqrt(p * (1 - p)/n)
c(p, low, upp)


############################################################
# clopper-pearson CI                                       #
# 0.75 [0.476, 0.927]                                      #
############################################################

res <- binom.test(x = 12, n = 16, alternative = "two.sided", conf.level = 0.95)

x <- 12
n <- 16

k1 <- 2 * (x + 1)
k2 <- 2 * (n - x) 
p_upp <- k1 / (k1 + k2/qf(p = 0.975, df1 = k1, df2 = k2))

l1 <- 2 * (n - x + 1)
l2 <- 2 * x 

p_low <- l2 / (l2 + l1 * qf(p = 0.975, df1 = l1, df2 = l2))



