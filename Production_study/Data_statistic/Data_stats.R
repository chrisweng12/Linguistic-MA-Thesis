table = matrix(c(68,20,15,5,119,84,54,29,26,17,14,14,7,94,10,16), nrow=4, byrow=T)

colnames(table) = c("Brown","Blue","Hazel","Green")
rownames(table) = c("Black","Brunette","Red","Blonde")
table

chisq.test(table,correct=T)$expected >= 1
"""
         Brown Blue Hazel Green
Black     TRUE TRUE  TRUE  TRUE
Brunette  TRUE TRUE  TRUE  TRUE
Red       TRUE TRUE  TRUE  TRUE
Blonde    TRUE TRUE  TRUE  TRUE
"""
mean(chisq.test(table,correct=F)$expected) >= 5
"""
[1] TRUE
"""
chisq.test(table)
"""
	Pearson's Chi-squared test

data:  table
X-squared = 138.29, df = 9, p-value < 2.2e-16
"""
round(chisq.test(table)$stdres,digits = 4)
"""
           Brown    Blue   Hazel   Green
Black     6.1365 -4.2538 -0.5750 -2.2879
Brunette  2.1643 -3.3979  2.0502 -0.5083
Red      -0.1008 -2.3111  0.9895  2.5766
Blonde   -8.3282  9.9676 -2.7380  0.7320
"""

table2 = matrix(c(105,25,23,5,8,59,9,1,4,5,74,1,13,29,0,
  161,47,56,59,10,3,6,15,2),nrow = 6, byrow = T)

colnames(table2) = c("p","t","k","non")
rownames(table2) = c("p","t","k","non","unclassfiable","excluded")
table2
"""
                p  t  k non
p             105 25 23   5
t               8 59  9   1
k               4  5 74   1
non            13 29  0 161
unclassfiable  47 56 59  10
excluded        3  6 15   2
"""
chisq.test(table2,correct=T)$expected >= 1
mean(chisq.test(table2,correct=F)$expected) >= 5

chisq.test(table2)
"""
	Pearson's Chi-squared test

data:  table2
X-squared = 820.53, df = 15, p-value < 2.2e-16
"""

round(chisq.test(table2)$stdres,digits = 4)
"""
                   p       t       k     non
p             13.6210 -3.0153 -3.4313 -7.1744
t             -3.1330 11.0701 -2.8546 -5.0825
k             -4.5577 -4.2896 14.2093 -5.3620
non           -7.2209 -4.1604 -9.7075 21.0888
unclassfiable  0.8074  2.6239  3.2295 -6.6608
excluded      -1.6146 -0.2307  3.9212 -2.0759
"""


round(chisq.test(table2)$residuals,digits = 4)
"""
                    p       t       k     non
p             10.4218 -2.3071 -2.6253 -5.4893
t             -2.5641  9.0599 -2.3362 -4.1596
k             -3.7097 -3.4915 11.5655 -4.3644
non           -5.2991 -3.0531 -7.1239 15.4761
unclassfiable  0.6100  1.9825  2.4400 -5.0325
excluded      -1.3728 -0.1961  3.3340 -1.7650
"""