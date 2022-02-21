## ECON4408 R Script

setwd("C:/Users/ASUS/Documents/M.ECON/Semester 2 2021/ECON4408/ECON4408")

install.packages("tidyverse", "stargazer", "magrittr", "haven")

# wbdr.data
# remove missing values
wbdr_na <- complete.cases(wbdr)
wbdr_na <- wbdr[wbdr_na, ]

prosp_na <- complete.cases(prosp)
prosp_na <- prosp[prosp_na, ]

worms_na <- complete.cases(worms)
worms_na <- worms[worms_na, ]

retro_na <- complete.cases(retro)
retro_na <- retro[retro_na, ]

# revision - remove 'World'
wbdr <- wbdr[-c(198), ]
wbdrNA <- wbdrNA[-c(179), ]

# remove NAs in infant mort
wbdrNA2 <- wbdr
wbdrNA2 <- complete.cases(wbdr$infant_mort)
wbdrNA2 <- wbdr[wbdrNA2, ]

# Part 1
# Q1
mean(wbdr_na$ppp_gdp)
# [1] 9171.193
mean(wbdr_na$infant_mort)
# [1] 40.66597
sd(wbdr_na$ppp_gdp)
# [1] 11323.61
sd(wbdr_na$infant_mort)
# [1] 34.36978

# Part 1 Revision
mean(wbdrNA$ppp_gdp)
# 12404.46
sd(wbdrNA$ppp_gdp)
# 14312.16
mean(wbdrNA$infant_mort) 
# 35.629
sd(wbdrNA$infant_mort)
# 34.854
mean(wbdr$fem_lit, na.rm=TRUE) #77.228 - 100 = 22.772
mean(wbdr$male_lit, na.rm=TRUE) #85.582 - 100 = 14.418

stargazer(as.data.frame(wbdr), type="text")

# Q2
# create a subset of wbdr where no NAs in ppp_gdp
wbdrNA <- wbdr
wbdrNA <- complete.cases(wbdr$ppp_gdp)
wbdrNA <- wbdr[wbdrNA, ]
# find mean, min, max of tot_lit, infant_mort among the 50 richest and poorest
wbdrNAasc <- arrange(wbdrNA, ppp_gdp)
wbdrNAdesc <- arrange(wbdrNA, desc(ppp_gdp))
wbdrNAasc <- head(wbdrNAasc, 50) # 50 poorest
wbdrNAdesc <- head(wbdrNAdesc, 50) # 50 richest
summary(wbdrNAasc$tot_lit)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 26.18   48.65   59.30   60.71   72.51   99.64       7 
summary(wbdrNAasc$infant_mort)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 33.50   57.41   74.50   78.13   92.95  155.00 
summary(wbdrNAdesc$tot_lit)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 84.37   92.36   94.88   94.84   98.89   99.79      25 
summary(wbdrNAdesc$infant_mort)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.700   3.475   4.350   8.518   7.205 123.600       2 

wbdrNAasc$tot_illit <- 100 - wbdrNAasc$tot_lit
wbdrNAdesc$tot_illit <- 100 - wbdrNAdesc$tot_lit

stargazer(as.data.frame(wbdrNAasc), type="text")
stargazer(as.data.frame(wbdrNAdesc), type="text")

# Q3
# median GDP in subsample with NA and without
summary(wbdrNA)
# median ppp_gdp 6821.1
hist(wbdrNA$ppp_gdp, xlab = "GDP per capita, PPP 2005", main = "Distribution of Cross-Country Income")
abline(v = rep(median(wbdrNA$ppp_gdp)), col = "red", )
abline(v = rep(mean(wbdrNA$ppp_gdp)), col = "blue")

# Q4 - return to unrestricted data wbdr
wbdr$tot_illit <- 100 - wbdr$tot_lit
model1 <- lm(tot_lit ~ ppp_gdp, wbdr) # coeff ppp_gdp 0.0007054 (0.000127)***
stargazer(model1, type="text")
model1_2 <- lm(tot_illit ~ ppp_gdp, wbdr)
stargazer(model1_2, type = "text")

# Q5
model2 <- lm(infant_mort ~ ppp_gdp, wbdr)
stargazer(model2, type="text") # -0.0013651 (0.0001531)***

# Q6
model3 <- lm(infant_mort ~ tot_lit, wbdr) # -1.41015 (0.09593)***
stargazer(model3, type="text")
ggplot(data = wbdr, mapping = aes(x = tot_lit, y = infant_mort)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,) + 
  xlab('Illiteracy Rate') +
  ylab('Infant Mortality')

model3_2 <- lm(infant_mort ~ tot_illit, wbdr)
stargazer(model3_2, type = "text")
ggplot(data = wbdr, mapping = aes(x = tot_illit, y = infant_mort)) +
  theme_bw() + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE,) + 
  xlab('Illiteracy Rate') +
  ylab('Infant Mortality')

# Part 2 - worms.dta
stargazer(as.data.frame(worms), type = "text")

# Q2 i 38495 pupils
summary(is.na(worms$pupid))
#    Mode   FALSE    TRUE 
# logical   38495      76 
# Q2 ii What percentage are boys? 0.5207669
# sex - "Pupil gender, 1=male 0=female"
summary(worms$sex == 1) # 15347 are male
summary(worms$sex == 0) # 14123 are female, 9101 NAs
# Q2 iii What percentage of pupils took the deworming pill in 1998?
summary(worms$pill98 == 1)
#    Mode   FALSE    TRUE    NA's 
# logical   25801    7025    5745 
summary(worms$pill98 == 0) # 0 is treatment
#    Mode   FALSE    TRUE    NA's 
# logical    7025   25801    5745 

summary(worms$pupid & worms$pill98 == 0) # 25725
summary(worms$pupid & worms$pill98 == 1) # 7025, 5745 NAs

7025 / (25801+7025) # 21.4%

# Q2 iv What percentage of schools was assigned deworming in 1998? Group 1 school - 11639 / 34792 = 0.3345309
# Is this more or less than the percentage of pupils who actually took the pill in 1998? 7025 / 32826 = 0.2140072 (more)

summary(worms$pupid & worms$wgrp == 1) # 11639 of 34,792 = 0.3345309
summary(worms$pupid & worms$wgrp == 2) # 11995 of 34,792
summary(worms$pupid & worms$wgrp == 3) # 11158 of 34,792, 3779 NAs

# Q3 Using the data, find the difference in outcomes (school participation and worm infection rates) between:
# Students that took the pill and students which did not in 1998. Is this a good estimate of the effect of taking the pill on school attendance? Why or why not?
worms_na <- worms
worms_na <- complete.cases(worms$totpar98)
worms_na <- worms[worms_na,]
tapply(worms_na$totpar98, worms_na$pill98 == 1, mean)
#     FALSE      TRUE(dewormed) 
# 0.7455613 0.8765350 diff is 0.1309737 (higher participation rates for the treated group)

worms_na2 <- worms
worms_na2 <- complete.cases(worms$infect_early99)
worms_na2 <- worms[worms_na2,]
tapply(worms_na2$infect_early99, worms_na2$pill98 == 1, mean)
#     FALSE      TRUE(dewormed)
# 0.5086878 0.2464986 diff is -0.2621892 (lower infection for treated group)

# Students in treatment schools versus students not in treatment schools in 1998 (regardless of whether they actually took the pill). How does this compare to the answer in part (i) and what does it imply?
tapply(worms_na$totpar98, worms_na$wgrp == 1, mean)
tapply(worms_na$totpar98, worms_na$wgrp == 2 & 3, mean)

tapply(worms_na2$infect_early99, worms_na2$wgrp == 1, mean)
tapply(worms_na2$infect_early99, worms_na2$wgrp == 2 & 3, mean)

# Part 3 - retro.dta and prosp.dta
unique(retro$wallchar)
summary(retro$wallchar == 0) # change 0 to 1, 2, 3, 4, 5, 15, "NA"
retro$wc_r <- retro$wallchar/4

#Q1 revision - wallchar
retro2 <- retro
retro2$wc_r <- retro2$wallchar/4
retro2 <- complete.cases(retro2$wc_r)
retro2 <- retro[retro2, ]

# Inefficient way to create dummy
retro_na$wc_r <- retro_na$wallchar
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 1, NA)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 2, NA)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 3, NA)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 5, NA)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 15, NA)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 0, 0)
retro_na$wc_r <- replace(retro_na$wc_r, retro_na$wc_r == 4, 1)

stargazer(as.data.frame(retro), type = "text") # To generate a summary, stargazer requires a data.frame as input.
stargazer(as.data.frame(prosp), type = "text")

retro %>%
  ggplot( aes(x=nmsc)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef") +
  xlab('Normalized Test Score') +
  ylab('Density')

#Q2
# regress normalized test score on wc_r
retro_na <- complete.cases(retro_na$wc_r)
retro_na <- retro[retro_na, ]
model4 <- lm(nmsc ~ wc_r, retro) # coeff 0.003 (0.016)
stargazer(model4, type="text")

model4_2 <- lm(nmsc ~ wc_r, retro_na) # the correct model coef 0.046 (0.016) ***
stargazer(model4_2, type="text")

#Q2 revision
model4_3 <- lm(nmsc ~ wc_r, retro2)
stargazer(model4_3, type="text") # coeff wc_r 0.206***(0.007)

# multiple regression
str(retro)
model5 <- lm(nmsc ~ wc_r + struct + rain + bboard + bkpup + stalvl + classsz, retro) # look more about dummy
stargazer(model5, type="text")

# before regression, have to replace values in struct and bboard since there are some nonintegers
retro_na$struct <- replace(retro_na$struct, retro_na$struct == 0.5, NA)
retro_na$bboard <- replace(retro_na$bboard, retro_na$bboard == 0.5, NA)
retro_na$bboard <- replace(retro_na$bboard, retro_na$bboard == 0.75, NA)
model5_2 <- lm(nmsc ~ wc_r + struct + rain + bboard + bkpup + deskp + stalvl + classsz, retro_na)
stargazer(model5_2, type="text")

# multiple regression - revision retro2
model5_3 <- lm(nmsc ~ wc_r + struct + rain + bboard + bkpup + deskp + stalvl + classsz, retro2)
stargazer(model5_3, type="text") 
# coeff wc_r 0.196*** (0.007)

retro2$struct <- replace(retro2$struct, retro2$struct == 0.5, NA)
retro2$bboard <- replace(retro2$bboard, retro2$bboard == 0.5, NA)
retro2$bboard <- replace(retro2$bboard, retro2$bboard == 0.75, NA)

model5_4 <- lm(nmsc ~ wc_r + struct + rain + bboard + bkpup + deskp + stalvl + classsz, retro2)
stargazer(model5_4, type="text") 
# coef 0.2004435*** (0.0074888)

#Q3 i - prospective data
prosp_na <- prosp
prosp_na <- complete.cases(prosp$score)
prosp_na <- prosp[prosp_na, ]
prosp_na <- complete.cases(prosp$nmsc)
prosp_na <- prosp[prosp_na, ]

mean(prosp_na$score[prosp_na$wc == 1]) - mean(prosp_na$score[prosp_na$wc == 0])
# -0.0008773832
mean(prosp_na$nmsc[prosp_na$wc == 1]) - mean(prosp_na$nmsc[prosp_na$wc == 0])
# -0.00368964
model6 <- lm(score ~ wc, prosp_na)
stargazer(model6, type="text")
model7 <- lm(nmsc ~ wc, prosp_na)
stargazer(model7, type="text")

#Q3 ii wall charts related subject
mean(prosp_na$score[prosp_na$wc == 1 & prosp_na$sub == "ghc"]) - mean(prosp_na$score[prosp_na$wc == 0 & prosp_na$sub == "ghc"])
# -0.0009720624
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "ghc"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "ghc"])
# -0.008952263
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "sca"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "sca"])
# 0.02668808
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "mat"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "mat"])
# -0.001233701
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "hsb"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "hsb"])
# -0.01177551

model8 <- lm(nmsc ~ wc + sub, prosp_na)
stargazer(model8, type = "text")
model8_1 <- lm(nmsc ~ wc*sub, prosp_na)
stargazer(model8_1, type = "text")

prosp_na$subghc <- ifelse(prosp_na$sub == "ghc", 1, 0)
model8_2 <- lm(nmsc ~ wc + subghc, prosp_na)
stargazer(model8_2, type="text")
model8_3 <- lm (nmsc ~ wc*subghc, prosp_na)
stargazer(model8_3, type="text")

#Q3 iii non wallchart subject - eng, kis, acm
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "eng"])
mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "eng"])
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "eng"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "eng"])
# -0.001206595
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "kis"])
mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "kis"])
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "kis"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "kis"])
# -0.0201652
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "acm"])
mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "acm"])
mean(prosp_na$nmsc[prosp_na$wc == 1 & prosp_na$sub == "acm"]) - mean(prosp_na$nmsc[prosp_na$wc == 0 & prosp_na$sub == "acm"])
# -0.01151898