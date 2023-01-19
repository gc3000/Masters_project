# MASTERS PROJECT CODE 

# Twitter Key

key = "Wtn7cEgiT5okpz8qljQ1nbNzJ"
secret = "ariCoPDXMCov0SWacAjifw3zwsVE3tbkfmHkhJ5Dap7VHgSvto"

#API key:
#Wtn7cEgiT5okpz8qljQ1nbNzJ

#API secret key:
#ariCoPDXMCov0SWacAjifw3zwsVE3tbkfmHkhJ5Dap7VHgSvto


#===============#
# AUTHORISING R #
#===============#

library("rtweet")
library("dplyr")
library("stringr")
library("tm")
library("base64enc")
library("httpuv")
library("gridExtra") 
library("Matrix")
library("tidyr")
library("treemap")
library("ggpubr")


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")



#Set up OAuth

create_token(
  app = "my_twitter_app",
  consumer_key = key,
  consumer_secret = secret)
  
#TWEETS, EARLIEST FEB 2020 or LAST ONE BEFORE FEB IF NO FEB TWEETS ELSE LAST 100 TWEETS 
  
# Cancer Council NSW

#"2020-02-04 03:19:03 UTC" From this beginning date to present 

tl_ccnsw <- get_timelines("CCNewSouthWales", n = 1000, include_rts = FALSE)

tl_ccnsw$text[1:52]

# Tweets from closest to present day back since 4th of Feburary 2020. 

tl_ccnsw_cd <- tl_ccnsw[1:52, ]

# Deaf Society NSW 

tl_dfsnsw <- get_timelines("deafsocietynsw", n = 1000, include_rts = FALSE)

# Tweets from closest to presdent day back since 23rd September 2019

tl_dfsnsw_cd <- tl_dfnsw[1:9, ]

# St John Ambulance NSW 

tl_sja <- get_timelines("stjohnnsw", n = 1000, include_rts = FALSE)

# From present day to 8th of March 2021 

tl_sja_cd <- tl_sja[1:100, ]

# Heart Foundation 

tl_hf <- get_timelines("heartfoundation", n = 1000, include_rts = FALSE)

#4th of April 2021 first tweet to present 

tl_hf_cd <- tl_hf[1:100, ]

# The Leporsy Mission

tl_tlma <- get_timelines("TLMAustralia", n = 1000, include_rts = FALSE)

#17th of december 2020 to present (last 100)

tl_tlma_cd <- tl_tlma[1:100, ]

#St Vinnies NSW 

tl_vnsw <- get_timelines("VinniesNSW", n = 1000, include_rts = FALSE)

#24th of Feburary 2021 to present (last 100)

tl_vnsw_cd <- tl_vnsw[1:100, ]

# Guide Dogs NSW 

gd_nsw <- get_timelines("GuideDogsAus", n = 1000, include_rts = FALSE)

# 22nd of March 2021 to present (last 100)

gd_nsw_cd <- gd_nsw[1:100, ]

# Movember Foundation

tl_mo <- get_timelines("MovemberAus", n = 1000, include_rts = FALSE)

# November 12 2020 (last 100)

tl_mo_cd <- tl_mo[1:100, ]

#LIVERwell incorporating victoria hepatitis 

tl_lw <- get_timelines("hepvic", n = 1000, include_rts = FALSE)

# 6th of May 2020 to present (last 100)

tl_lw_cd <- tl_lw[1:100, ]

# Diabetes NSW 

tl_dnsw <- get_timelines("DNSW_ACT", n = 1000, includte_rts = FALSE)

#30th of march 2021 to present (last 100)

tl_dnsw_cd <- tl_dnsw[1:100, ]

#Tweet_type column 

#Cancer Council tl_ccnsw_cd

#Cultivation - C, Solicitation - A (ask), Stewardship - S

cancer_council_cat <-

 c("C", "S", "S", "S", "S", "C", "A", "S", "C", "A",
 
 "S", "S", "S", "A", "C", "C", "C", "C", "C", "A",  
 
 "S", "A", "C", "C", "C", "A", "C", "C", "C", "C",
 
 "C", "S", "A", "A", "C", "C", "A", "A", "A", "A",      
 
 "A", "S", "S", "A", "S", "A", "A", "C", "C", "S",
 
 "A", "C") # 52 total. 
 
 # Guide Dogs Australia tweet type
 
 guide_dogs_cat <- 
 
 c("A", "A", "C", "C", "C", "C", "C", "C", "C", "S",
 
 "S", "C", "S", "S", "C", "S", "S", "S", "C", "S", 
 
 "S", "A", "S", "C", "S", "C", "C", "C", "C", "C", 
 
 "S", "S", "S", "S", "S", "S", "S", "S", "S", "C",
 
 "C", "C", "C", "C", "C", "S", "A", "C", "C", "S", 
 
 "C", "A", "C", "C", "C", "C", "C", "C", "C", "C", 
 
 "C", "C", "C", "C", "S", "S", "S", "C", "C", "C", 
 
 "C", "C", "C", "S", "S", "S", "S", "S", "C", "C", 
 
 "C", "C", "C", "C", "S", "C", "S", "S", "S", "C", 
 
 "C", "C", "C", "C", "C", "A", "A", "C", "C", "S")
 
 
 diabetes_nsw_cat <- 
 
 c("C", "C", "C", "S", "A", "C", "C", "C", "C", "C", 
 
 "C", "C", "C", "S", "C", "C", "C", "C", "S", "C", 
 
 "S", "C", "C", "C", "C", "C", "C", "C", "C", "C", 
 
 "C", "C", "C", "C", "S", "S", "C", "C", "S", "C", 
 
 "C", "C", "C", "S", "C", "S", "C", "C", "C", "C", 
 
 "C", "C", "C", "S", "C", "C", "C", "A", "C", "C", 
 
 "C", "C", "C", "C", "C", "C", "S", "C", "C", "C", 
 
 "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", 
 
 "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", 
 
 "C", "C", "A", "C", "C", "C", "C", "C", "C", "C") 
 
 heart_foundation_cat <- 
 
 c("A", "A", "C", "C", "S", "C", "C", "C", "C", "A", 
 
 "C", "S", "S", "S", "C", "C", "S", "C", "C", "C", 
 
 "C", "C", "S", "C", "C", "S", "S", "C", "C", "C", 
 
 "C", "C", "C", "C", "C", "S", "A", "C", "C", "S", 
 
 "S", "S", "S", "S", "C", "C", "A", "C", "C", "C", 
 
 "C", "C", "S", "S", "C", "S", "S", "S", "S", "S", 
 
 "S", "C", "C", "S", "C", "C", "C", "A", "C", "C", 
 
 "C", "S", "S", "S", "C", "C", "S", "C", "C", "C", 
 
 "A", "C", "C", "C", "C", "S", "C", "C", "C", "S", 
 
 "C", "C", "S", "S", "C", "C", "C", "C", "S", "C")
 
 
 liverwell_cat <-
 
 c("S", "A", "S", "S", "S", "C", "S", "C", "A", "S",
 
 "S", "C", "C", "C", "S", "A", "C", "C", "A", "A", 
 
 "A", "A", "C", "A", "S", "C", "C", "A", "C", "C", 
 
 "C", "A", "C", "C", "C", "C", "S", "S", "C", "S",
 
 "S", "C", "C", "C", "C", "S", "A", "C", "S", "C", 
 
 "S", "A", "S", "C", "S", "C", "C", "C", "C", "C", 
 
 "S", "C", "C", "S", "S", "C", "C", "C", "C", "C", 
 
 "C", "A", "C", "C", "C", "C", "S", "C", "A", "C", 
 
 "A", "A", "C", "C", "A", "C", "C", "C", "C", "S", 
 
 "C", "A", "S", "S", "A", "C", "S", "C", "S", "C")
 
 leporsy_foundation_cat <-
 
 c("A", "A", "A", "A", "A", "C", "A", "C", "A", "C", 
 
 "S", "C", "A", "A", "S", "A", "C", "S", "C", "A", 
 
 "A", "C", "A", "C", "S", "S", "A", "C", "A", "S",
 
 "S", "S", "A", "C", "C", "A", "C", "A", "A", "A", 
 
 "S", "A", "A", "C", "A", "S", "C", "C", "A", "C", 
 
 "A", "C", "A", "C", "A", "C", "A", "C", "C", "A", 
 
 "C", "A", "A", "S", "S", "C", "C", "A", "C", "A", 
 
 "S", "C", "C", "C", "C", "S", "A", "S", "A", "S", 
 
 "C", "C", "S", "A", "S", "S", "S", "S", "S", "S", 
 
 "S", "S", "S", "C", "A", "A", "S", "S", "C", "A")
 
 movember_cat <- 
 
 c("A", "S", "A", "A", "A", "C", "S", "S", "S", "C", 
 
 "C", "A", "S", "S", "S", "C", "C", "S", "S", "S", 
 
 "S", "C", "C", "A", "A", "A", "A", "A", "S", "C", 
 
 "C", "C", "C", "C", "S", "S", "S", "S", "C", "S", 
 
 "S", "A", "S", "S", "A", "C", "S", "S", "S", "C", 
 
 "A", "S", "C", "A", "S", "S", "S", "S", "S", "C", 
 
 "S", "A", "C", "A", "S", "S", "C", "A", "C", "C", 
 
 "A", "C", "A", "S", "S", "S", "C", "A", "A", "C", 
 
 "A", "C", "S", "A", "A", "C", "A", "A", "C", "C", 
 
 "C", "A", "C", "C", "A", "C", "A", "C", "C", "A")
 
 
 sj_nsw_cat <- 
 
 c("S", "C", "S", "C", "C", "S", "S", "A", "C", "C", 
 
 "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", 
 
 "S", "C", "S", "S", "S", "S", "C", "S", "A", "C", 
 
 "S", "A", "S", "C", "S", "C", "C", "C", "C", "C",
 
 "C", "C", "C", "C", "S", "C", "S", "S", "C", "S", 
 
 "S", "S", "A", "S", "C", "S", "C", "C", "S", "C", 
 
 "C", "C", "C", "C", "S", "S", "C", "S", "C", "C", 
 
 "C", "C", "C", "S", "S", "C", "S", "C", "S", "C", 
 
 "S", "C", "C", "S", "S", "S", "C", "C", "C", "C", 
 
 "S", "C", "C", "C", "S", "C", "C", "C", "C", "S")
 
 
 
 vinnie_nsw_cat <-
 
 c("S", "S", "C", "C", "S", "C", "A", "C", "C", "C", 
 
 "C", "S", "C", "S", "C", "C", "C", "S", "S", "S", 
 
 "C", "C", "C", "A", "C", "C", "C", "C", "C", "C", 
 
 "C", "C", "C", "C", "S", "S", "S", "C", "C", "S", 
 
 "C", "A", "C", "C", "C", "C", "C", "C", "A", "C", 
 
 "C","C", "C", "C", "C", "C", "S", "S", "A", "S", 
 
 "C", "C", "S", "C", "S", "C", "S", "C", "S", "C", 
 
 "C", "S", "A", "A", "S", "A", "C", "C", "S", "C", 
 
 "C", "S", "C", "C", "C", "C", "C", "S", "C", "C", 
 
 "C", "S", "S", "C", "C", "C", "C", "C", "C", "S")
 
 deaf_nsw <- 
 
 c("C", "C", "C", "S", "S", "S", "S", "S", "S")
 

# add category columns to dataframes. 

gd_nsw_cd[, "cat"] <- guide_dogs_cat 
tl_dfsnsw_cd[ , "cat"] <- deaf_nsw
tl_hf_cd$cat <- heart_foundation_cat 
tl_lw_cd$cat <- liverwell_cat
tl_movember_cd$cat <- movember_cat
tl_sja_cd$cat <- sj_nsw_cat 
tl_tlma_cd$cat <- leporsy_foundation_cat
tl_vnsw_cd$cat <- vinnie_nsw_cat 
tl_ccnsw_cd$cat <- tl_ccnsw_cd$cancer_council_cat



massive_df <- bind_rows(gd_nsw_cd, tl_ccnsw_cd, tl_dfsnsw_cd, tl_dnsw_cd, tl_hf_cd, tl_lw_cd, 
                        tl_movember_cd, tl_sja_cd, tl_tlma_cd, tl_vnsw_cd)
						


# likes_photo <- massive_df[ , c("media_type", "favorite_count")]
#g <- unlist(likes_photo[[1]])
#gg <- cbind.data.frame(g, massive_df$favorite_count)
#  photo_df <- gg[gg$g == "photo", ]
# nphoto_df <- gg[gg$g == "no-photo", ]


#> var(c_df$favorite_count)
#[1] 31.46858
#> var(a_df$favorite_count)
#[1] 14.99052
#> var(s_df$favorite_count)
#[1] 63.73035
#> 54/15
#[1] 3.6
#> res.aov <- aov(favorite_count ~ cat, data = massive_df)
#> res.aov
#Call:
#  aov(formula = favorite_count ~ cat, data = massive_df)

#Terms:
#                    cat Residuals
#Sum of Squares    350.22  32906.16
#Deg. of Freedom        2       858

#Residual standard error: 6.192913
#Estimated effects may be unbalanced
#> summary(res.aov)
#             Df Sum Sq Mean Sq F value Pr(>F)  
#cat           2    350  175.11   4.566 0.0107 *
#Residuals   858  32906   38.35                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> TukeyHSD(res.aov)
#  Tukey multiple comparisons of means
#    95% family-wise confidence level

#Fit: aov(formula = favorite_count ~ cat, data = massive_df)

#$cat
#        diff        lwr      upr     p adj
#C-A 0.433857 -0.9883481 1.856062 0.7539628
#S-A 1.700190  0.1456786 3.254700 0.0280375
#S-C 1.266332  0.1332203 2.399445 0.0240103

# The difference is between cultivation and stewardship and stewardship and solitication. No significant difference
# between solitication and cultivation. Stewardship tweets get more likes compared to cultivation and solitication tweets,
# therefore to improve awareness and possible donations, it is important to focus on stewardship tweets as these tweets
# get favorited the most. 


#@JeremyMiles is right. First, there's a rule of thumb that the ANOVA is robust to heterogeneity of variance so long as the largest variance is not more than 4 times the smallest variance. Furthermore, the general effect of heterogeneity of variance is to make the ANOVA less efficient. That is, you would have lower power. Since you have a significant effect anyway, there is less reason to be concerned here.
#Update:
#
#I demonstrate my point about lower efficiency / power here: Efficiency of beta estimates with heteroscedasticity
#I have a comprehensive overview of strategies for dealing with problematic heteroscedasticity in one-way ANOVAs here: Alternatives to one-way ANOVA for heteroscedastic data
#Share
#Cite
#Improve this answer
#Follow
#edited Apr 13 '17 at 12:44

#Community♦
#1
#answered Apr 23 '13 at 18:20


#> library("ggpubr")
#> ggboxplot(massive_df, x = "cat", y = "favorite_count", 
#+           color = "cat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#+           order = c("C", "S", "A"),
#+           ylab = "Likes", xlab = "Tweet Type")


#> TukeyHSD(res.aov)
#  Tukey multiple comparisons of means
#    95% family-wise confidence level

#Fit: aov(formula = favorite_count ~ cat, data = massive_df)

#$cat
#        diff        lwr      upr     p adj
#C-A 0.433857 -0.9883481 1.856062 0.7539628
#S-A 1.700190  0.1456786 3.254700 0.0280375
#S-C 1.266332  0.1332203 2.399445 0.0240103

> mean(tl_vnsw_cd$favorite_count)
[1] 1.93
> mean(tl_tlma_cd$favorite_count)
[1] 1.09
> mean(tl_sja_cd$favorite_count)
[1] 3.17
> mean(tl_movember_cd$favorite_count)
[1] 9.59
> mean(tl_lw_cd$favorite_count)
[1] 1.44
> mean(tl_hf_cd$favorite_count)
[1] 9.05
> mean(tl_dnsw_cd$favorite_count)
[1] 0.94
> mean(tl_dfsnsw_cd$favorite_count)
[1] 0.5555556
> mean(tl_ccnsw_cd$favorite_count)
[1] 3.692308
> mean(gd_nsw_cd$favorite_count)
[1] 2.16

#create treemap QUESTION 1

charity <- c("VinnesNSW", "Leporsy Mission", "St Johns Ambulance NSW", "Movember", "LIVERwell", "Heart Foundation", "Diabetes NSW",
"Deaf NSW", "CCNSW", "Guide Dogs Aus")

mean_likes <- c(1.93, 1.09, 3.17, 9.59, 1.44, 9.05, 0.94, 0.56, 3.69, 2.16)

mdata <- data.frame(charity, mean_likes)

treemap(mdata, index = "charity", vSize = "mean_likes", type = "index", title = "Average number of likes per charity")

# Histogram QUESTION 1

hist(massive_df$favorite_count, col = 'lightgreen', xlab = "Likes", 
ylab = "Twitter Posts", main = "Like Distribution of Twitter Posts", breaks = 40)

# QUESTION 2

> massive_df$favorite_count[massive_df$cat == "C"]
  [1]  0  1  0  0  0  0  2  2  2  2  4  3  1 13  3  1  2 20  0  0  0  3  0  0  0  0  0  0  0  1  1  1  2  0  0  0  2  0
 [39]  0  0  0  2  0  0  0  2  4  2  5  1  2 54  0  0  0  4  1  1  2  6  2  4 12  5  1  1  3  5  3  2  1  2  1  3  2  2
 [77]  5  2  9  1  0  0  0  1  0  0  4  2  4  3  0  0  3  2  0  1  0  0  0  1  0  0  0  3 10  3  1  0  1  0  0  3  0  0
[115]  2  1  0  0  0  2  3  3  0  1  0  1  1  0  1  0  0  0  2  2  2  0  1  0  1  2  1  0  0  0  0  0  0  0  0  0  0  1
[153]  1  2  0  1  0  0  1  1  0  0  2  4  1  1  0  0  5 16  9  6 11  6 29 20  4  4 18  3  7 11  3 34  2  6 12  7  2 25
[191]  1  5  1  7  1 18  8 13  5  9  0 13  8  3 16 11 19  1 18  7  5  8  8  4  2 10 14  5  8  3  5  3  9  1  7  2 51 12
[229] 10  1  3  3  0  2  0  0  0  0  0  2  3  0  1  0  0  0  0  0  8  1  0  1  3  0  1  0  3  2  1  2  0  0  4  1  7  4
[267]  1  1  0  1  1  1  1  9  1  0  2  0  1  2  0  1  0  9  6 16 13 16  5  4  8 23  5  3 13  6 15 13  7  6 11  2  6  8
[305] 12 28  4  6 10  6  4  7 18 16 14  6  7  1  3  2  1  3  2  3  1  3  8  6  7  8  5  5  1  1  7  3  2  5  1  1  2  2
[343]  2  7  2  3  0  2  2  2  1  1  2  2  1  4  2  3  2  2  2  2  3  1  2  2  1  1  1  1  5  2  2  6  3  4  0  1  2  0
[381]  0  2  1  0  1  1  1  2  1  0  3  0  0  0  1  0  2  1  0  1  1  1  1  1  3  1  2  0  0  0  5  0  1  0  0  2  3  2
[419]  2  0  1  3  0  1  0  5  2  3  6  0  1  3  3  1  2 10  1  6  2  3  2  1  6  2  2  2  1  2  3  1  2  0  1  0  0  0
[457]  1  1  0  1  0  1  2  2  2  1  2  1  2  1  0  0  1  2  1
> massive_df$favorite_count[massive_df$cat == "S"]
  [1]  0  0  1  0  3  0  0  0  0  0  7  1  0  3  3  8  1  1  3  2 14  1  0  0  2  0  0  2  7  3  0  1  1  1  3  0  9  3
 [39] 10  5  6  2  2  0  3  4  1  9  0  1  0  0  0  0  3  0  3  0  2  0  3  0  0  0  1  2  6 47 35  3 11  0 14  2 26  3
 [77]  0  1  0  3  0  0  5  0  3  1  1  2 44  2  7  3 15  0 16  2 41  2  1  3  0  0  2  1  1  0  1 10  1  1  8  2  0  0
[115]  1  1  4  2  2  2  0  3  0  1  0  4  2 14  2  8  7 12 17 11 70 24  7 12 24 15 26  6 17  6  9 10 11 12  4 13  9 19
[153]  9  0 15  1  2 20  3  4  4  2  5 11  3  3  1  2  3  6  9  3  3  3  3  5  3  2  5  5  9  2  5  2  2  4  2  5  2  7
[191]  1  3  8  9  7  2  3  1  4  6  0  0  0  0  1 11  0  1  2  2  3  1  0  0  1  1  1  0  0  0  2  2  1  1  1  1  9  0
[229]  2  3  0  0  2  2  0  0  3  6  1  1  3  4  9  3  2  1  2  0  2  3  1  0
> massive_df$favorite_count[massive_df$cat == "A"]
  [1]  0  0  0  0  2  1  1  1  2  1  2  1  1  0  1  4  6  4  3  5  2  6  5 21  0  0  0  2 19  2  4  3  2  7  1  0  2  1
 [39]  0  0  0  0  0  0  5  0  4  1  2  2  0  1  3  5 12  7 13  5  2  8  2  4  5 12  3  4  5  3  5  2  6  5 12  7  6  6
 [77] 10 21  2  8  6  3  8  0  1  2  3  1  0  1  0  0  1  3  0  1  2  1  0  2  2  1  1  0  0  0  0  1  0  0  2  0  0  2
[115]  5  3  3  0  1  1  1  1  0  1  0  0  2  1  4  3  0  2  7  0

> meanA
[1] 2.902985
> meanC
[1] 3.336842
> meanS
[1] 4.603175

# QUESTION 2 NORMALITY 

par(mfrow = c(1, 2)) # combine plots
# histogram
hist(res.aov$residuals)
# QQ-plot
qqPlot(res_aov$residuals,
  id = FALSE # id = FALSE to remove point identification
)


ggboxplot(massive_df, x = "massive_df$cat", y = "massive_df$favorite_count", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("ctrl", "trt1", "trt2"),
          ylab = "Likes", xlab = "Tweet type")
		  
		  
#QUESTION 3

t.test(photo_group$`massive_df$favorite_count`, no_photo_group$`massive_df$favorite_count`)

	Welch Two Sample t-test

data:  photo_group$`massive_df$favorite_count` and no_photo_group$`massive_df$favorite_count`
t = 3.1405, df = 772.61, p-value = 0.001751
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.5056079 2.1914858
sample estimates:
mean of x mean of y 
 4.228866  2.880319 




	Welch Two Sample t-test

data:  no_hashtag_likes and hashtag_likes
t = 0.87693, df = 827.23, p-value = 0.3808
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.4521162  1.1823374
sample estimates:
mean of x mean of y 
 3.787524  3.422414
