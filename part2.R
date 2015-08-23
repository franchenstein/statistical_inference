library(ggplot2)
library(datasets)
library(broom)
library(dplyr)

#Loading the Tooth Growth data:
tg <- ToothGrowth

#Exploratory Analysis:
ggplot(data = tg,
       aes(x = as.factor(dose),
           y = len,
           fill = supp)) +
    geom_bar(stat = "identity") +
    facet_grid(.~supp) +
    xlab("Dose in Miligrams") +
    ylab("Tooth Length") +
    guides(fill = guide_legend(title = "Supplement Type"))

#T-Test for difference in tooth length means across supplement types:
t_supp <- t.test(len ~ supp, tg, var.equal = FALSE)
tidy(t_supp)

#Pairwise t-tests for differences in tooth lengths means across dose levels:

pWise <- t.test(tg$len[tg$dose == 2], tg$len[tg$dose == 1]) %>%
tidy %>%
mutate(null_hypothesis = '2mg - 1mg = 0') %>%
select(9, 1:8)

pWise <- t.test(tg$len[tg$dose == 2], tg$len[tg$dose == 0.5]) %>%
tidy %>%
mutate(null_hypothesis = '2mg - 0.5mg = 0') %>%
select(9, 1:8) %>%
bind_rows(pWise, .)

pWise <- t.test(tg$len[tg$dose == 1], tg$len[tg$dose == 0.5]) %>%
tidy %>%
mutate(null_hypothesis = '1mg - 0.5mg = 0') %>%
select(9, 1:8) %>%
bind_rows(pWise, .)

print.data.frame(pWise)