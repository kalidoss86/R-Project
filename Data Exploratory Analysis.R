# http://varianceexplained.org/RData/code/code_lesson4/#segment2
library(dplyr)

salaries <- read.csv("http://dgrtwo.github.io/pages/lahman/Salaries.csv")

dim(salaries)

str(salaries)

# salaries$yearID <- as.factor(salaries$yearID)

# salaries$yearID <- as.numeric(salaries$yearID)

head(salaries)

head(filter(salaries, yearID>2000 ))

head(filter(salaries, playerID == 'vaughmo01'  ),20)

head(filter(salaries, playerID == 'vaughmo01'  ) %>% arrange(desc(salary)),20) 

mean(salaries$salary)

max(salaries$salary)

min(salaries$salary)

sal_mean <- salaries %>% 
  group_by( yearID) %>%
  summarise(avg_sal = mean(salary, na.rm = TRUE ) ) %>%
  head(n=21L)

str(salaries)

sal_mean <- sal_mean %>% mutate(increment = avg_sal - lag(avg_sal, default = avg_sal[1]) )

sal_mean <- sal_mean %>% mutate(percentInc = increment/avg_sal *100 )
