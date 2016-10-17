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
  summarise(avg_sal = mean(salary, na.rm = TRUE ),
            max_sal = max(salary, na.rm = TRUE),
            min_sal = min(salary, na.rm = TRUE) )

sal_year_lg <- salaries %>%
  group_by(yearID, lgID) %>%
  summarise(avg_sal = mean(salary, na.rm = TRUE ),
            max_sal = max(salary, na.rm = TRUE),
            min_sal = min(salary, na.rm = TRUE) )


str(salaries)

sal_mean <- sal_mean %>% mutate(increment = avg_sal - lag(avg_sal, default = avg_sal[1]) )

sal_mean <- sal_mean %>% mutate(percentInc = increment/avg_sal *100 )

library(ggplot2)

ggplot(sal_mean, aes(x= yearID, y = increment )) +
  geom_point()


# basic histogram
hist(sal_mean$avg_sal)

ggplot(sal_mean, aes(x = avg_sal ) ) +
  geom_histogram()

qplot(data = sal_mean, x = avg_sal, main="Histogram of Avg Salaray"  )

qplot(data = sal_mean, x = increment, y= yearID, main="Histogram of Avg Salaray", color=yearID  )

qplot(data = sal_mean, x = log(increment), y= log(yearID), main="Histogram of Avg Salaray", color=yearID  )

qplot(data = salaries, x= salaries$teamID , y = salaries$salary , geom="boxplot"  )

qplot(data=salaries, x=salaries$yearID, y = salaries$salary,  color=salaries$teamID)

ggplot(salaries, aes(y = salaries$yearID, x = salaries$salary  ) ) +
  geom_point()

ggplot(salaries, aes(y = salaries$yearID, x = log(salaries$salary)  ) ) +
  geom_point()

ggplot(sal_mean, aes(y= sal_mean$yearID, x = log(sal_mean$avg_sal)  ) ) +
  geom_point()

ggplot(sal_mean, aes(y= sal_mean$yearID, x = log(sal_mean$avg_sal), stat_  ) ) +
  geom_histogram(stat = "bin", binwidth = 4000 )


# Follow this tutorial
# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

ggplot(salaries, aes(yearID, salary ) ) + geom_point()

ggplot(sal_mean, aes(yearID, avg_sal ) ) + geom_line()

ggplot(sal_year_lg, aes(yearID, avg_sal, col=lgID)  ) + geom_line() +
  geom_smooth(se = FALSE, method = "lm" )

ggplot(salaries, aes(yearID, salary, color=lgID ) ) + 
  geom_point() +
  facet_wrap(lgID~ teamID )

View(salaries)
