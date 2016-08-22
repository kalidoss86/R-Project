rm(cultivar2wine)
rm(wine)
rm(directory)
rm(filenames)
rm(i)
rm(mylist)
rm(names)
rm(numfiles)
rm(makeProfilePlot)


library(dplyr)
library(hflights)

data("hflights")
head(hflights)


flights <- tbl_df(hflights)

flights

print(flights, n=20)


data.frame(head(flights))

# R approach
flights[flights$Month == 1 & flights$DayofMonth == 1,  ]

# dplyr approach
filter(flights,  Month == 1, DayofMonth==1 )

filter(flights, UniqueCarrier == 'AA' | UniqueCarrier == 'UA' )

filter(flights, UniqueCarrier %in% c('AA', 'UA') )

select(flights, DepTime, ArrTime, FlightNum )

select(flights,  Year:DayofMonth, contains("Taxi"), contains("Delay") )

filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60 )

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay") ]

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)

flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(desc(DepDelay))

flights$Speed <- flights$Distance / flights$AirTime * 60
flights[, c("Distance", "AirTime", "Speed" ) ]

flights %>% 
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime * 60 )

flights <- flights %>% mutate(Speed = Distance/AirTime*60)

head(with(flights, tapply(ArrDelay, Dest, mean, na.rm = TRUE )  ) )

head(aggregate(ArrDelay ~ Dest, flights, mean ))

flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE)  )
