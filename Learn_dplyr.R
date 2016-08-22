


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

str(flights)

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(mean), Cancelled, Diverted )

flights %>%
  group_by(UniqueCarrier) %>%
  summarise_each(funs(min(., na.rm = TRUE), max(., na.rm = TRUE  ) ), matches('Delay')  )

flights %>%
  group_by(Month, DayofMonth ) %>%
  summarise(flights_count = n()  ) %>%
  arrange(desc(flights_count))

flights %>%
  group_by(Month, DayofMonth ) %>%
  tally(sort = TRUE )

flights %>%
  group_by(Dest) %>%
  summarise(flights_count = n(), plane_count = n_distinct(TailNum) )

flights %>%
  group_by(Dest) %>%
  select(Cancelled) %>%
  table() %>%
  head()

flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay ) %>%
  filter(min_rank(desc(DepDelay)) <=2 ) %>%
  arrange(UniqueCarrier, desc(DepDelay) )



?arrange

?min_rank
  