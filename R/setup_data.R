
case.df$ddate = as.Date(case.df$symptom_date, format = '%m/%d/%Y')

# # subset only symptom onset
# case.df = case.df[(case.df$EVENT_TYPE == 'SYMPTOM_ONSET_DATE'),] # 861 
# 
# case.df$case1 = 1
# 
# case.df.1 = setNames(aggregate(case.df$case1 ~ case.df$ddate, FUN = 'sum'), c("ddate", "case_sum"))

case.dates <- cbind.data.frame('dates'=seq.Date(from=min(case.df$ddate), to=as.Date('2021-12-31'), by='day'))

case.df = merge(case.dates, case.df, by.x = 'dates', by.y = 'ddate', all.x = T)

case.df$N_cases = ifelse(is.na(case.df$N_cases), 0, case.df$N_cases)
# max is 6 cases in one day 

zero = case.df[(case.df$case_sum == 0),] # 3327 days have a zero value;  82% of days have zero 


weather.d = read.csv("./Data/weather_7years.csv")
weather.d$ddate = as.Date(as.character(weather.d$date1))
keep.names = c('ddate','Reported_PrecipSum', 'Temp_Mean')
weather.d = weather.d[,keep.names]

weather.d$Reported_PrecipSum = ifelse(weather.d$Temp_Mean < 32, 0, weather.d$Reported_PrecipSum)
# weather.d = weather.d[(weather.d$ddate < '2019-01-01' & weather.d$ddate > '2017-12-31'),]
weather.d = weather.d[(weather.d$ddate < '2021-12-31' & weather.d$ddate > '2011-01-01'),] # this weather stops at the end of 2020. 

dates = weather.d$ddate
weather.d = as.data.frame(weather.d[,2])
rownames(weather.d) = dates; names(weather.d) = 'rain0' # here rain1 isn't actually lagged by one day. it's the day of the rain. 
# range(weather.d$rain0)
plot(weather.d$rain0, type = 'l')
# now mimic the case data using the 2018 rainfall data and see how it compares 


#Can choose between binary or continuous rainfall, binary seems to give a slightly better fit

# I think binary is a slightly better fit 

# try out categorical by the half inch?
# quantile(weather.d$rain0, probs = seq(0, 1, 0.10), na.rm = T)
# weather.d = weather.d %>% mutate(rain0_cat = cut(rain0, breaks=seq(0, 3.0, 0.15)))
# unique(weather.d$rain0_cat)

weather.d$rain0 = ifelse(weather.d$rain0 > 0.25, 1, 0)

# weather.d$rain0 = ifelse(weather.d$rain0 > 0.75, 1, 0)
# I think continuous rain weights offer better fit than binary rain 
wdates = rownames(weather.d)
weather.d$ddate = as.Date(rownames(weather.d))
weather.d0 = as.data.table(weather.d)
weather.d1 = weather.d0[, unlist(lapply(.SD, shift, n = 1:21, type = 'lag'), recursive = F)]

weather.d1$ddate = as.Date(wdates)
weather.d2  = merge(weather.d1, weather.d, by = 'ddate')
test = merge(case.df, weather.d2, by.x = 'dates',by.y = 'ddate', all.y = T)
test <- test %>%
  mutate(t=row_number(),
         sin1= 2*sin(2*pi*t/365),
         cos1= 2*cos(2*pi*t/365),
         sin2= 2*sin(2*pi*t/182),
         cos2= 2*cos(2*pi*t/182),
         sin3= 2*sin(2*pi*t/91),
         cos3= 2*cos(2*pi*t/91))

test.short = test[(year(test$dates) > 2016),]
mod.test <- glm(case_sum ~ sin1 + cos1 + rain0 + rain01 + rain02 + rain03 + rain04 + rain05 + rain06 + rain07 + rain08 + rain09 + rain010 +
                  rain011 + rain012 + rain013 + rain014 + rain015 + rain016 + rain017 + rain018 + rain019 + rain020, data= test.short, family='poisson')
summary(mod.test) 

test  = test[,c(1:23,45:50)]

