# starting up
library(tidyverse)
setwd("~/Google_Drive/UCU/Winter_3/ucaccmet2j/Week_II/Week_III_2/sports_BMI")

# reading in the files
basketball <- read_csv('Basketball_data.csv')
boxer <- read_csv('boxer_data.csv')
icehockey <- read_csv('icehockey_data.csv')
volleyball <- read_csv('volleyball_data.csv')
rugby <- read_csv('rugby_data.csv')

# adding sports label
basketball_label <- mutate(basketball, sport_label = "basketball")
boxer_label <- mutate(boxer, sport_label = "boxing")
icehockey_label <- mutate(icehockey, sport_label = 'icehockey')
volleyball_label <- mutate(volleyball, sport_label = 'volleyball')
rugby_label <- mutate(rugby, sport_label = 'rugby')

# merging files ...
allsports <- rbind(icehockey_label, rugby_label, volleyball_label, boxer_label, basketball_label)

# converting to numeric and adding bmi
allsports$Height <- as.numeric(as.character(allsports$Height))
allsports$Weight <- as.numeric(as.character(allsports$Weight))
allsports$Weight <- allsports$Weight/1000
allsports_bmi <-mutate(allsports, BMI = Weight/(Height*Height))

# filtering for extreme weight and height and bmi
allsports_bmi <- filter(allsports_bmi, BMI < 30, BMI > 14)

# means
allsports_by_label <- group_by(allsports_bmi, sport_label)
allsports_sum <- summarise(allsports_by_label, 
                           mean_BMI = mean(BMI), 
                           mean_height = mean(Height),
                           mean_weight = mean(Weight))


# plot mean BMI
ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_BMI) +
  geom_col()

# plot mean height
ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_height) +
  geom_col()

# plot mean weight
ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_weight) +
  geom_col()

# histogram BMI
ggplot(data = allsports_bmi) +
  aes(BMI, after_stat(density), colour = sport_label) +
  geom_freqpoly(binwidth = 0.7)

# histogram height
ggplot(data = allsports_bmi) +
  aes(Height, after_stat(density), colour = sport_label) +
  geom_freqpoly(binwidth = 0.05)

# histogram weight
ggplot(data = allsports_bmi) +
  aes(Weight, after_stat(density), colour = sport_label) +
  geom_freqpoly(binwidth = 8)
