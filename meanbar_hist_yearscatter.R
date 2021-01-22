# starting up
library(tidyverse)
setwd("~/Google_Drive/UCU/Winter_3/ucaccmet2j/Week_II/Week_III_2/sports_BMI")

install.packages("ggthemes")  
library(ggthemes) 

install.packages("RColorBrewer")
library(RColorBrewer)

# reading in the files
basketball <- read_csv('Basketball_date_data.csv')
boxer <- read_csv('boxer_date_data.csv')
icehockey <- read_csv('icehockey_date_data.csv')
volleyball <- read_csv('volleyball_date_data.csv')
rugby <- read_csv('rugby_date_data.csv')

# adding sports label
basketball_label <- mutate(basketball, sport_label = "basketball")
boxer_label <- mutate(boxer, sport_label = "boxing")
icehockey_label <- mutate(icehockey, sport_label = 'icehockey')
volleyball_label <- mutate(volleyball, sport_label = 'volleyball')
rugby_label <- mutate(rugby, sport_label = 'rugby')

# merging files ...
allsports <- rbind(icehockey_label, rugby_label, volleyball_label, boxer_label, basketball_label)

# converting to numeric and dates and adding bmi
allsports$Height <- as.numeric(as.character(allsports$Height))
allsports$Weight <- as.numeric(as.character(allsports$Weight))
allsports$Weight <- allsports$Weight/1000
allsports$BirthDate <- parse_date(allsports$BirthDate, "%Y-%m-%d")
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
p <- ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_BMI, fill = sport_label) +
  geom_col() +
  labs(title = 'Mean BMI for different sports',
       subtitle = 'Each column represents the mean BMI for a sport',
       fill = 'Sports') +
  xlab('Sport') +
  ylab('Mean BMI (kg/m^2)') +
  theme_gray(base_family = 'Times')
ggsave('mean_bmi.png', p , width = 8, height = 6)

# plot mean height
p <- ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_height, fill = sport_label) +
  geom_col() +
  labs(title = 'Mean height for different sports',
       subtitle = 'Each column represents the mean height for a sport',
       fill = 'Sports') +
  xlab('Sport') +
  ylab('Mean height (m)') +
  theme_gray(base_family = 'Times')
ggsave('mean_height.png', p , width = 8, height = 6)

# plot mean weight
p <- ggplot(data = allsports_sum) + 
  aes(x = sport_label, y = mean_weight, fill = sport_label) +
  geom_col(position = 'dodge') +
  labs(title = 'Mean weight for different sports',
       subtitle = 'Each column represents the mean weight for a sport',
       fill = 'Sports') +
  xlab('Sport') +
  ylab('Mean weight (kg)') +
  theme_gray(base_family = 'Times') 
ggsave('mean_weight.png', p , width = 8, height = 6)

# histogram BMI
p <- ggplot(data = allsports_bmi) +
  aes(BMI, after_stat(density), 
      colour = sport_label) +
  geom_freqpoly(binwidth = 0.7,
                size = 1) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Relative occurence of BMI for different sports',
       subtitle = 'Each line represents the distribution of BMI for a sport') +
  xlab('BMI (kg/m^2)') +
  ylab('Density') +
  scale_colour_discrete(name = "Sports")
ggsave('histogram_bmi.png', p , width = 8, height = 6)

# histogram height
p <- ggplot(data = allsports_bmi) +
  aes(Height, after_stat(density), 
      colour = sport_label) +
  geom_freqpoly(binwidth = 0.05,
                size = 1) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Relative occurence of height for different sports',
       subtitle = 'Each line represents the distribution of height for a sport') +
  xlab('Height (m)') +
  ylab('Density') +
  scale_colour_discrete(name = 'Sports')
ggsave('histogram_height.png', p , width = 8, height = 6)

# histogram weight
p <- ggplot(data = allsports_bmi) +
  aes(Weight, after_stat(density), 
      colour = sport_label) +
  geom_freqpoly(binwidth = 8,
                size = 1) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Relative occurence of weight for different sports',
       subtitle = 'Each line represents the distribution of weight for a sport') +
  xlab('Weight (kg)') +
  ylab('Density') +
  scale_colour_discrete(name = 'Sports')
ggsave('histogram_weight.png', p , width = 8, height = 6)

# calculate mean for every year
allsports_year <- mutate(allsports_bmi, year = format(BirthDate, "%Y"))
allsports_year$year <- parse_date(allsports_year$year, "%Y")
allsports_year <- group_by(allsports_year, year, sport_label)
allsports_year_sum <- summarise(allsports_year, 
                           mean_BMI = mean(BMI), 
                           mean_height = mean(Height),
                           mean_weight = mean(Weight))

# change of mean height over time for different sports
p <- ggplot(data = allsports_year_sum) +
  aes(x = year, y = mean_height, colour = sport_label) +
  geom_point() +
  geom_smooth(method = loess) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Mean height per year',
       subtitle = 'Each color represents the mean height for a sport') +
  xlab('Year') +
  ylab('Mean height (m)') +
  scale_colour_discrete(name = 'Sports')
ggsave('height_year_scatter.png', p , width = 8, height = 6)

p <- ggplot(data = allsports_year_sum) +
  aes(x = year, y = mean_weight, colour = sport_label) +
  geom_point() +
  geom_smooth(method = loess) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Mean weight per year',
       subtitle = 'Each color represents the mean weight for a sport') +
  xlab('Year') +
  ylab('Mean weight (kg)') +
  scale_colour_discrete(name = 'Sports')
ggsave('weight_year_scatter.png', p , width = 8, height = 6)

p <- ggplot(data = allsports_year_sum) +
  aes(x = year, y = mean_BMI, colour = sport_label) +
  geom_point() +
  geom_smooth(method = loess) +
  theme_gray(base_family = 'Times') +
  labs(title = 'Mean BMI per year',
       subtitle = 'Each color represents the mean BMI for a sport') +
  xlab('Year') +
  ylab('Mean BMI (kg/m^2)') +
  scale_colour_discrete(name = 'Sports')
ggsave('bmi_year_scatter.png', p , width = 8, height = 6)


