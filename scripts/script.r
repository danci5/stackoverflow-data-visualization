library(tidyverse)
library(stringr)
library(scales)
library(lubridate)
library(devtools)
library(ggplot2)
library(plotrix)  # pie chart
library(plotly)  # pie chart
library("tm")   #text mining
# install_github("lchiffon/wordcloud2")
library(wordcloud2)

getwd()
# working directory should be ending with stackoverflow-visualization/scripts

# DATA LOADING
questions <- read_csv("../data/Questions.csv")
answers <- read_csv("../data/Answers.csv")
tags <- read_csv("../data/Tags.csv")

# DATA AGGREGATION 
# binding rows of all posts and creating new column which defines the post type
# answers don't have Title
r_posts <- bind_rows(questions, answers) %>% mutate(PostType = ifelse(is.na(Title), "Answer", "Question"))

# packages mining
# regex for finding the package
reg <- "(library|require)\\([\"\']?(.*?)[\"\']?\\)|([\\.a-zA-Z\\d]+)::|the [\"\'\`]?([a-zA-Z\\.\\d]+)[\"\'\`]? package"

r_packages <- r_posts %>%
  mutate(Packages = str_match_all(Body, reg),
         Package = map(Packages, ~ c(.[, 3:5]))) %>%
  select(-Packages, -Body) %>%
  unnest(Package) %>%
  filter(!is.na(Package), !Package %in% c("", "R", "r")) %>%
  mutate(Package = str_replace(Package, "'", "")) %>%
  distinct(Id, Package, .keep_all = TRUE)



##############################################
# Flipped bar chart - row chart with most mentioned packages
library(ggthemes)
row_chart <- r_packages %>%
  count(Package, sort = TRUE) %>%
  head(20) %>%
  mutate(Package = reorder(Package, n)) %>%
  ggplot(aes(Package, n)) +
  geom_col(fill = "#8080ff") +
  coord_flip()  +   
  theme_fivethirtyeight() +
  labs(x = "",
       y = "",
       title = "Most Mentioned R Packages",
       subtitle = "TOP 20 up to September 2017")
row_chart

##############################################
# Lollipop chart - most mentioned packages
lollipop <- r_packages %>%
  count(Package, sort = TRUE) %>%
  head(15) %>%
  mutate(Package = reorder(Package, -n)) %>%
  ggplot(aes(Package, n)) + 
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  geom_segment(aes(x=Package, 
                   xend=Package, 
                   y=n, 
                   yend=0)) + 
  labs(title="Most mentioned packages", 
       subtitle = "TOP 15 up to September 2017",
       x="", 
       y ="") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.8) + 
  theme_gdocs()     )
lollipop

##############################################
# Interactive time series - package by year 
year_totals <- r_packages %>% count(Year = year(CreationDate)) %>%
  rename(YearTotal = n)

package_by_year <- r_packages %>%
  transmute(Id = coalesce(ParentId, Id), Package, Year = year(CreationDate)) %>%
  distinct(Id, Package, Year) %>%
  count(Package, Year) %>%
  group_by(Package) %>%
  mutate(PackageTotal = sum(n)) %>%
  ungroup() %>%
  inner_join(year_totals, by = "Year")

package_year_data <- package_by_year %>%
  filter(PackageTotal >= 1200) %>%
  mutate(Percent = n / YearTotal) %>%
  complete(Package, Year, fill = list(Percent = 0)) %>%
  mutate(Package = reorder(Package, -PackageTotal, mean))

package_year_data <- package_year_data[ ,-3][ ,-3]
#View(package_year_data)
package_year_data$Package <- as.factor(package_year_data$Package)
referenceLines <- package_year_data
colnames(referenceLines)[1] <- "groupVar"

zp <- ggplot(package_year_data,
             aes(x = Year, y = Percent, color = Package))
zp <- zp + geom_line(data = referenceLines, # Plotting the "underlayer"
                     aes(x = Year, y = Percent, group = groupVar),
                     colour = "GRAY", alpha = 1/2, size = 1/2)
zp <- zp + geom_line(size = 1, show.legend = FALSE)  # Drawing the "overlayer"
zp <- zp + facet_wrap(~ Package)
zp <- zp + scale_x_continuous(breaks = seq(2009, 2017, 2))
zp <- zp + scale_y_continuous(labels = percent_format())

zp <- zp + theme_bw() + ggtitle("Package rise/descent in the years") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x=" ",y=" ") + 
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=32, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=22)) 

ggplotly()
##############################################

