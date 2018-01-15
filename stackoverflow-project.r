library(tidyverse)
library(stringr)
library(scales)
library(lubridate)
library(devtools)
library(ggplot2)
library(plotrix)
library(plotly) 
library("tm")   
# install_github("lchiffon/wordcloud2")
library(wordcloud2)
library(ggthemes)

getwd()
# working directory should be ending with stackoverflow-visualization/scripts

# DATA LOADING
questions <- read_csv("../data/Questions.csv")
answers <- read_csv("../data/Answers.csv")

# DATA AGGREGATION 
# binding rows of all posts and creating new column which defines the post type
# answers don't have Title, only questions do
r_posts <- bind_rows(questions, answers) %>% mutate(PostType = ifelse(is.na(Title), "Answer", "Question"))

# DATA MINING
# packages mining
# regex for finding the package
reg <- "(library|require)\\([\"\']?(.*?)[\"\']?\\)|([\\.a-zA-Z\\d]+)::|the [\"\'\`]?([a-zA-Z\\.\\d]+)[\"\'\`]? package"

r_packages <- r_posts %>%
  mutate(Packages = str_match_all(Body, reg),
         Package = map(Packages, ~ c(.[, 3:5]))) %>%
  select(-Packages, -Body) %>%
  unnest(Package) %>% # list-column -> each element of the list its own row.
  filter(!is.na(Package), !Package %in% c("", "R", "r")) %>%
  mutate(Package = str_replace(Package, "'", "")) %>%
  distinct(Id, Package, .keep_all = TRUE) # only unique rows


# VISUALIZATIONS
# Flipped bar chart - row chart with most mentioned packages
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
  labs(title="Most mentioned packages") + theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle=65, vjust=0.8)) 
lollipop


# Wordcloud
docs <- Corpus(VectorSource(r_packages$Package))  # data as corpus
# inspect(docs)
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud2(data = d, color = "random-light", backgroundColor = "black", minRotation = -pi/6, maxRotation = pi/6,
           rotateRatio = 1, ellipticity = 0.5, fontWeight = 400, fontFamily = "Comic Sans MS", minSize=2)


# Wordcloud 2
docs <- Corpus(VectorSource(r_packages$Package))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud2(data = d, color=rep_len(c("black","yellow", "pink"), nrow(d) ), backgroundColor = "white", minRotation = -pi/5, maxRotation = pi/5,
           rotateRatio = 1, fontWeight = 100, shape = 'star',fontFamily = "Courier", minSize=2)


# Wordcloud based on mask - R dark
figPath = "../r.png"
wordcloud2(d, figPath = figPath, color = "random-light", minRotation = pi/6, maxRotation = pi/6,
           rotateRatio = 1, backgroundColor = "black", size=3, minSize = 3)

# Wordcloud based on mask - R light
wordcloud2(d, figPath = figPath, color = "random-light", minRotation = pi/6, maxRotation = pi/6,
           rotateRatio = 1, backgroundColor = "white", size=3, minSize = 3)


# Pie chart
p <- r_packages %>%
  count(Package, sort = TRUE)
tail_p <- tail(p, n = nrow(p)-8)
sum(tail_p$n)

r <- rbind(head(p, n=8), c("others", sum(tail_p$n)))
r$n <- as.numeric(r$n)
r <- as.data.frame(r)

plot_ly(r, labels = ~Package, values = ~n, type = 'pie', marker = list(colors = c('rgb(245, 245, 245)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(217,140,86)', 'rgb(207,126,64)','rgb(198,109,53)', 'rgb(179,96,44)', 'rgb(167,84,37)')),
        textposition = 'outside',textinfo = 'label+percent') %>%
  layout(xaxis = list(title = 'Top 8 packages + others', showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


# 3D Pie chart
p <- r_packages %>%
  count(Package, sort = TRUE) %>%
  head(8) 
pie3D(p$n, labels=p$Package,explode=0.1, radius=.8, labelcex = 1.2,  start=0.8,
      main="8 most mentioned packages")


# Interactive time series - package by year 
year_totals <- r_packages %>% count(Year = year(CreationDate)) %>%
  rename(YearTotal = n)

package_by_year <- r_packages %>%
  transmute(Id = coalesce(ParentId, Id), Package, Year = year(CreationDate)) %>%  # delete old and create new variables
  distinct(Id, Package, Year) %>%
  count(Package, Year) %>%
  group_by(Package) %>%
  mutate(PackageTotal = sum(n)) %>%
  ungroup() %>%
  inner_join(year_totals, by = "Year")

package_year_data <- package_by_year %>%
  filter(PackageTotal >= 1200) %>%
  mutate(Percent = n / YearTotal) %>%
  complete(Package, Year, fill = list(Percent = 0)) %>% # in NA fills values of Percent 0
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
