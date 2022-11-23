library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)

# The functions might be useful for A4
source("../source/a4-helpers.R")
get_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
View(get_data)

get_data <- get_data %>% drop_na()

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
texas_state <- get_data %>%
  filter(
    str_detect(state, "TX")
  )
View(texas_state)

texas_state <- texas_state %>% drop_na()

texas_state$percentage_black_men_prison <- (texas_state$black_female_prison_pop / texas_state$male_prison_pop) * 100

texas_state$percentage_aapi_men_prison <- (texas_state$aapi_male_prison_pop / texas_state$male_prison_pop) * 100

texas_state$percentage_latinx_men_prison <- (texas_state$latinx_male_prison_pop / texas_state$male_prison_pop) * 100

texas_state$total_minority_men_prison <- (texas_state$black_male_prison_pop + texas_state$latinx_male_prison_pop + texas_state$aapi_male_prison_pop + 
                                            texas_state$native_male_prison_pop + texas_state$other_race_male_prison_pop)

#Texas county with highest percentage of Black men
county_max_black_men_prison <- texas_state %>%
  group_by(county_name) %>%
  summarize(percentage_black_men_prison = max(percentage_black_men_prison)) %>%
  filter(percentage_black_men_prison == max(percentage_black_men_prison)) %>%
  pull(county_name)

#Texas county with highest percentage of AAPI men
county_max_aapi_men_prison <- texas_state %>%
  group_by(county_name) %>%
  summarize(percentage_aapi_men_prison = max(percentage_aapi_men_prison)) %>%
  filter(percentage_aapi_men_prison == max(percentage_aapi_men_prison)) %>%
  pull(county_name)

#Texas county with highest percentage of Latinx men
county_max_latinx_men_prison <- texas_state %>%
  group_by(county_name) %>%
  summarize(percentage_latinx_men_prison = max(percentage_latinx_men_prison)) %>%
  filter(percentage_latinx_men_prison == max(percentage_latinx_men_prison)) %>%
  pull(county_name)

#Texas county with highest percentage of minority men
county_max_total_minority <- texas_state %>%
  group_by(county_name) %>%
  summarize(total_minority_men_prison = max(total_minority_men_prison)) %>%
  filter(total_minority_men_prison == max(total_minority_men_prison)) %>%
  pull(county_name)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  get_data <- select(get_data, year, total_jail_pop)%>%
    group_by(year) %>%
    summarize(total = sum(total_jail_pop, na.rm = TRUE))
  return(get_data)
}
library(scales)
# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  pl <- ggplot(get_year_jail_pop(), aes(x=year, y=total)) +
    geom_bar(stat="identity") +
  scale_y_continuous(labels = comma) +
    labs(
      x= "Year",
      y= "Total Population",
      caption = "The Growth of U.S. Prison Population from 1970 to 2018",
    )
  return(pl)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
get_jail_pop_by_states <- function(states) {
  texas_state <- select(texas_state, year, total_pop) %>%
    group_by(year) %>%
    summarize(total = sum(total_pop, na.rm=TRUE))
  return(texas_state)  
}
  
plot_jail_pop_by_states <- function(states) {
  texas_state <- get_jail_pop_by_states(states)
  p <- ggplot(texas_state) +
    geom_smooth(aes(x=year, y=total, color=state), se=F) +
    labs(
      title= "Prison Population Growth in Texas",
      x= "Year",
      y= "Total Population",
      caption= "The United States Prison Population Growth in Texas "
    )
  return(p)
}
plot_jail_pop_by_states()

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>

white_black_jail_pop <- function() {
  texas_state <- group_by(get_data, year) %>%
    summarize(white_pop_total = max(white_jail_pop, na.rm = TRUE), black_pop_total = max(black_jail_pop, na.rm = TRUE)) %>%
    select(year, white_pop_total, black_pop_total)
  return(texas_state)
}

plot_white_black_jail_pop <- function(){
  plot <- ggplot(white_black_jail_pop()) +
    geom_smooth(aes(x = year, y = white_pop_total, color = "white"), se = F) +
    geom_smooth(aes(x = year, y = black_pop_total, color = "black"), se = F) +
    labs(
      title= "White vs Black Prison Population",
      x= "Year",
      y= "Jail Population",
      caption = "The Trends of White and Black Prisoners in U.S. Prison Population",
      color= "Ethnicity"
    )
  return(plot)
}


plot_white_black_jail_pop()

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas


  
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        
    axis.text = element_blank(),        
    axis.ticks = element_blank(),       
    axis.title = element_blank(),       
    plot.background = element_blank(),  
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank()      
  )


state_shape <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")


map_data <- state_shape %>%
  left_join(get_data, by = "fips")


heat_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "white",
    size = .1        
  ) +
  coord_map() + 
  scale_fill_continuous(limits = c(0, max(get_data$black_jail_pop)), 
                        low = "#aac2f6", high = "Red") +
  blank_theme +
  labs(
    title = "Incarcerations of Black Men By County", 
    fill = "Prison Incarcerations in Percentages"
  )
plot(heat_map)



#----------------------------------------------------------------------------#

## Load data frame ---- 


