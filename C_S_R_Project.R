library(tidyverse)
library(rvest)
library(stringr)

#cars<- read_html("https://www.cargurus.com/Cars/spt_cheap_cars-Boston_L15690?page=1")

cars1 <- read_html("https://www.cargurus.com/Cars/spt_cheap_cars-Boston_L15690?page=1")

html_children(cars)

v1 <- html_node(cars,"body")

html_children(v1)

car_names <- cars1 %>%
  html_nodes(".cg-dealFinder-result-model") %>%
  html_text() %>%
  str_replace_all("\n","") %>%
  str_extract("(.*),") %>%
  str_replace_all("Used\\sCars(.*)","") %>%
  str_replace_all("^\\s+","") %>%
  str_replace_all("\\s+$","") %>%
  str_replace_all("\\$","") 
#  str_extract_all("^[0-9]+\\s") %>%
#  str_replace_all("\\s$","")
  
car_names

car_years <- cars1 %>%
    html_nodes(".cg-dealFinder-result-model") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract("(.*),") %>%
    str_replace_all("Used\\sCars(.*)","") %>%
    str_replace_all("^\\s+","") %>%
    str_replace_all("\\s+$","") %>%
    str_replace_all("\\$","") %>%
    str_extract_all("^[0-9]+\\s") %>%
    str_replace_all("\\s$","")

car_years

car_make <- cars1 %>%
  html_nodes(".cg-dealFinder-result-model") %>%
  html_text() %>%
  str_replace_all("\n","") %>%
  str_extract("(.*),") %>%
  str_replace_all("Used\\sCars(.*)","") %>%
  str_replace_all("^\\s+","") %>%
  str_replace_all("\\s+$","") %>%
  str_replace_all("\\$","") %>%
  str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s") %>%
  str_replace_all("^[0-9]+\\s","") %>%
  str_replace_all("\\s$","")

car_make

car_model <- cars1 %>%
  html_nodes(".cg-dealFinder-result-model") %>%
  html_text() %>%
  str_replace_all("\n","") %>%
  str_extract("(.*),") %>%
  str_replace_all("Used\\sCars(.*)","") %>%
  str_replace_all("^\\s+","") %>%
  str_replace_all("\\s+$","") %>%
  str_replace_all("\\$","") %>%
  str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s(.*)") %>%
  str_replace_all("^[0-9]+\\s[A-Za-z-]+","") %>%
  str_replace_all("\\s$","")

car_model


car_price <- cars1 %>%
  html_nodes(".cg-dealFinder-result-stats") %>%
  html_text() %>%
  str_replace_all("\n","") %>%
  str_extract_all("\\$[0-9,]+") %>%
  str_replace_all("\\$","") %>%
  str_replace_all(",","") %>%
  as.double()

car_price

car_mileage <- cars1 %>%
  html_nodes(".cg-dealFinder-result-stats") %>%
  html_text() %>%
  str_replace_all("\n","") %>%
  str_extract_all("[(M|m)ileage:]+\\s[0-9,]+") %>%
  str_replace_all("[(M|m)ileage:]+\\s","") %>%
  str_replace_all(",","") %>%
  as.double()

car_mileage



car_price <- cars %>%
  html_nodes(".cg-dealFinder-result-stats") %>%
  html_text(trim = TRUE) %>%
  str_replace_all("\n","") %>%
  str_extract_all("\\$[0-9,]+")%>%
  str_replace_all("\\$","")

car_price

v1 <- tibble(
  car_years = cars1 %>%
    html_nodes(".cg-dealFinder-result-model") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract("(.*),") %>%
    str_replace_all("Used\\sCars(.*)","") %>%
    str_replace_all("^\\s+","") %>%
    str_replace_all("\\s+$","") %>%
    str_replace_all("\\$","") %>%
    str_extract_all("^[0-9]+\\s") %>%
    str_replace_all("\\s$",""),
  
  car_make = cars1 %>%
    html_nodes(".cg-dealFinder-result-model") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract("(.*),") %>%
    str_replace_all("Used\\sCars(.*)","") %>%
    str_replace_all("^\\s+","") %>%
    str_replace_all("\\s+$","") %>%
    str_replace_all("\\$","") %>%
    str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s") %>%
    str_replace_all("^[0-9]+\\s","") %>%
    str_replace_all("\\s$",""),
  
  car_model = cars1 %>%
    html_nodes(".cg-dealFinder-result-model") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract("(.*),") %>%
    str_replace_all("Used\\sCars(.*)","") %>%
    str_replace_all("^\\s+","") %>%
    str_replace_all("\\s+$","") %>%
    str_replace_all("\\$","") %>%
    str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s(.*)") %>%
    str_replace_all("^[0-9]+\\s[A-Za-z-]+","") %>%
    str_replace_all("\\s$",""), 
  
  car_price = cars1 %>%
    html_nodes(".cg-dealFinder-result-stats") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract_all("\\$[0-9,]+") %>%
    str_replace_all("\\$",""),
  
  car_mileage = cars1 %>%
    html_nodes(".cg-dealFinder-result-stats") %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_extract_all("[(M|m)ileage:]+\\s[0-9,]+") %>%
    str_replace_all("[(M|m)ileage:]+\\s","")
  
)


View(v1)

# Scrapping cars information on a single page.

get_one_page_information <- function(page){
  cars_url<- "https://www.cargurus.com/Cars/spt_cheap_cars-Boston_L15690?page=%s"
  cars_url <- sprintf(cars_url, page)
  cars <- read_html(cars_url)
  
  v1 <- tibble(
    car_years = cars %>%
      html_nodes(".cg-dealFinder-result-model") %>%
      html_text() %>%
      str_replace_all("\n","") %>%
      str_extract("(.*),") %>%
      str_replace_all("Used\\sCars(.*)","") %>%
      str_replace_all("^\\s+","") %>%
      str_replace_all("\\s+$","") %>%
      str_replace_all("\\$","") %>%
      str_extract_all("^[0-9]+\\s") %>%
      str_replace_all("\\s$",""),
    
    car_make = cars %>%
      html_nodes(".cg-dealFinder-result-model") %>%
      html_text() %>%
      str_replace_all("\n","") %>%
      str_extract("(.*),") %>%
      str_replace_all("Used\\sCars(.*)","") %>%
      str_replace_all("^\\s+","") %>%
      str_replace_all("\\s+$","") %>%
      str_replace_all("\\$","") %>%
      str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s") %>%
      str_replace_all("^[0-9]+\\s","") %>%
      str_replace_all("\\s$",""),
    
    car_model = cars %>%
      html_nodes(".cg-dealFinder-result-model") %>%
      html_text() %>%
      str_replace_all("\n","") %>%
      str_extract("(.*),") %>%
      str_replace_all("Used\\sCars(.*)","") %>%
      str_replace_all("^\\s+","") %>%
      str_replace_all("\\s+$","") %>%
      str_replace_all("\\$","") %>%
      str_extract_all("^[0-9]+\\s[A-Za-z-]+\\s(.*)") %>%
      str_replace_all("^[0-9]+\\s[A-Za-z-]+","") %>%
      str_replace_all("\\s$",""), 
    
    car_price = cars %>%
      html_nodes(".cg-dealFinder-result-stats") %>%
      html_text() %>%
      str_replace_all("\n","") %>%
      str_extract_all("\\$[0-9,]+") %>%
      str_replace_all("\\$","") %>%
      str_replace_all(",","") %>%
      as.double(),
    
    car_mileage = cars %>%
      html_nodes(".cg-dealFinder-result-stats") %>%
      html_text() %>%
      str_replace_all("\n","") %>%
      str_extract_all("[(M|m)ileage:]+\\s[0-9,]+") %>%
      str_replace_all("[(M|m)ileage:]+\\s","") %>%
      str_replace_all(",","") %>%
      as.double()
    
  )
  
}

get_one_page_information(page = 10)

get_all_car_results <- function(page,pages,start){
  multiple_page <- data.frame()
  for ( page in start:pages){
    single_page <- get_one_page_information(page)
    multiple_page = rbind(single_page,multiple_page)
    Sys.sleep(0.5)
  }
  
  multiple_page  
}

#v2 <- get_all_car_results(page = 1, pages = 20, start = 1)

v3 <- get_all_car_results(page = 1,pages = 50, start = 1)



View(v3)

# cars with highest listings.
ggplot(data = v3)+
  geom_bar(mapping = aes(x = car_years))

# cars with highest listings.
ggplot(data = v3)+
  geom_bar(mapping = aes(x = car_make))+
  coord_flip()
 
# here we can see that the car price and car mileage are inversely proportional.

ggplot(data = v3)+
  geom_smooth(mapping = aes(y = car_price,  x = car_mileage), se = FALSE)

# cars average price

ggplot(data = v3)+
  geom_boxplot(mapping = aes(x = car_make, y = car_price))+
  coord_flip()

getwd()

write_csv(v3,"Cars_scraped.csv")



