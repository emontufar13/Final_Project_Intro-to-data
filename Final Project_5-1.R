library(jsonlite)
library(vroom)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(tidyverse)
library(readr)


#my chosen dataset include 4 Json files which will be read below

#Business Data :
business_data = stream_in(file("yelp_academic_dataset_business.json"))
#Tip Data :
tip_data = stream_in(file("yelp_academic_dataset_tip.json"))
#Checkin Data :
checkin_data = stream_in(file("yelp_academic_dataset_checkin.json"))

#Creating a summary table for restauarants with different amount of stars and average reviews 
summary_table = business_data |> 
  group_by(stars) |> 
  summarise(across( review_count,
                    list(
                      mean = ~mean(.x,na.rm =  TRUE),
                      median = ~median(.x,na.rm =  TRUE),
                      sd = ~sd(.x,na.rm =  TRUE),
                      min = ~min(.x,na.rm =  TRUE),
                      max = ~max(.x,na.rm =  TRUE)
                    ),
                    .names = "{.col}_{.fn}"
  ))

write.csv(summary_table,
          "Yelp_Stars_Review_Summary.csv", row.names = FALSE)

##Making charts for final presentation
##Rather than doing the while USA I chose to focus mostly on FL
comp_states = c("FL", "PA", "AZ")


#Below I cleaned the data to focus on the states I chose and defined cuisines 
business_cleaned = business_data |>
  filter(str_detect(categories, "Restaurants")) |>
  filter(state %in% comp_states) |>
  mutate(price = as.numeric(str_extract(attributes$RestaurantsPriceRange2, "[1-4]"))) |>
  mutate(cuisine = case_when(
    str_detect(categories, "Mexican") ~ "Mexican",
    str_detect(categories, "Italian") ~ "Italian",
    str_detect(categories, "Chinese") ~ "Chinese",
    str_detect(categories, "American") ~ "American",
    str_detect(categories, "Pizza") ~ "Pizza",
    TRUE ~ "Other"
     ))


# I cam back to modify code here to load more data in chunks to prevent crashes and get a better picture of the data

# For this particular data file, I can only load in chunks because of the size

target_ids = business_cleaned %>% pull(business_id)

con = file("yelp_academic_dataset_review.json", "r")
chunk_list = list()
i <- 1
chunk_size <- 100000


repeat{
  lines <- readLines(con, n = chunk_size)
  if(length(lines) == 0) break

chunk <- jsonlite::stream_in(textConnection(lines), verbose = FALSE)

filtered_chunk = chunk %>%
    filter(business_id %in% target_ids) |>
    mutate(date = as.Date(date)) |>
    filter(date >= as.Date("2021-01-01")) |>
    select(business_id, stars, date)

if(nrow(filtered_chunk)> 0) {
  chunk_list[[i]] <- filtered_chunk
  i <- i +1
  }
}

close(con)

review_data = bind_rows(chunk_list)


#Combing bigeest data set with business datatset
full_project_data = review_data |>
  inner_join(business_cleaned, by = "business_id")




#############################
#####Charts


#Chart 1
#shows if more expensive restaurants receive higher ratings
p1 = ggplot(business_cleaned |> filter(!is.na(price)),
       aes(x = as.factor(price), y = stars, fill = as.factor(price))
       )+
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "RdYlGn") +
  #Ensuring chart starts at 0 
  scale_y_continuous(limits = c(0,5), expand = expansion(mult = c(0,0.05)))+
  labs (title = "Price Range vs Star Rating", 
        x = "Price Range ($ to $$$$)", y = "Average Rating", 
        fill = "Price Level") +
    theme_minimal()


#Exporting chart as png 
ggsave("Price_v_Rating.png", p1, width = 8, height = 5, dpi = 300)


#Chart 2 

#Comparing average ratings across different cuisines in Florida
fl_cuisines = business_cleaned |>
  #I filtered out the other two states to focus on FL and the other cuisines
  filter(state == "FL", cuisine != "Other") |>
  group_by(cuisine) |>
  summarise(avg_stars = mean(stars, na.rm = TRUE))

p2 = ggplot(fl_cuisines, aes(x = reorder(cuisine, -avg_stars), y = avg_stars, fill = cuisine)) +
  geom_col() +
  coord_cartesian(ylim = c(0, 5)) +
  # Ensuring chart starts at 0 to read properly
  scale_y_continuous(limits = c(0,5), expand = expansion(mult = c(0,0.05)))+
  labs(title = "Average Rating by Cuisine in Florida", 
       x = "Cuisine Type", y = "Average Stars") +
  theme_minimal() +
  theme(legend.position = "none")

#Exporting chart as png 
ggsave("Cuisine_Comparison.png", p2, width = 8, height = 5, dpi = 300)


#Chart 3
#This chart is to show the realtionship between reviews to higher ratings

p3 = ggplot(full_project_data, aes(x = review_count, y = stars.x)) +
  geom_point(alpha = 0.3, color = "darkblue") +
  geom_smooth(method ="lm", color = "red") +
  #Ensuring chart starts at 0 
  scale_y_continuous(limits = c(0,5), expand = expansion(mult = c(0,0.05)))+
  scale_x_log10()+ # THis helps the chart be more consie and readable while keeping the same trend visible 
  labs( title = "Influence of Review Volume on Ratings", x = "Number of Reviews (Log Scale)", y = "Star Rating") +
theme_light()

#Exporting chart as png 
ggsave("Review_Volume.png", p3, width = 8, height = 5, dpi = 300)


#Chart 4 

fl_map = map_data("state", region = "florida")

#after running the code for the first time, the map was only showing data for tampa bay area 
#but this may be because I took a small sample and not all the data


#I kept this code in case if the datat entries are only for tampa bay area but needed to run all code first 
tampa_lon = c(-83.1, -82.1) 
tampa_lat = c(27.5, 28.3)

#below I coded a map that shows the Density of restaurants based on stars 
p4 = ggplot() + 
  geom_polygon(data = fl_map, aes( x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = business_cleaned |> filter(state == "FL"),
             aes(x = longitude, y = latitude, color = stars),
             alpha = 0.6, size = 1.5) +
  scale_color_gradient(low = "red", high = "forestgreen") +
  labs(title = "Geographic Distribution of Ratings in Florida", 
       subtitle = "Density indicates Urban areas vs. Rural areas",
       color = "Stars") +
  coord_fixed(1.3, #xlim = tampa_lon, ylim = tampa_lat
              ) +
  theme_void() 

#Exporting chart as png 
ggsave("Florida_map.png", p4, width = 8, height = 5, dpi = 300)



#Chart 5 
#comparing the avg raring by price between three states

state_comp = business_cleaned |>
  filter(!is.na(price)) |>
  group_by(state, price) |>
  summarise(avg_rating = mean(stars, na.rm = TRUE), .groups = 'drop')

p5 = ggplot(state_comp,aes(x =state, y = avg_rating, fill = as.factor(price))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_cartesian( ylim = c(0, 5)) +
  labs( title = "Aveargae Rating by STate and Price Level", 
        x = "State", y = "Average Star Rating", fill = "Price Range") +
  theme_minimal()

#Exporting chart as png 
ggsave("State_comparison.png", p5, width = 8, height = 5, dpi = 300)


