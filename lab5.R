rm(list=ls())

library(tidyverse)
library(ca)
library(cluster)
library(MASS)

airbnb = read_csv("http://data.insideairbnb.com/spain/catalonia/barcelona/2022-01-09/data/listings.csv.gz") 

airbnb_sub = airbnb %>%
  dplyr::select(room_type,
                neighbourhood_group_cleansed, 
                accommodates, 
                price, 
                review_scores_rating) %>% 
  rename(hood = neighbourhood_group_cleansed) %>%
  mutate(room_type = factor(room_type),
         hood = factor(hood)) %>%
  mutate(price = as.numeric(gsub("\\$", "", price))) %>% 
  drop_na() %>%
  sample_n(10)


airbnb_dist <- daisy(airbnb_sub)

airbnb_dist_mds <- isoMDS(airbnb_dist)$points

colnames(airbnb_dist_mds) = c("x", "y")

airbnb_dist_mds %>% 
  as_tibble() %>%
  mutate(hood = airbnb_sub$hood) %>%
  ggplot(aes(x=x, y=y, fill=hood)) + geom_point(pch=21, size=3)

ggplot(airbnb)

