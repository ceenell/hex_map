### make temp prediction hex map

## goal: plot the number of temperature observations at the national scale
## use hex bins to grid observation data

# prelims -----------------------------------------------------------------

## load libraries
library(rmapshaper);library(sf);library(maps)
library(scico)
library(tidyverse)
library(viridis)


# read in data ------------------------------------------------------------

## site data
### rows = sites; cols = site_id, site_type, latitude, longitude, source
sites_dat <- readRDS("data/all_sites.rds") %>%
  filter(!is.na(latitude)) # filter out rows where latitude is NA
str(sites_dat) # look at the structure of the data - 600,000+ rows and 5 columns

## temperature observations
daily_dat <- readRDS('data/daily_temperatures.rds') %>%
  filter(!is.na(date)) # drop rows without date
str(daily_dat) # this is a huge file

# prep data ---------------------------------------------------------------

## prep site data
## find the latitude and longitude coordinates for each site
lat_long_sites <- sites_dat %>%
  select(site_id, latitude, longitude) %>%
  distinct() # finds the unique combinations of variables listed in select() 
str(lat_long_sites) #drops around 2000 rows that were replicate sites

## prep observation data
## summarize temperature data by site, count the number of rows (observations) for each
sites_stats <- daily_dat %>%
  group_by(site_id) %>%
  summarize(obs_per_site = n()) 
str(sites_stats) # way more manageably sized

rm(daily_dat) # drop file from env to save memory

#  combine site and observation data
sites_stats <- sites_stats %>%
  left_join(lat_long_sites)
# should say Joining, by = "site_id" which is the key in both datasets used to join
class(sites_stats)

## convert to sf object (make it spatial)
sites_stats <- sites_stats %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
class(sites_stats)

glimpse(sites_stats) # see there is now a "geometry" variable that defines the coordinates

# make base map -----------------------------------------------------------

## need a base map to be able to spatially filter observations to the usa
## could also be useful in final plot

library(rmapshaper)

## read in usa state map, convert to sf
states_sf <-readRDS('data/state-map.rds')%>%
  st_as_sf()
## this map file is special because USGS ppl have repositioned AK, HI, and PR to be in same view

## simplify map shapes to make easier to work with
states_sf <- states_sf %>% ms_simplify()%>% st_buffer(0)
# st_buffer(0) is a magic trick that resolves geometry conflicts and makes sure everything is a polygon

## spatially summarize state polygons to make map outline of usa
usa_sf <- states_sf %>% group_by() %>% summarize()

# plot the map
ggplot() +
  geom_sf(data = states_sf, fill = "darkslateblue", color = "pink", size = .5) +
  geom_sf(data=usa_sf, fill=NA, color = "pink", size=1.5)+
  theme_void()
# stunnninggg


# make hex map ------------------------------------------------------------

## make a hex grid of the usa map. this is the base that will get populated with data
## if you change to n=c(100,100) that will change how many cells are on the map (100 rows and 100 columns)
# changing to square = TRUE will make square cells
site_grid <- usa_sf %>% 
  st_make_grid(n=c(100,100), what="polygons", square = FALSE)%>%
  st_as_sf()%>%
  mutate(area = st_area(.), geometry=x)

## add a number for a unique value for each cell/hex, will be used as key later to join with data
site_grid$hex<-as.character(seq.int(nrow(site_grid)))

## transform crs of sites to same as the basemap
sites_usa <- sites_stats %>% 
  st_transform(st_crs(usa_sf))

## intersect the hex grid with the site data
## this drops any sites outside of the grid area (usa)
## also joins site data to the hex map
site_int <- site_grid %>% st_intersection(sites_usa)

## aggregate site data to each hex cell
# summarize across rows to count how many observations are in each hex
sitey <- site_int %>%
  select(-x) %>%
  group_by(hex, geometry)%>%
  summarize(n_obs = length(obs_per_site), 
            n_sites = length(unique(site_id)))
glimpse(sitey) # should have the same number of rows as the hex grid


# plot hex map!! ----------------------------------------------------------

## starting with the combined hex/site data
sitey%>%
  ungroup()%>%
  ggplot()+
  geom_sf(aes(fill=n_obs), color="black", size=.2)+ # plot the hex bins
  scale_fill_viridis(option='plasma', trans='log',  breaks=c(1, 10, 25, 50, 100, 1000), direction=1)+
  theme_void()+
  theme(plot.background = element_rect(fill="black"), legend.text = element_text(color="white"))+ 
  guides(fill=guide_legend(title="Observations"))



