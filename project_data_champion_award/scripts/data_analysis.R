# -------------------------------------------------------------------
# -------------------------------------------------------------------
# Spatial analysis of tobacco outlets (shops & vending machines) around schools
# For a step-by-step explanation, see the PDF "Tutorial_GIS.pdf"
# -------------------------------------------------------------------
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Load packages (install if needed)
# -------------------------------------------------------------------

# install.packages("here", dependencies = TRUE) # install if needed by removing the "#" at the beginning
library(here)     # creates relative paths for the R project to access the data
library(sf)       # encodes and analyzes spatial vector data
library(readr)    # reads delimited and CSV files
library(dplyr)    # edits data
library(tidyr)    # edits data
library(tmap)     # creates maps
library(osmdata)  # gets data from OpenStreetMap
library(ggplot2)  # creates graphics and illustrations
library(stringr)  # converts strings
library(scales)   # defines and formats numeric/categorial scales for graphics
library(glmmTMB)  # runs generalized linear mixed models

# -------------------------------------------------------------------
# File paths & basic checks
# -------------------------------------------------------------------

file_school_data  <- here("project_data_champion_award", 
                          "sources",
                          "school_data",
                          "schools.shp")
file_machine_data <- here("project_data_champion_award",
                          "sources", 
                          "cig_vending_machines.csv")
file_shop_data    <- here("project_data_champion_award",
                          "sources", 
                          "cig_shop_data.csv")

stopifnot(file.exists(file_school_data), file.exists(file_shop_data))

# -------------------------------------------------------------------
# Get vending machine locations from OpenStreetMap (OSM)
# -------------------------------------------------------------------

# City bbox (lon_min, lat_min, lon_max, lat_max)
city_bbox <- c(8.48, 53.01, 8.98, 53.23) # Bremens city bbox

# Build and run Overpass query
query <- opq(bbox = city_bbox) %>%
  add_osm_feature(key = "amenity", value = "vending_machine") %>%
  add_osm_feature(key = "vending",  value = "cigarettes", value_exact = FALSE)

machine_query  <- osmdata_sf(query)
machine_points <- machine_query$osm_points

coords <- st_coordinates(machine_points)
machine <- data.frame(
  id  = machine_points$osm_id,
  lon = round(coords[, 1], 5),
  lat = round(coords[, 2], 5)
)

# Save as CSV
write.table(
  machine,
  file = file_machine_data,
  sep = ";",
  dec = ".",
  row.names = FALSE
)

# -------------------------------------------------------------------
# Read data & set coordinate reference systems (CRS):
# - Transform data using ETRS89 / UTM zone 32N (EPSG:25832) for metric operations
# - Transform data using WGS84 (EPSG:4326) for creating maps
# -------------------------------------------------------------------

# Transform school data (from shapefile)
school_raw <- st_read(file_school_data, quiet = TRUE) %>%
  rename(school_type = schulart_2) # change variable name to English
school_met <- school_raw %>% 
  mutate(school_id = row_number()) %>%
  rename(school_name = nam,
         school_neighborhood = ortsteilna)

# Quick check: is school_met in the correct crs?
if (is.na(st_crs(school_met)$epsg) || st_crs(school_met)$epsg != 25832) {
  school_met <- st_transform(school_met, 25832)}

school_map <- st_transform(school_met, 4326)

# Transform machine data (from CSV file)
machine_raw <- read_delim(file_machine_data, delim = ";",
                          locale = locale(decimal_mark = "."),
                          show_col_types = FALSE)
machine_map <- st_as_sf(machine_raw, coords = c("lon","lat"), crs = 4326)
machine_met <- st_transform(machine_map, 25832)

# Transform shop data (from CSV file)
shop_raw <- read_csv(file_shop_data, show_col_types = FALSE) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))
shop_map <- st_as_sf(shop_raw, coords = c("lon","lat"), crs = 4326)
shop_met <- st_transform(shop_map, 25832)

# -------------------------------------------------------------------
# Create interactive maps (mode: "view")
# -------------------------------------------------------------------

tmap_mode("view")

tm_basemap("OpenStreetMap") +
  tm_shape(machine_map) + tm_dots(fill = "red",   size = 0.4) +
  tm_shape(shop_map)    + tm_dots(fill = "blue",  size = 0.4) +
  tm_shape(school_map)  + tm_dots(fill = "black", size = 0.4) +
  tm_layout(title = "Schools in black, vending machines in red and shops in blue")

# ------------------------------------------------------------------------------
# Create 500 m buffers around schools (metric CRS) & filter outlets inside
# ------------------------------------------------------------------------------

# Buffers in meters
school_buffer_500 <- st_buffer(school_met, 500)

# Intersect outlets with the buffers, still in meters
machine_500_met <- st_filter(machine_met, school_buffer_500)
shop_500_met    <- st_filter(shop_met,    school_buffer_500)

# Transform everything to WGS84 for web mapping
school_buffer_500_map <- st_transform(school_buffer_500, 4326)
machine_500_map       <- st_transform(machine_500_met,   4326)
shop_500_map          <- st_transform(shop_500_met,      4326)

# Create the map with the buffers
tm_basemap("OpenStreetMap") +
  tm_shape(school_buffer_500_map) + tm_borders(col = "black") +
  tm_shape(machine_500_map) + tm_dots(fill = "red",  size = 0.4) +
  tm_shape(shop_500_map) + tm_dots(fill = "blue", size = 0.4) +
  tm_shape(school_map) + tm_dots(fill = "black", size = 0.4) +
  tm_layout(title = "Tobacco outlets within 500 meters of schools")

# -------------------------------------------------------------------
# Descriptive summaries
# -------------------------------------------------------------------

# Combine machines and shops into one outlet layer
machines_met2 <- machine_met %>%
  mutate(
    outlet_id     = as.character(id),
    type          = "machine",
    main_category = "machine"
  )

shops_met2 <- shop_met %>%
  mutate(outlet_id = as.character(placeId),
         type      = "shop")

outlets_met <- bind_rows(machines_met2, shops_met2) %>%
  select(outlet_id, type, main_category, geometry)

city_total <- outlets_met %>%
  st_drop_geometry() %>%
  count(type, name = "n_total_type") %>%
  arrange(desc(n_total_type))
print(city_total)

# Outlets within 500 meters of schools
buffer_outlets <- st_join(
  school_buffer_500 %>% select(school_id, school_type, school_name,
                               school_neighborhood),
  outlets_met,
  join = st_intersects,
  left = TRUE
)

# Counts per school
counts_per_school <- buffer_outlets %>%
  st_drop_geometry() %>%
  group_by(school_id, school_type, school_name, school_neighborhood) %>%
  summarise(
    n_machines = sum(type == "machine", na.rm = TRUE),
    n_shops    = sum(type == "shop",    na.rm = TRUE),
    n_total    = n_machines + n_shops,
    .groups    = "drop"
  ) %>%
  arrange(school_id)

head(counts_per_school)

write_csv(counts_per_school, here("project_data_champion_award",
                                  "outputs",
                                  "counts_per_school.csv"))

# Summary by school type
stats_by_schooltype <- counts_per_school %>%
  group_by(school_type) %>%
  summarise(
    mean_total   = round(mean(n_total,  na.rm = TRUE),2),
    sd_total     = round(sd(n_total,    na.rm = TRUE),2),
    min_total    = min(n_total,   na.rm = TRUE),
    p25_total    = stats::quantile(n_total, 0.25, na.rm = TRUE),
    median_total = stats::quantile(n_total, 0.50, na.rm = TRUE),
    p75_total    = stats::quantile(n_total, 0.75, na.rm = TRUE),
    max_total    = max(n_total,   na.rm = TRUE),
    n_schools    = n(),
    .groups      = "drop"
  ) %>%
  arrange(school_type)

head(stats_by_schooltype)

stats_split <- counts_per_school %>%
  group_by(school_type) %>%
  summarise(
    mean_machines = round(mean(n_machines, na.rm = TRUE),2),
    sd_machines   = round(sd(n_machines,   na.rm = TRUE),2),
    mean_shops    = round(mean(n_shops,    na.rm = TRUE),2),
    sd_shops      = round(sd(n_shops,      na.rm = TRUE),2),
    .groups       = "drop"
  )

head(stats_split)

# -------------------------------------------------------------------
# Plot: average machines vs. shops per school type
# -------------------------------------------------------------------

plot_split <- stats_split %>%
  pivot_longer(c(mean_machines, mean_shops),
               names_to  = "type",
               values_to = "mean_count"
  ) %>%
  mutate(type = recode(type, 
                       "mean_machines" = "machines",
                       "mean_shops"    = "shops"))

plot_split %>%
  filter(school_type %in% c("Grundschule", "Gymnasium", "Oberschule")) %>%
  mutate(
    school_type_en = recode(school_type,
                            "Grundschule" = "Primary schools",
                            "Gymnasium"   = "Academic secondary schools (Gymnasium)",
                            "Oberschule"  = "Comprehensive secondary schools (Gesamtschule)")
  ) %>%
  ggplot(aes(x = school_type_en, y = mean_count, fill = type)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(mean_count, 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.2, size = 5.5) +
  scale_fill_manual(
    values = c("shops" = "blue", "machines" = "red")) +
  guides(fill = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Average number of tobacco outlets per school type",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12)

# -------------------------------------------------------------------
# Multilevel analysis
# Goal: Is the number of nearby outlets associated with population density
#       and a social index? Random intercept for neighborhood.
# -------------------------------------------------------------------

# Read the contextual data and join
pop_density_data  <- here("project_data_champion_award",
                          "sources", 
                          "pop_density_data.csv")
social_index_data <- here("project_data_champion_award",
                          "sources", 
                          "social_index_data.csv")

pop_density <- read_csv(pop_density_data)
soc_index   <- read_csv(social_index_data)

basis_analysis <- counts_per_school %>%
  left_join(pop_density, by = c("school_neighborhood" = "area2")) %>%
  left_join(soc_index,   by = c("school_neighborhood" = "area"))

stopifnot(all(c("n_total", "pop_index_2023", "social_index") %in% names(basis_analysis)))

# Standardise predictors
basis_analysis <- basis_analysis %>%
  mutate(
    pop_index_z    = as.numeric(scale(pop_index_2023)),
    social_index_z = as.numeric(scale(social_index))
  )

# Check dispersion

## All schools
mean_all  <- mean(basis_analysis$n_total, na.rm = TRUE)
var_all   <- var(basis_analysis$n_total,  na.rm = TRUE)
ratio_all <- var_all / mean_all
print(mean_all); print(var_all); print(ratio_all)

## Primary schools only
data_prim  <- filter(basis_analysis, school_type %in% c("Grundschule", "Private Grundschule"))
mean_prim  <- mean(data_prim$n_total, na.rm = TRUE)
var_prim   <- var(data_prim$n_total,  na.rm = TRUE)
ratio_prim <- var_prim / mean_prim
print(mean_prim); print(var_prim); print(ratio_prim)

# Fit the multilevel model

## All schools
model_nb <- glmmTMB(
  n_total ~ social_index_z + pop_index_z + (1 | school_neighborhood),
  data   = basis_analysis,
  family = nbinom2()
)
summary(model_nb)

## Primary schools only
model_prim <- glmmTMB(
  n_total ~ social_index_z + pop_index_z + (1 | school_neighborhood),
  data   = data_prim,
  family = nbinom2()
)
summary(model_prim)
