library(terra)
my_raster <- rast("D:/OUCRU/hfmd/data/landuse/vnm_pd_2020_1km.tif")
plot(my_raster, col = c("#419bdf","#387e49","#87b151","#8185c2",
                        "#e59635","#dfc359","#c4291c","#a39b92","#b39fe2"),
     type="class",
     plg=list(legend=c("Water", "Trees", "Grass", "Flooded vegetation", "Crops",
                       "Shrub and scrub", "Built", "Bare", "Snow and ice")))

plot(my_raster)
class(my_raster)

library(ggplot2)
library(dplyr)
library(sf)          # For shapefile handling
library(viridis)

# --- Load data ---
df <- read_csv("D:/OUCRU/hfmd/data/landuse/vnm_pd_2020_1km_ASCII_XYZ.csv")

df_sf <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)


# --- Check number of points ---

# --- Filter only Ho Chi Minh City polygons ---
# Check column names: GADM uses NAME_1 for province, NAME_2 for district
library(sf)
library(stringi)
map_path <- "D:/OUCRU/HCDC/project phân tích sero quận huyện/"
vn_qh <- st_read(dsn = file.path(map_path, "gadm41_VNM.gpkg"), layer = "ADM_ADM_2")

vn_qh1 <- vn_qh %>%
  clean_names() %>%     ## cho thành chữ thường
  filter(
    str_detect(
      name_1,
      "Hồ Chí Minh"
    )
  )
qhtp <- vn_qh1[-c(14,21),]

qhtp$geom[qhtp$varname_2 == "Thu Duc"] <- vn_qh1[c("21","24","14"),] %>%
  st_union()

qhtp <- qhtp %>% st_cast("MULTIPOLYGON")
qhtp$varname_2 <- stri_trans_general(qhtp$varname_2, "latin-ascii") %>%
  tolower() %>%
  str_remove("district") %>%
  trimws(which = "both")

qhtp$nl_name_2 <- c("BC","BTân","BT","CG","CC","GV",
                    "HM","NB","PN","1","10","11","12",
                    "3","4","5","6","7","8","TB",
                    "TP","TĐ")

df_hcmc <- st_intersection(df_sf, qhtp)

# --- Plot HCMC population density heatmap ---
ggplot() +
  geom_tile(data = as.data.frame(st_coordinates(df_hcmc)) %>%
              bind_cols(df_hcmc |> st_drop_geometry()),
            aes(x = X, y = Y, fill = Z)) +
  scale_fill_viridis_c(option = "viridis", name = "Population density\n(people per km²)") +
  geom_sf(data = qhtp, fill = NA, color = "black", size = 0.4) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Population Density Inside Ho Chi Minh City (2020, 1km Resolution)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12)


hcmc_density <- df_hcmc %>%
  st_drop_geometry() %>%
  group_by(name_2) %>%
  summarise(
    mean_density = mean(Z, na.rm = TRUE),
    median_density = median(Z, na.rm = TRUE),
    max_density = max(Z, na.rm = TRUE),
    n_points = n()
  ) %>%
  arrange(desc(mean_density)) %>%
  mutate(district = name_2 %>%
           str_remove("Quận|Huyện") %>%
           trimws(which = "both"))

hcmc_density$district %>% unique()
data_result$district %>% unique()
data_result %>%
  group_by(district) %>%
  summarise(mean_beta = mean(beta.beta)) %>%
  left_join(.,hcmc_density,by = join_by(district)) %>%
  mutate(log_den = log(mean_density)) %>%
  ggscatterstats(
    x = log_den,
    y = mean_beta,
    bf.message = FALSE,
    marginal = FALSE,
    label.var = name_2,
    xlab = "Log of mean human density",
    ylab = "Mean of beta"
  )
