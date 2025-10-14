library(terra)
my_raster <- rast("D:/OUCRU/hfmd/data/landuse/vnm_pd_2020_1km.tif")

plot(my_raster)
class(my_raster)

library(ggplot2)
library(dplyr)
library(sf)          # For shapefile handling
library(viridis)
library(stringi)

# --- Load data ---
df <- read_csv("D:/OUCRU/hfmd/data/landuse/vnm_pd_2020_1km_ASCII_XYZ.csv")

df_sf <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)

ggplot(df, aes(x = X, y = Y, fill = Z)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", name = "Population density\n(people per km²)") +
  coord_fixed() +
  labs(
    title = "Population Density of Vietnam (2020, 1km Resolution)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal(base_size = 12)

# --- Check number of points ---

# --- Filter only Ho Chi Minh City polygons ---
# Check column names: GADM uses NAME_1 for province, NAME_2 for district

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
              bind_cols(df_hcmc |> st_drop_geometry()) %>%
              filter(Z > 1000),
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
  filter(Z > 1000) %>%
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
  # group_by(district) %>%
  # summarise(mean_beta = mean(beta.beta)) %>%
  left_join(.,hcmc_density,by = join_by(district)) %>%
  mutate(log_den = log(median_density)) %>%
  ggplot(aes(x = log_den,y=beta.beta))+
  geom_point()+
  theme_minimal()+
  labs(x = "Log of mean human density",
       y = "beta(t)")

data_result

library(terra)
my_raster <- rast("D:/OUCRU/hfmd/data/landuse/dynamic world/2023-01-01_2024-01-01_DYN_WORLD_V1.tif")
plot(my_raster, col = c("#419bdf","#387e49","#87b151","#8185c2",
                        "#e59635","#dfc359","#c4291c","#a39b92","#b39fe2"),
     type="class",
     plg=list(legend=c("Water", "Trees", "Grass", "Flooded vegetation", "Crops",
                       "Shrub and scrub", "Built", "Bare", "Snow and ice")))

r_df <- as.data.frame(my_raster, xy = TRUE, na.rm = TRUE)
names(r_df)[3] <- "class"

# --- Convert numeric codes to descriptive labels ---
class_labels <- c(
  "Water", "Trees", "Grass", "Flooded vegetation", "Crops",
  "Shrub and scrub", "Built", "Bare", "Snow and ice"
)

r_df$class <- factor(r_df$class, levels = 0:8, labels = class_labels)

# --- Convert to sf object for use with geom_sf ---
r_sf <- st_as_sf(r_df, coords = c("x", "y"), crs = 4326)

ggplot() +
  geom_sf(data = r_sf, aes(color = class), size = 0.2, alpha = 0.8) +
  scale_color_manual(
    values = c(
      "Water" = "#419bdf",
      "Trees" = "#387e49",
      "Grass" = "#87b151",
      "Flooded vegetation" = "#8185c2",
      "Crops" = "#e59635",
      "Shrub and scrub" = "#dfc359",
      "Built" = "#c4291c",
      "Bare" = "#a39b92",
      "Snow and ice" = "#b39fe2"
    )
  ) +
  labs(
    title = "Dynamic World Land Cover (2023–2024)",
    color = "Land cover class"
  ) +
  coord_sf() +
  theme_minimal(base_size = 12)



cut %>%
  mutate(cluster2 = case_when(
    district == "1" ~ "1",
    district %in% c("Tân Bình","Phú Nhuận","3")~"2",
    district %in% c("11","6","5")~"3",
    district %in% c("7")~"4",
    district %in% c("4")~"5",
    district %in% c("10")~"6",
    district %in% c("Cần Giờ")~"7"),
    district2 =  stri_trans_general(cut$district, "latin-ascii") %>%
      tolower() %>%
      str_remove("district") %>%
      trimws(which = "both")
  ) %>% replace(is.na(.),"8") %>%
  left_join(qhtp, ., by = join_by(varname_2 == district2)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(cluster2)),show.legend = T)+
  scale_fill_discrete(name = "Cluster",na.translate = FALSE)+
  geom_sf_text(aes(label = nl_name_2),size=2.5)+
  theme_void()


cut %>%
  mutate(cluster2 = case_when(
    district == "1" ~ "1",
    district %in% c("Tân Bình","Phú Nhuận","3")~"2",
    district %in% c("11","6","5")~"3",
    district %in% c("7")~"4",
    district %in% c("4")~"5",
    district %in% c("10")~"6",
    district %in% c("Cần Giờ")~"7"),
    district2 =  stri_trans_general(cut$district, "latin-ascii") %>%
      tolower() %>%
      str_remove("district") %>%
      trimws(which = "both")
  ) %>% replace(is.na(.),"8") %>%
  left_join(data_result,.,by = join_by(district == district2)) %>%
  left_join(.,hcmc_density,by = join_by(district)) %>%
  group_by(cluster2) %>%
  mutate(mean_dens_cluster = mean(mean_density)) %>%
  ungroup() %>%
  ggplot(aes(x = log(mean_dens_cluster),y=beta.beta))+
  geom_point()+
  theme_minimal()+
  labs(x = "Log of mean human density",
       y = "beta(t)")

data_result %>%
  # group_by(district) %>%
  # summarise(mean_beta = mean(beta.beta)) %>%
  left_join(.,hcmc_density,by = join_by(district)) %>%
  left_join(.,cut,by = join_by(district)) %>%
  group_by(cluster) %>%
  mutate(mean_dens_cluster = mean(mean_density)) %>%
  ungroup() %>%
  ggplot(aes(x = log(mean_dens_cluster),y=beta.beta))+
  geom_point()+
  theme_minimal()+
  labs(x = "Log of mean human density",
       y = "beta(t)")



