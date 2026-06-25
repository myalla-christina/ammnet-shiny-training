# ══════════════════════════════════════════════════════
# AMMNet Shiny Training — Data Preparation
# Country:  Burkina Faso (real shapefile)
# Data:     Simulated incidence, rainfall, ITN coverage
# Period:   2020–2023
# Author:   Christina Myalla
# ══════════════════════════════════════════════════════

library(malariaAtlas)
library(sf)
library(tidyverse)
library(scales)

set.seed(42)

if (!dir.exists("data")) dir.create("data")

# ══════════════════════════════════════════════════════
# STEP 1: Real Burkina Faso shapefile
# ══════════════════════════════════════════════════════

cat("── Step 1: Loading shapefile ──\n")

bfa_admin <- malariaAtlas::getShp(
  country     = "Burkina Faso",
  admin_level = "admin2"
)

bfa_shp <- st_as_sf(bfa_admin) |>
  select(
    district = name_2,
    region   = name_1,
    geometry
  ) |>
  st_make_valid()

# confirm exact region names
cat("Region names from shapefile:\n")
bfa_shp |>
  st_drop_geometry() |>
  distinct(region) |>
  arrange(region) |>
  

saveRDS(bfa_shp, "data/bfa_districts.rds")
cat("Saved: data/bfa_districts.rds\n\n")

# ══════════════════════════════════════════════════════
# STEP 2: District baseline characteristics
# Region names match shapefile EXACTLY
# ══════════════════════════════════════════════════════

cat("── Step 2: Assigning district profiles ──\n")

# !! region names match shapefile exactly !!
region_baselines <- tribble(
  ~region,              ~base_incidence, ~base_rainfall,
  "Boucle du Mouhoun",  320,             750,
  "Cascades",           420,             1050,
  "Centre",             280,             780,
  "Centre-Est",         300,             820,
  "Centre-Nord",        180,             580,
  "Centre-Ouest",       310,             820,
  "Centre-Sud",         290,             800,
  "Est",                350,             900,
  "Hauts-Bassins",      390,             980,
  "Nord",               150,             520,
  "Plateau Central",    270,             760,   # no hyphen
  "Sahel",              120,             420,
  "Sud-Ouest",          400,             1050
)

districts <- bfa_shp |>
  st_drop_geometry() |>
  select(district, region) |>
  left_join(region_baselines, by = "region")

# check join worked — no NAs
cat("Districts with missing baselines:",
    sum(is.na(districts$base_incidence)), 
    "(expect 0)\n")

n_districts <- nrow(districts)

# add district-level variation
districts <- districts |>
  mutate(
    base_incidence = pmax(
      base_incidence + rnorm(n_districts, 0, 40), 50
    ),
    base_rainfall = pmax(
      base_rainfall + rnorm(n_districts, 0, 80), 200
    ),
    population = round(
      rlnorm(n_districts, meanlog = 10.5, sdlog = 0.6)
    )
  )

cat("Incidence range:", 
    round(range(districts$base_incidence)), "\n")
cat("Rainfall range:", 
    round(range(districts$base_rainfall)), "\n")

# ══════════════════════════════════════════════════════
# STEP 3: Seasonal weights
# Burkina Faso — Sahel wet season July–August
# Malaria peaks September–October (6-week lag)
# ══════════════════════════════════════════════════════

# named by month number as character for safe lookup
seasonal_rain_weights <- c(
  "1"  = 0.01, "2"  = 0.02, "3"  = 0.04,
  "4"  = 0.06, "5"  = 0.10, "6"  = 0.14,
  "7"  = 0.20, "8"  = 0.22, "9"  = 0.13,
  "10" = 0.06, "11" = 0.01, "12" = 0.01
)

seasonal_inc_weights <- c(
  "1"  = 0.02, "2"  = 0.02, "3"  = 0.03,
  "4"  = 0.04, "5"  = 0.07, "6"  = 0.10,
  "7"  = 0.14, "8"  = 0.18, "9"  = 0.20,
  "10" = 0.12, "11" = 0.05, "12" = 0.03
)

# slight declining trend — intervention scale-up
year_trend <- c(
  "2020" = 1.00,
  "2021" = 0.97,
  "2022" = 0.94,
  "2023" = 0.91
)

# ITN baselines by region
itn_baselines <- tribble(
  ~region,             ~itn_2020,
  "Boucle du Mouhoun", 52,
  "Cascades",          58,
  "Centre",            48,
  "Centre-Est",        51,
  "Centre-Nord",       44,
  "Centre-Ouest",      53,
  "Centre-Sud",        50,
  "Est",               55,
  "Hauts-Bassins",     60,
  "Nord",              42,
  "Plateau Central",   49,   # no hyphen
  "Sahel",             38,
  "Sud-Ouest",         57
)

# ══════════════════════════════════════════════════════
# STEP 4: Simulate monthly data
# ══════════════════════════════════════════════════════

cat("\n── Step 4: Simulating monthly data ──\n")

years  <- 2020:2023
months <- 1:12

malaria_data <- expand_grid(
  districts |>
    select(district, region,
           base_incidence,
           base_rainfall,
           population),
  year  = years,
  month = months
) |>
  mutate(
    # safe lookup using character month key
    month_key = as.character(month),
    yr_key    = as.character(year),
    
    # monthly rainfall
    rainfall_mm = round(
      base_rainfall *
        seasonal_rain_weights[month_key] *
        year_trend[yr_key] *
        runif(n(), 0.75, 1.25),
      1
    ),
    
    # monthly incidence per 1,000
    monthly_incidence = round(
      base_incidence *
        seasonal_inc_weights[month_key] *
        year_trend[yr_key] *
        12 *
        runif(n(), 0.85, 1.15),
      1
    ),
    
    # annual incidence per 1,000
    incidence = round(
      base_incidence *
        year_trend[yr_key] *
        runif(n(), 0.90, 1.10),
      1
    )
  ) |>
  # join ITN coverage
  left_join(itn_baselines, by = "region") |>
  mutate(
    itn_coverage = round(
      pmin(
        itn_2020 +
          (year - 2020) * 3 +
          rnorm(n(), 0, 4),
        95
      ), 1
    )
  ) |>
  select(
    district, region, year, month,
    rainfall_mm, incidence,
    monthly_incidence, itn_coverage,
    population
  ) |>
  arrange(district, year, month)

cat("Rows:", nrow(malaria_data), "\n")
cat("NAs in rainfall:", 
    sum(is.na(malaria_data$rainfall_mm)), "\n")
cat("NAs in incidence:", 
    sum(is.na(malaria_data$incidence)), "\n")
cat("NAs in itn_coverage:", 
    sum(is.na(malaria_data$itn_coverage)), "\n")

# ══════════════════════════════════════════════════════
# STEP 5: Annual summary
# ══════════════════════════════════════════════════════

cat("\n── Step 5: Annual summary ──\n")

annual_df <- malaria_data |>
  group_by(district, region, year, population) |>
  summarise(
    incidence      = round(mean(incidence,
                                na.rm = TRUE), 1),
    total_rainfall = round(sum(rainfall_mm,
                               na.rm = TRUE), 0),
    estimated_cases = round(
      sum(monthly_incidence * population / 1000,
          na.rm = TRUE)
    ),
    itn_coverage   = round(mean(itn_coverage,
                                na.rm = TRUE), 1),
    .groups        = "drop"
  )

cat("Annual rows:", nrow(annual_df), "\n")

# ══════════════════════════════════════════════════════
# STEP 6: Sense checks
# ══════════════════════════════════════════════════════

cat("\n── Step 6: Sense checks ──\n")

cat("\nMean incidence by region (2022):\n")
annual_df |>
  filter(year == 2022) |>
  group_by(region) |>
  summarise(
    mean_inc = round(mean(incidence, na.rm = TRUE), 0),
    .groups  = "drop"
  ) |>
  arrange(desc(mean_inc)) |>
  print(n = 13)

cat("\nRainfall seasonality (national mean mm):\n")
malaria_data |>
  group_by(month) |>
  summarise(
    mean_rain = round(mean(rainfall_mm, na.rm = TRUE), 0),
    .groups   = "drop"
  ) |>
  mutate(
    month_name = month.abb[month],
    bar        = strrep("█", pmax(mean_rain %/% 15, 0))
  ) |>
  select(month_name, mean_rain, bar) |>
  print(n = 12)

cat("\nITN coverage by year:\n")
annual_df |>
  group_by(year) |>
  summarise(
    mean_itn = round(mean(itn_coverage, na.rm = TRUE), 1),
    .groups  = "drop"
  ) |>
  print()

cat("\nIncidence trend by year:\n")
annual_df |>
  group_by(year) |>
  summarise(
    mean_inc = round(mean(incidence, na.rm = TRUE), 1),
    .groups  = "drop"
  ) |>
  print()

# ══════════════════════════════════════════════════════
# STEP 7: Visual checks
# ══════════════════════════════════════════════════════

cat("\n── Step 7: Visual checks ──\n")

# map 1 — incidence
p1 <- bfa_shp |>
  left_join(
    annual_df |> filter(year == 2022),
    by = c("district", "region")
  ) |>
  ggplot(aes(fill = incidence)) +
  geom_sf(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette   = "YlOrRd",
    direction = 1,
    name      = "Incidence\nper 1,000"
  ) +
  labs(
    title    = "Burkina Faso — Pf Incidence 2022",
    subtitle = "Simulated · higher burden in south and southwest"
  ) +
  theme_void(12)

print(p1)

# map 2 — ITN coverage
p2 <- bfa_shp |>
  left_join(
    annual_df |> filter(year == 2022),
    by = c("district", "region")
  ) |>
  ggplot(aes(fill = itn_coverage)) +
  geom_sf(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(
    palette   = "Greens",
    direction = 1,
    name      = "ITN\ncoverage (%)"
  ) +
  labs(
    title    = "Burkina Faso — ITN Coverage 2022",
    subtitle = "Simulated · coverage improving over time"
  ) +
  theme_void(12)

print(p2)

# plot 3 — rainfall + incidence seasonality
p3 <- malaria_data |>
  group_by(year, month) |>
  summarise(
    mean_rain = mean(rainfall_mm,       na.rm = TRUE),
    mean_inc  = mean(monthly_incidence, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(year = factor(year)) |>
  ggplot(aes(x = month)) +
  geom_col(
    aes(y = mean_rain / 5),
    fill  = "#378ADD",
    alpha = 0.4
  ) +
  geom_line(
    aes(y = mean_inc, colour = year, group = year),
    linewidth = 1
  ) +
  geom_point(
    aes(y = mean_inc, colour = year),
    size = 2
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_colour_brewer(palette = "Set2") +
  labs(
    title    = "Rainfall and malaria incidence seasonality",
    subtitle = "Bars = rainfall (mm/5) · Lines = monthly incidence per 1,000",
    x        = NULL,
    y        = "Value",
    colour   = "Year"
  ) +
  theme_minimal(12)

print(p3)

# ══════════════════════════════════════════════════════
# STEP 8: Save outputs
# ══════════════════════════════════════════════════════

cat("\n── Step 8: Saving ──\n")

saveRDS(malaria_data, "data/malaria_data.rds")
cat("Saved: data/malaria_data.rds\n")

saveRDS(annual_df,    "data/annual_summary.rds")
cat("Saved: data/annual_summary.rds\n")

saveRDS(bfa_shp,      "data/bfa_districts.rds")
cat("Saved: data/bfa_districts.rds\n")

cat("\n✓ Complete!\n")
cat("─────────────────────────────\n")
cat("Districts:   ", n_distinct(malaria_data$district), "\n")
cat("Regions:     ", n_distinct(malaria_data$region),   "\n")
cat("Years:        2020–2023\n")
cat("Monthly rows:", nrow(malaria_data),                "\n")
cat("Annual rows: ", nrow(annual_df),                   "\n")
cat("─────────────────────────────\n")
cat("Columns in malaria_data:\n")
cat(" ", paste(names(malaria_data), collapse = ", "), "\n")