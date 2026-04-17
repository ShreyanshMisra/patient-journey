suppressPackageStartupMessages({
  library(tidycensus)
  library(dplyr)
  library(readr)
})

# API Key
api_key <- Sys.getenv("CENSUS_API_KEY")
if (nzchar(api_key)) {
  census_api_key(api_key, overwrite = TRUE)
}

# Variables to Pull
vars <- c(
  median_household_income = "B19013_001",
  # Vehicle access (B25044 — tenure by vehicles available)
  hu_total                = "B25044_001",  # total occupied housing units
  hu_owner_no_vehicle     = "B25044_003",  # owner-occupied, no vehicle
  hu_renter_no_vehicle    = "B25044_010",  # renter-occupied, no vehicle
  # Insurance (B27010)
  ins_total               = "B27010_001",  # pop for whom insurance is determined
  uninsured_under_19      = "B27010_017",
  uninsured_19_34         = "B27010_033",
  uninsured_35_64         = "B27010_050",
  uninsured_65_plus       = "B27010_066",
  # Poverty (C17002)
  pov_total               = "C17002_001",
  pov_below_050           = "C17002_002",
  pov_050_099             = "C17002_003"
)

# 5-year 2022 for Kansas at block-group level
cat("Pulling ACS 2022 5-year data for Kansas block groups...\n")
acs_raw <- get_acs(
  geography = "block group",
  variables = vars,
  state     = "KS",
  year      = 2022,
  survey    = "acs5",
  output    = "wide"
)

cat("Rows pulled:", nrow(acs_raw), "\n")

# Derived columns
acs <- acs_raw %>%
  mutate(
    pct_no_vehicle = ifelse(
      hu_totalE > 0,
      (hu_owner_no_vehicleE + hu_renter_no_vehicleE) / hu_totalE,
      NA_real_
    ),
    pct_uninsured  = ifelse(
      ins_totalE > 0,
      (uninsured_under_19E + uninsured_19_34E +
         uninsured_35_64E + uninsured_65_plusE) / ins_totalE,
      NA_real_
    ),
    pct_poverty    = ifelse(
      pov_totalE > 0,
      (pov_below_050E + pov_050_099E) / pov_totalE,
      NA_real_
    )
  ) %>%
  rename(
    CensusBlockGroupFipsCode = GEOID,
    median_household_income  = median_household_incomeE
  ) %>%
  select(
    CensusBlockGroupFipsCode,
    median_household_income,
    pct_no_vehicle,
    pct_uninsured,
    pct_poverty
  )

cat("Rows after processing:", nrow(acs), "\n")
cat("Summary:\n")
print(summary(acs))

# Write
dir.create("dataset", showWarnings = FALSE)
out_path <- "dataset/acs_block_group.csv"
write_csv(acs, out_path)
cat("Wrote:", out_path, "\n")
