# ==============================================================================
# 01_clean_data.R (DUMMY VERSION)
# Purpose: Generate dummy Trackman data, clean it, add calculation flags,
#          and save a compressed R data file for the dashboard to use.
# ==============================================================================

library(tidyverse)

# 1. GENERATE DUMMY DATA (Simulating the CSV)
# -------------------------------------------
set.seed(101) # Ensures we get the same "random" numbers every time
N <- 150      # Number of pitches to simulate

# We create a tibble that looks exactly like your raw CSV import
trackman_raw <- tibble(
  PitchNo = 1:N,
  Date = rep("2025-01-30", N),
  Pitcher = "Pitcher, Test",
  PitcherThrows = "Right",
  PitcherTeam = "Banyan",
  BatterSide = sample(c("Right", "Left"), N, replace = TRUE),
  BatterTeam = "Opponent",
  Inning = sort(rep(1:9, length.out = N)),
  Outs = sample(0:2, N, replace = TRUE),
  Balls = sample(0:3, N, replace = TRUE),
  Strikes = sample(0:2, N, replace = TRUE),
  
  # Pitch Types
  TaggedPitchType = sample(c("Fastball", "Cutter", "Curveball", "ChangeUp", "Slider"), N, replace = TRUE, prob = c(0.4, 0.2, 0.15, 0.15, 0.1)),
  AutoPitchType = "Undefined", # Usually not needed if Tagged is present
  
  # Calls and Results
  PitchCall = sample(c("StrikeCalled", "BallCalled", "InPlay", "Foul", "StrikeSwinging"), N, replace = TRUE),
  KorBB = sample(c("Strikeout", "Walk", "Undefined"), N, replace = TRUE),
  TaggedHitType = sample(c("GroundBall", "FlyBall", "LineDrive", "Popup", "Undefined"), N, replace = TRUE),
  AutoHitType = "Undefined",
  PlayResult = "Undefined",
  OutsOnPlay = 0,
  RunsScored = 0,
  
  # Physics Data
  RelSpeed = rnorm(N, 92, 3),
  VertRelAngle = rnorm(N, -2, 1),
  HorzRelAngle = rnorm(N, 2, 1),
  SpinRate = rnorm(N, 2300, 150),
  SpinAxis = runif(N, 180, 240),
  Tilt = sample(c("11:00", "11:30", "12:00", "1:00"), N, replace = TRUE),
  RelHeight = rnorm(N, 6.0, 0.2),
  RelSide = rnorm(N, 1.5, 0.2),
  Extension = rnorm(N, 6.2, 0.3),
  InducedVertBreak = rnorm(N, 16, 5),
  HorzBreak = rnorm(N, 10, 5),
  PlateLocHeight = rnorm(N, 2.5, 1.0),
  PlateLocSide = rnorm(N, 0, 1.0),
  VertApprAngle = rnorm(N, -6, 1),
  HorzApprAngle = rnorm(N, 2, 1),
  
  # Batted Ball Data (Only valid if InPlay, but we'll fill random for all for simplicity)
  ExitSpeed = rnorm(N, 88, 12),
  Angle = rnorm(N, 15, 20),
  Direction = runif(N, -45, 45),
  Distance = rnorm(N, 200, 100)
)

# 2. CLEAN & PROCESS (This is the real logic you will use later)
# -----------------------------------------------------------
trackman_clean <- trackman_raw %>%
  select(
    # Select only the columns we need for the dashboard
    PitchNo, Date, Pitcher, PitcherThrows, PitcherTeam, 
    BatterSide, BatterTeam, Inning, Outs, Balls, Strikes,
    TaggedPitchType, PitchCall, TaggedHitType, 
    RelSpeed, SpinRate, Tilt, RelHeight, RelSide, Extension, 
    InducedVertBreak, HorzBreak, PlateLocHeight, PlateLocSide,
    ExitSpeed, Angle
  ) %>%
  
  # 3. ADD CALCULATION FLAGS
  # ------------------------
  mutate(
    # Ensure Date is actually a Date object
    Date = as.Date(Date, format = "%Y-%m-%d"), 
    
    # Create Flags for metrics
    isStrike = PitchCall %in% c("StrikeCalled", "Foul", "StrikeSwinging", "InPlay"),
    isWhiff  = PitchCall == "StrikeSwinging",
    isSwing  = PitchCall %in% c("Foul", "StrikeSwinging", "InPlay"),
    isInPlay = PitchCall == "InPlay",
    
    # Create a Count string (e.g., "0-0", "3-2")
    CountStr = paste(Balls, Strikes, sep = "-")
  ) %>%
  
  # Standardize "3-2" to "Full"
  mutate(CountStr = ifelse(CountStr == "3-2", "Full", CountStr))

# 4. SAVE CLEAN DATA
# ------------------
# Create 'data' folder if it doesn't exist
if(!dir.exists("data")) dir.create("data")

saveRDS(trackman_clean, "data/trackman_cleaned.rds")

message("-------------------------------------------------------")
message("SUCCESS! Dummy data generated and cleaned.")
message("File saved to: data/trackman_cleaned.rds")
message("You can now run Script 2 (Dashboard) to see the report.")
message("-------------------------------------------------------")