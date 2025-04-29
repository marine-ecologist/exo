library(tidyverse)
library(janitor)
library(ggplot2)


exo <- read.csv("/Users/rof011/Desktop/KorEXO Measurement File Export - 031824 030139.csv") |>
  slice(8:n()) |> # cut the dumb headers
  mutate(across(everything(), ~str_replace_all(., "\\xb5S/", ""))) |> # replace the annoying UTF-8 encoding
  row_to_names(1) |> # make the first row the header
  clean_names(replace = c("`\`" = "")) |> # clean new rownames 
  na.omit() |> # drop columns with missing data
  filter(if_all(everything(), ~!is.na(.) & . != "")) |> # drop rows with missing data |> 
  unite("time", date_mm_dd_yyyy:time_hh_mm_ss, sep=" ", remove=FALSE) |> # combine date and time
  mutate(is_header = row_number() == 1 | str_detect(date_mm_dd_yyyy, "^Date")) |> # if first row or contains date 
  group_by(grp = cumsum(is_header)) |> 
  filter(!str_detect(date_mm_dd_yyyy, "Date")) |> # drop rows containing date
  mutate(sample_id = if_else(row_number() == 1, paste0(date_mm_dd_yyyy, "_", time_hh_mm_ss), NA_character_)) |> # make NA if else
  fill(sample_id, .direction = "down") |> # fill down
  ungroup() |> 
  mutate(time=dmy_hms(time)) |> # lubridate for time 
  select(-date_mm_dd_yyyy, -time_hh_mm_ss) |>  # drop old date and tiem
  select(-is_header, -grp) |>   # remove helper columns
  relocate(sample_id) |>  # move id to first column
  select(- time_fract_sec, -site_name, -time_fract_sec, -wiper_position_volt, -battery_v, -cable_pwr_v, -upc) |> # clean redundancy
  group_by(sample_id) |> 
  mutate(seq_id=seq(1:n())) |> # add a sequence id per cast
  ungroup() |> 
  mutate(sample_id=as.factor(sample_id)) |> # make sample_id a factor
  mutate_if(is.character, as.numeric) |> # change character to numeric
  filter(vertical_position_m > 0) |> # remove air casts
  relocate(vertical_position_m, .after=time) |> # move earlier in column order 
  group_by(sample_id) |> 
  mutate(
    max_val_row = which.max(vertical_position_m), # Identify the row with the maximum 'vertical_position_m' value
    cast = ifelse(row_number() <= max_val_row, "descent", "ascent") # Assign 'descent' or 'ascent'
  ) |> 
  select(-max_val_row) # drop temp col




# plot_a
exo |> 
  filter(sample_id %in% levels(sample_id)[6:11]) |> 
  ggplot() + theme_bw() + facet_wrap(~sample_id) +
  geom_point(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), size=0.2, show.legend = FALSE) +
  geom_line(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), size=0.1, show.legend = FALSE) +
  geom_smooth(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), show.legend = FALSE, 
              method = "lm", formula = y ~ splines::bs(x, 3), se=FALSE) +
  scale_y_reverse() 

# plot b
exo |> 
  filter(sample_id %in% levels(sample_id)[6:11]) |> 
  filter(cast=="descent") |>  
  group_by(sample_id) %>% 
  slice_head(n=nrow(.)-1) |> 
  ggplot() + theme_bw() + facet_wrap(~sample_id) +
  geom_point(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), size=0.2, show.legend = FALSE) +
  geom_line(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), size=0.1, show.legend = FALSE) +
  geom_smooth(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), show.legend = FALSE, 
              method = "lm", formula = y ~ splines::bs(x, 3), se=FALSE) +
  scale_y_reverse() 

# plot c
exo |> 
  ggplot() + theme_bw() + 
  geom_smooth(aes(y=chlorophyll_ug_l, x=vertical_position_m, color=sample_id), show.legend = FALSE, 
              method = "lm", formula = y ~ splines::bs(x, 3), se=FALSE) +
  scale_y_reverse() 



library(mgcv)
library(splines)
library(ggeffects)
library(tidygam)

fm1 <- gam(vertical_position_m ~ chlorophyll_ug_l, k=5,  data=exo)
preds <- predict_gam(fm1)

ggplot() + theme_bw() + 
  geom_line(data=preds, aes(x=vertical_position_m, y=chlorophyll_ug_l)) +
  scale_y_reverse() 


fm1 <- gam(vertical_position_m ~ chlorophyll_ug_l * sample_id, k=4,  data=exo)
preds <- predict_response(fm1, terms = c("chlorophyll_ug_l", "sample_id"), type="fixed") |> as.data.frame()

ggplot() + theme_bw() + 
  geom_line(data=preds, aes(x=x, y=predicted,color=group)) +
  scale_y_reverse() 
