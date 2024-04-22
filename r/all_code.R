
# SSEI sablefish analysis for management memo. 
# Includes: survey and fishery CPUE and summary of biological data
# Authors:  Andrew Olson (andrew.olson@alaska.gov); and Rhea Ehresmann (rhea.ehresmann@alaska.gov) 
# Code adapted from J.S. NSEI Sablefish assessment: Jane Sullivan (jane.sullivan@alaska.gov)
# Last modified: April 19, 2024   # first run for 2024

# set up ----
source('r/helper.r') 
library(janitor) 

# Create figure and output folders
YEAR <- 2024 # assessment year
fig_path <- paste0('figures/', YEAR) # folder to hold all figures for a given year
dir.create(fig_path) # creates YEAR subdirectory inside figures folder
output_path <- paste0('output/', YEAR) # output and results
dir.create(output_path) 



# data ----

# harvest by year and permit type AHO from management memo
ssei_aho <- data.frame(year = c(1985:2023), # change most recent year to 2023
                       aho = c(rep(790000, 13), 632000, 720000, rep(696000, 9),
                               634000, 634000, 583280, 583280, 583280, 536618, 
                               536618, 482956, 516763, 578774, 590349, 572639, 601271, 
                               643360, 643360))  #need to update with AHO for new year

# Get all fish ticket data from OceanAK query for sablefish in SSEI management area
# and exclude fish tickets from test fishery = 43 and Annette Island Fisheries; remove trawl gear
read_csv("data/fishery/raw_data/SSEI fishticket data new.csv", guess_max = 50000) %>%  
  clean_names() %>% 
  rename(year = dol_year) %>% 
  filter(species_code == 710, !harvest_code %in% c(17, 43), gear_code != 7) -> fishery_df

# Data from OceanAK query for pot logbook data with species code already filtered for 710
read_csv("data/fishery/raw_data/SSEI pot logbook data new.csv") %>%  
  clean_names() %>% 
  filter(trip_target_species_code == 710, groundfish_management_area_code == "SSEI") -> pot_log_df

# Data from OceanAK query for longline logbook data - Region 1 dashboard stock report "Hooks and Ticket Pounds" 
read_csv("data/fishery/raw_data/SSEI longline logbook data new.csv", guess_max = 50000) %>% 
  clean_names() %>% 
  filter(target_species_code == 710, g_management_area_code == "SSEI") -> ll_log_df

# Data from OceanAK stock report - fishery logbook data sablefish lbs per set  ----- DO WE NEED THIS FOR POT LOGBOOKS?! 
read_csv("data/fishery/raw_data/ssei longline sablefish lbs per set new.csv") %>% 
  clean_names() -> ll_set_df

# Data from OceanAK query for longline survey hook accounting in SSEI 
# need to exclude 2021 experimental set 18 station 99
read_csv("data/survey/raw_data/ssei survey hook accounting new.csv", guess_max = 50000) %>% 
  clean_names()  %>%
  filter(station_no != 99) -> srv_data

# Data from OceanAK query for longline survey bio data in SSEI 
# need to exclude 2021 experimental set 
read_csv("data/survey/raw_data/SSEI LL survey bio data new.csv", guess_max = 50000) %>% 
  clean_names() %>% 
  filter(species == "Sablefish") -> svy_bio_df

# Data from OceanAK query for port sampling data in SSEI - filtered already for species 710 and SSEI
read_csv("data/fishery/raw_data/SSEI port sampling data new.csv", guess_max = 50000) %>% 
  clean_names()-> fish_bio_df

##################################################################################################
# harvest by year and permit type ----
##################################################################################################

fishery_df %>% 
  full_join(ssei_aho) %>% 
  group_by(year) %>% 
  summarise(total_harvest = sum(whole_weight_sum),
            aho = mean(aho)) %>% 
  mutate(mgmt_type = ifelse(year %in% 1985:1996, "Limited Entry", "Equal Quota Share")) -> harvest

write_csv(harvest, paste0(output_path, "/harvest.csv")) # save output

xaxis <- FNGr::tickr(harvest, year, 3)

ggplot(harvest, aes(year, total_harvest)) + 
  geom_bar(stat = "identity", aes(fill = mgmt_type)) +
  geom_line(aes(y = aho), linetype = 3, linewidth = 1) +
  ylab("Harvest (round lbs)\n") + xlab("\nYear") +
  scale_fill_grey() + # use grey-scale for the report
  #scale_fill_manual(values= cbPalette) + scale_color_manual(values = cbPalette) +
  scale_x_continuous(breaks = xaxis$breaks, labels=xaxis$labels) +
  scale_y_continuous(label = scales::comma) +
  theme(legend.position = c(0.75, 0.85), legend.title = element_blank()) 
  

ggsave(paste0(fig_path, '/ssei_fishery_harvest.png'), width = 6.5, height = 5, units = "in", dpi = 200)


##################################################################################################
# harvest by gear name
##################################################################################################

fishery_df %>% 
  full_join(ssei_aho) %>% 
  group_by(year, gear_name) %>% 
  summarise(total_harvest = sum(whole_weight_sum),
            n_permits = n_distinct(cfec),
            n_boats = n_distinct(adfg), 
            aho = mean(aho)) %>% 
  mutate(mgmt_type = ifelse(year %in% 1985:1996, "Limited Entry", "Equal Quota Share")) %>% 
  filter(n_boats >= 3) -> harvest_gear


xaxis <- FNGr::tickr(harvest, year, 3)

ggplot(harvest_gear, aes(fill = gear_name, x=year, y=total_harvest)) + 
  geom_bar(stat = "identity", position = "stack") +
  #geom_line(aes(y = aho), linetype = 3, linewidth = 1) +
  ylab("Harvest (round lbs)\n") + xlab("\nYear") +
  #scale_fill_grey() + # use grey-scale for the report
  scale_fill_manual(values= cbPalette) + 
  #scale_color_manual(values = cbPalette) +
  scale_x_continuous(breaks = xaxis$breaks, labels=xaxis$labels) +
  scale_y_continuous(label = scales::comma)+
  theme(legend.position = c(0.75, 0.85), legend.title = element_blank()) 

ggsave(paste0(fig_path, '/ssei_fishery_harvest_gear_Confidential.png'), width = 6.5, height = 5, units = "in", dpi = 200)

# Harvest distribution by area ---- not in report, look at distribution
unique(fishery_df$stat_area)

fishery_df %>% 
  filter(stat_area != 1065, 
         mgt_area == "SSEI", year >= 1997) %>%
  mutate(Area = case_when(stat_area %in% c(325431, 315431, 325401, 315401) ~ "Dixon Entrance",
                          stat_area %in% c(305431, 305501, 305502, 305503, 315432, 315501,
                                             315502, 315503, 315504, 325433, 325501, 325502,
                                             325503, 325504) ~ "Lower Clarence Strait",
                          stat_area %in% c(305531, 305532, 315531, 315532, 325531, 325532,
                                             325533, 335506, 335534, 335535) ~ "Upper Clarence Strait",
                          stat_area %in% c(315600, 325601, 325602, 325603, 325604, 325631, 325632,
                                             335533, 335601, 335602, 335603, 335632, 335633, 345535,
                                             345604) ~ "Sumner Strait",
                          TRUE ~ "Other")) %>% 
  group_by(year, gear_name, Area) %>% 
  summarise(total_harvest = sum(whole_weight_sum), 
            n_boats = n_distinct(adfg),
            permit_count = n_distinct(cfec)) %>% 
  mutate(Area = factor(Area, 
                       levels = c("Sumner Strait", "Upper Clarence Strait", 
                                  "Lower Clarence Strait", "Dixon Entrance", "Other"))) %>%  
 filter(n_boats >= 3) -> area_harvest 

write_csv(area_harvest, paste0(output_path, "/harvest_byarea.csv")) # save output

ggplot(area_harvest, aes(year, total_harvest, fill = Area)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_grey() +
  ylab("Total Harvest (round lbs)\n") + 
  xlab("\nYear") +
  scale_x_continuous(breaks = xaxis$breaks, labels=xaxis$labels) +
  scale_y_continuous(labels = scales::comma, breaks = seq(0, 700000, 100000)) +
  theme(legend.position = c(0.75, 0.88), legend.title = element_blank()) + 
  facet_wrap(~gear_name)

ggsave(paste0(fig_path,"/SSEI_Fishery_Harvest_Distribution_gear.png"), width = 6.5, 
       height = 6, units = "in", dpi = 200)


##################################################################################################
# pot fishery cpue ---- SKIP THIS IT's INCORRECT 
##################################################################################################

# Confidentiality
fishery_df %>%
  filter(gear_name == 'Pot') %>%
  group_by(year) %>% 
  summarize(n_tickets = n_distinct(fish_ticket_number), 
         n_permits = n_distinct(cfec),
         n_boats = n_distinct(adfg), 
         n_proc = n_distinct(processor)) %>% 
  filter(n_boats >= 3, year > 1995) -> keep_yrs  
# can only keep 2020, 2021, 2022 due to confidentiality with POTS - check LL 


# pot logbook data 
pot_log_df %>%
filter(year > 2019) %>%
group_by(year, trip_number) %>%
mutate(pounds = ifelse(is.na(pounds), numbers * 5.0, pounds)) %>%  # avg weight from port sampling data - pot trips only from 2019-2021
summarize(round_pounds = sum(pounds),
        n_pots = sum(number_of_pots)) %>%
mutate(cpue = round_pounds / n_pots) %>%  # if you view this you can see a huge variation in pot cpue, some impossible
summarise(sd = sd(cpue),
        cpue = mean(cpue),
      n = n(),
       se = sd / sqrt(n)) %>%
mutate(ll = cpue - 2 * se,
     ul = cpue + 2 * se) -> pot_cpue

write_csv(pot_cpue, paste0(output_path, "/pot_cpue.csv")) # save output

pot_cpue %>% ggplot(aes(year, cpue)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ll, ymax = ul), alpha=0.2) +
  ylab("CPUE (round lbs/pot)\n") +
  xlab('\nYear') +
  #scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm")) +
  expand_limits(y = 0)

ggsave(paste0(fig_path,"/pot_fishery_cpue.png"), width = 6.5,
height = 5, units = "in", dpi = 200)


##################################################################################################
# ll survey cpue ---- 
##################################################################################################

# standardize hook spacing (Sigler & Lunsford 2001, CJFAS) changes in hook
# spacing. pers. comm. with aaron.baldwin@alaska.gov: 1995 & 1996 - 118 in; 1997
# - 72 in.; 1998 & 1999 - 64 in; 2000-present - 78 in. This is different from KVK's
# code (he assumed 3 m before 1997, 2 m in 1997 and after) soak time was
# standardized to at least 3 hours in 1997 prior to that it was 1 hour Mike
# Vaughn 2018-03-06: Sets (aka subsets with 12 or more invalid hooks are subset
# condition code "02" or invalid)

srv_cpue <- srv_data %>% 
  dplyr::select(year, trip_no, set = set_no, skate = subset_no,
                skate_condition_cde = subset_condition_code, 
                Stat = g_stat_area, bare = hooks_number_bare,
                bait = hooks_number_with_bait, invalid = hooks_number_invalid,
                no_hooks = hooks_total_number, sablefish)

# we need to review raw data - looks like there are a few subsets with more than
# 12 invalid hooks that are not invalid

# data checks 

# TODO: these should be changed to condition code 2. 
srv_cpue %>% filter(skate_condition_cde %in% c(1,3) & invalid > 12) 
srv_cpue <- srv_cpue %>% # Fix manually for now
  mutate(skate_condition_cde = ifelse(skate_condition_cde %in% c(1,3) & invalid > 12, 
                                      2, skate_condition_cde)) 

srv_cpue %>% filter(no_hooks < 0) # there should be none

srv_cpue %>% filter(year > 1997 & c(is.na(no_hooks) | no_hooks == 0)) # there should be none


# TODO this needs to be fixed in database: bare, bait, invalid should all be 0,
# skate_condition_cde should be 2. Fixed manually for now:
srv_cpue <- srv_cpue %>% 
  mutate(skate_condition_cde = ifelse(year > 1997 & c(is.na(no_hooks) | no_hooks == 0), 2, skate_condition_cde))

str(srv_cpue)

# Get subset for cpue analysis, standardize hooks
srv_cpue <- srv_cpue %>% 
  filter(year >= 1998, 
         skate_condition_cde %in% c("1", "3")) %>% 
  replace_na(list(bare = 0, bait = 0, invalid = 0, sablefish = 0)) %>% 
  mutate(no_hooks = no_hooks - invalid, # remove invalid hooks
         std_hooks = ifelse(year %in% c(1998, 1999), 2.2 * no_hooks * (1 - exp(-0.57 * (64 * 0.0254))),
                            2.2 * no_hooks * (1 - exp(-0.57 * (78 * 0.0254)))))

# Data set is currently at skate level (each row is a skate). CPUE should be
# calculated at the set level (skates within a set are expected to be highly
# correlated, not independent)
srv_cpue %>% 
  group_by(year, trip_no, set) %>%
  dplyr::summarise(bare = sum(bare),
         bait = sum(bait),
         sablefish = sum(sablefish),
         set_hooks = sum(std_hooks),
         set_cpue = sablefish / set_hooks) %>%
  ungroup() %>% 
  mutate(trip_set_id = paste0(trip_no, "_", set)) %>% 
  group_by(year) %>% 
  dplyr::summarise(n_set = length(unique(trip_set_id)),
                   cpue = mean(set_cpue),
                   sd = round(sd(set_cpue), 4),
                   se = round(sd / sqrt(n_set), 4)) -> srv_cpue

write_csv(srv_cpue, paste0(output_path, "/llsurvey_cpue.csv")) # save output

# Percent change in compared to a ten year rolling average
srv_cpue %>% 
  filter(year > YEAR - 10 & year <= YEAR) %>% 
  mutate(lt_mean = mean(cpue),
         perc_change_lt = (cpue - lt_mean) / lt_mean * 100,
         eval_lt = ifelse(perc_change_lt < 0, "decrease", "increase")) %>% 
  filter(year == max(srv_cpue$year)) -> srv_lt
srv_lt

# Percent change from last year
srv_cpue %>% 
  filter(year >= max(srv_cpue$year) - 1 & year <= max(srv_cpue$year)) %>%
  select(year, cpue) %>% 
  
  mutate(year2 = ifelse(year == max(srv_cpue$year), "thisyr", "lastyr")) %>% 
  reshape2::dcast("cpue" ~ year2, value.var = "cpue") %>% 
  mutate(perc_change_ly = (thisyr - lastyr) / lastyr * 100,
         eval_ly = ifelse(perc_change_ly < 0, "decreased", "increased")) -> srv_ly
srv_ly

# Figure
axis <- tickr(srv_cpue, year, 3)
ggplot(data = srv_cpue) +
  geom_point(aes(x = year, y = cpue)) +
  geom_line(aes(x = year, y = cpue)) +
  geom_ribbon(aes(year, ymin = cpue - sd, ymax = cpue + sd),
              alpha = 0.2) +
  scale_x_continuous(breaks = axis$breaks, labels = axis$labels) + 
  lims(y = c(0, 0.40)) +
  labs(x = NULL, y = "Survey CPUE (number per hook)\n") 

ggsave(paste0(fig_path,"/ssei_ll_survey_cpue.png"), 
       dpi = 300, width = 6.5, height = 5, units = "in")


##################################################################################################
# Trends in number of total trips and vessels participating in the fishery with LL gear
##################################################################################################

ll_set_df %>% 
  select(year, trip_no, adfg = adfg_no, Spp_cde = trip_target, time_set,
         time_hauled, Gear = longline_system_code, hook_size,  
         hook_spacing, Stat = g_stat_area, no_hooks = number_of_hooks, 
         depth = average_depth_meters, 
         sets = effort_no, sable_lbs_set = sable_lbs_per_set, 
         start_lat = start_latitude_decimal_degrees,
         start_lon = start_longitude_decimal_degree) %>% 
  mutate(date = anydate(time_set),
         julian_day = yday(date),
         time_set = anytime(time_set),
         time_hauled = anytime(time_hauled),
         soak = difftime(time_hauled, time_set, units = 'hours'),
         Gear = factor(Gear),
         Gear = case_when(Gear == "6" ~ "AB",
                          Gear %in% c("1", "2", "5") ~ "CS",
                          TRUE ~ "Other"),
         Hook_size = factor(hook_size),
         Size = factor(as.numeric(gsub("[^0-9]", "", hook_size))),
         Year = factor(year),
         ADFG = factor(adfg)) %>% 
  group_by(year) %>% 
  mutate(total_vessels = n_distinct(ADFG),
         total_trips = n_distinct(trip_no)) %>% 
  filter(total_vessels >=3) %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>%  
  distinct() %>%  
  mutate(Gear = "Longline") %>% 
  filter(year <= 2023) -> longline_trips 

longline_trips %>% 
  ggplot(aes(year, Count)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  labs(x = "\nYear", y = "Count of Longline Trips and Vessels") +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  expand_limits(y = 0) +
  theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))

ggsave(paste0(fig_path, "/fishery_trip_vessel_trends_1997_", YEAR, ".png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

##################################################################################################
# Trends in number of total trips and vessels participating in the fishery with pot gear
##################################################################################################

pot_log_df %>% 
  select(year, trip_no = trip_number, adfg = adfg_number) %>% 
  mutate(adfg = factor(adfg)) %>% 
  group_by(year) %>% 
  mutate(total_vessels = n_distinct(adfg),
         total_trips = n_distinct(trip_no)) %>% 
  filter(total_vessels >=3) %>% 
  select(year, Vessels = total_vessels, Trips = total_trips) %>% 
  gather(Variable, Count, -year) %>%  
  distinct() %>%  
  mutate(Gear = "Pot") %>% 
  filter(year <= 2023) -> pot_trips 

  
total_trips <- full_join(pot_trips, longline_trips) 
total_trips %>% 
  ggplot(aes(year, Count)) +
  geom_line(aes(color = Gear), linewidth = 2) +
  geom_point(size = 2) +
  facet_wrap(~ Variable, ncol = 1, scales = "free") +
  labs(x = "\nYear", y = "Count") +
  scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
  expand_limits(y = 0) +
  theme(plot.margin = unit(c(0.5,1,0.5,0.5), "cm"))

ggsave(paste0(fig_path, "/pot_and_LL_fishery_trip_vessel_trends_1997_", YEAR, ".png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)


##################################################################################################
# ll fishery cpue ----
##################################################################################################
ll_set_df %>% 
  select(year, trip_no, adfg = adfg_no, Spp_cde = trip_target, time_set,
         time_hauled, Gear = longline_system_code, hook_size,  
         hook_spacing, Stat = g_stat_area, no_hooks = number_of_hooks, 
         depth = average_depth_meters, 
         sets = effort_no, sable_lbs_set = sable_lbs_per_set, 
         start_lat = start_latitude_decimal_degrees,
         start_lon = start_longitude_decimal_degree) %>% 
  mutate(date = anydate(time_set),
         julian_day = yday(date),
         time_set = anytime(time_set),
         time_hauled = anytime(time_hauled),
         soak = difftime(time_hauled, time_set, units = 'hours'),
         Gear = factor(Gear),
         Gear = case_when(Gear == "6" ~ "AB",
                          Gear %in% c("1", "2", "5") ~ "CS",
                          TRUE ~ "Other"),
         Hook_size = factor(hook_size),
         Size = factor(as.numeric(gsub("[^0-9]", "", hook_size))),
         Year = factor(year),
         ADFG = factor(adfg),
         std_hooks = 2.2 * no_hooks * (1 - exp(-0.57 * (hook_spacing / 39.37))), 
         std_cpue = sable_lbs_set / std_hooks,
         dum = 1, 
         dumstat = 1) %>% 
  filter(!is.na(date), !is.na(hook_spacing), !is.na(sable_lbs_set), !is.na(no_hooks), 
     !is.na(start_lon), !is.na(start_lon), !is.na(soak), !is.na(depth),
     !is.na(hook_size), hook_size != "MIX", soak > 0) %>% 
  group_by(year, trip_no) %>% 
  mutate(no_sets = n_distinct(sets)) %>% 
  group_by(year) %>% 
  mutate(total_vessels = n_distinct(adfg),
         total_trips = n_distinct(trip_no)) %>% 
  ungroup() -> fishery_cpue 


##################################################################################################
# nominal cpue ----
##################################################################################################
fishery_cpue %>% 
  group_by(year) %>% 
  summarise(annual_cpue = mean(std_cpue),
            sdev = sd(std_cpue),
            n = length(std_cpue),
            se = sdev / sqrt(n()),
            var = var(std_cpue),
            cv = sdev / annual_cpue,
            upper = annual_cpue + (2 * se),
            lower = annual_cpue - (2 * se)) -> fish_sum

write_csv(fish_sum, paste0(output_path, "/llfishery_cpue.csv")) # save output

xaxis <- FNGr::tickr(fishery_cpue, year, 2)
fish_sum %>% ggplot(aes(year, annual_cpue)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = annual_cpue - sdev, ymax = annual_cpue + sdev),
              alpha = 0.2) +
  labs(x = NULL, y = "Longline Fishery CPUE (lb/hook)\n") +
  scale_x_continuous(breaks=xaxis$breaks, labels=xaxis$labels) +
  expand_limits(y = 0) + ylim(-0.5, 1.5) 

ggsave(paste0(fig_path, "/ssei_ll_fishery_cpue.png"), width = 6.5, 
       height = 5, units = "in", dpi = 200)

##################################################################################################
# Percent change in fishery nominal cpue compared to a ten year rolling average
##################################################################################################
fish_sum %>% 
  filter(year > max(fish_sum$year) - 10) %>% 
  mutate(lt_mean = mean(annual_cpue),
         perc_change_lt = (annual_cpue - lt_mean) / lt_mean * 100)

# Percent change in fishery nominal cpue from last year
fish_sum %>% 
  filter(year >= max(fish_sum$year) - 1) %>%
  select(year, annual_cpue) %>% 
  reshape2::dcast("annual_cpue" ~ year) -> perc_ch
names(perc_ch) <- c("cpue", "last_year", "this_year") 
perc_ch %>% mutate(perc_change_ly = (`this_year` - `last_year`) / `last_year` * 100)

##################################################################################################
# Age comps ----
##################################################################################################
rec_age <- 2 # age recruiting to fishery or survey
plus_group <- 25 # plus group, lump all the old fish into on age

rbind(
  # ll survey
  svy_bio_df %>% 
    filter(age != "NA", 
           sex %in% c("Male", "Female"), 
           age_readability %in% c("Very Sure", "Comfortably Sure", "Fairly Sure"),
           age >= rec_age) %>% 
    mutate(Source = "Longline survey") %>% 
    select(Source, year, Sex = sex, age),
  # ll gear
  fish_bio_df %>% 
    filter(project_code == 602, age != "NA", 
           sample_type == "Random", sex_code %in% c(1, 2), 
           age_readability_code %in% c(1, 2, 3)) %>% 
    mutate(Sex = case_when(sex_code == 1 ~ "Male",
                         sex_code == 2 ~ 'Female',
                         TRUE ~ 'Other'),
           Source = "Longline fishery") %>% 
    select(Source, year, Sex, age),
  # pot gear
  fish_bio_df %>% 
    filter(project_code == 617, age != "NA", 
           sample_type == "Random", sex_code %in% c(1, 2), 
           age_readability_code %in% c(1, 2, 3)) %>% 
    mutate(Sex = case_when(sex_code == 1 ~ "Male",
                           sex_code == 2 ~ 'Female',
                           TRUE ~ 'Other'),
           Source = "Pot fishery") %>% 
    select(Source, year, Sex, age)) %>% 
  filter(age >= rec_age) %>% 
  mutate(age = ifelse(age >= plus_group, plus_group, age)) %>% 
  count(Source, year, Sex, age) %>% 
  group_by(Source, year, Sex) %>% 
  mutate(proportion = round( n / sum(n), 5)) %>% 
  tidyr::complete(age = rec_age:plus_group, fill = list(n = 0, proportion = 0)) %>% 
  #tidyr::complete(age = rec_age:plus_group, nesting(Source, Sex, year), fill = list(n = 0, proportion = 0)) %>% 
  arrange(Source, year, Sex, age) %>% 
  ungroup() %>% 
  mutate(label = case_when(Source == "Longline survey" ~ "llsrv",
                           Source == "Longline fishery" ~ "llfsh",
                           Source == "Pot fishery" ~ "potfsh" )) -> agecomp_df
#agecomp_df %>% group_by(Source) %>% distinct(year) %>% View()
  
# Check that they sum to 1
agecomp_df %>% 
  group_by(Source, Sex, year) %>% 
  summarise(sum(proportion)) %>% View

# Output age comp sample sizes and proportions
write_csv(agecomp_df, paste0(output_path, "/age_comps.csv"))

# Function to plot ages

plot_age <- function(data = agecomp_df,
                     src = NULL) {
  
  data <- data %>% filter(label == src) 
  data <-  data %>% complete(year = seq(min(data$year), max(data$year), 1))
  xaxis <- FNGr::tickr(data, year, 5)
  yaxis <- FNGr::tickr(data, age, 5)
  
  p <- ggplot(data = na.omit(data),
              aes(x = year, y = age, size = proportion)) + #*FLAG* could swap size with proportion_scaled
    geom_point(shape = 21, fill = "black", colour = "black") +
    scale_size(range = c(0, 4)) +
    facet_wrap(~ Sex) +
    labs(x = "\nYear", y = "Observed age\n") +
    guides(size = FALSE) +
    scale_x_continuous(breaks = xaxis$breaks, labels = xaxis$labels) +
    scale_y_continuous(breaks = yaxis$breaks, labels = yaxis$labels)
  
  
  print(p)
  ggsave(plot = p, paste0(fig_path, "/agecomp_", src, ".png"), dpi=300, height=5, width=7.5, units="in")
  
}

plot_age(data = agecomp_df, src = "llsrv")
plot_age(data = agecomp_df, src = "llfsh")
plot_age(data = agecomp_df, src = "potfsh")

##################################################################################################
# lengths ----
##################################################################################################

fish_bio_df %>% 
  filter(sex_code!=0) %>% 
  mutate(Sex = case_when(sex_code == 1 ~ "Male",
                         sex_code == 2 ~ 'Female',
                         TRUE ~ 'Other'),
         length = length_millimeters / 10,
         survey_type = ifelse(project_code==602, 'Longline', 'Pot')) -> fish_lengths

fish_lengths %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 4) +  #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) + 
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") + 
  facet_wrap(~ survey_type)

ggsave(paste0(fig_path, "/ssei_fishery_lengths.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# length by gear types ----
# longline
fish_lengths %>% 
  filter(survey_type == "Longline", Sex != 'Other') %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                       scale = 3, alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 3) + #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) +
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") +
  facet_wrap(~ Sex)

ggsave(paste0(fig_path, "/ssei_fishery_ll_lengths_sex.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# pot
fish_lengths %>% 
  filter(survey_type == "Pot", Sex != 'Other') %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
  geom_density_ridges(aes(point_fill = year, point_color = year),
                      scale = 3, alpha = 0.3) +
  #geom_vline(xintercept = 61, linetype = 3) + #skip this line as it pertains more to NSEI - J.S. 04/01/2020
  xlim(35, 90) +
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse() +
  theme(legend.position = "none") +
  facet_wrap(~ Sex)

ggsave(paste0(fig_path, "/ssei_fishery_pot_lengths_sex.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)

# survey lengths ----
svy_bio_df %>% 
  filter(sex %in% c('Male', 'Female')) %>% 
  mutate(length = length_millimeters / 10) %>% 
  ggplot(aes(length, year, group = year, fill = year)) + 
    geom_density_ridges(aes(point_fill = year, point_color = year),
                        scale = 3, alpha = 0.3) +
  xlim(35, 90) +
  xlab("\nLength (cm)") + 
  ylab("Year\n") +
  scale_y_reverse(breaks = seq(2021, 1988, by = -5)) +
  theme(legend.position = "none") +
  facet_wrap(~ sex)

ggsave(paste0(fig_path, "/ssei_survey_lengths.png"), width = 6.5, 
       height = 8, units = "in", dpi = 200)





