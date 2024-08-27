# load libraries
library(tidyverse)
library(lme4)

# Read in data
d <- read_csv("data/merged_data.csv")

# Select variables
d <- d %>%
  select(new_la_code, la_name, time_period, 
         at_31_cla_rate10000, at31_cpp_rate10000, at31_cin_ep_rate_10000, # outcomes
         gspend_noncla_nonsg_pc, gspend_saf_pc, # spending predictors
         imd19_idaci, rel_pov_rate100 # poverty measures
         )


# Filter data
d <- d %>% 
  filter(!la_name %in% c("Isles of Scilly", "Isle of Wight", "City of London")) 

# turn spending into £100s
d <- d %>% 
  mutate_at(
    vars(gspend_noncla_nonsg_pc:gspend_saf_pc), ~./100
  )

# create lagged variables, uncentered
d <- d %>%
  group_by(la_name) %>%
  arrange(la_name, time_period) %>%
  mutate_at(
    vars(at_31_cla_rate10000:rel_pov_rate100), list(`unc_lag` = ~lag(., 1, order_by = time_period))
  ) %>%
  ungroup()

d %>%
  select(la_name, time_period, gspend_noncla_nonsg_pc, gspend_noncla_nonsg_pc_unc_lag) %>% head(20)


# Example models ----------------------------------------------------------

# Example model without LA intercepts
model_lm <- lm(data = d, 
              formula = at_31_cla_rate10000 ~ gspend_noncla_nonsg_pc + gspend_noncla_nonsg_pc_unc_lag
)

summary(model_lm)

plot(model_lm)


# Example model with LA random intercepts within-between
#' This is equivalent to only explaining differences within local authorities using general
#' data on their predictors - both their between differences and their within differences.
model_un <- lmer(data = d, 
     formula = at_31_cla_rate10000 ~ gspend_noncla_nonsg_pc + gspend_noncla_nonsg_pc_unc_lag + 
                                     (1 | la_name)
     )

summary(model_un)
ranef(model_un)

hist(ranef(model_un)$la_name[,1])

# Within-between pre-processing -------------------------------------------

#' This is generalised code to go through all variables and to create:
#' Between versions of the variables (group means = grp_mean)
#' Within versions of the variables (values minus group means)
#' Within versions of the variables with a lag 
d <- d %>%
  group_by(la_name) %>%
  mutate_at(
    vars(at_31_cla_rate10000:rel_pov_rate100),
    list(grp_mean = ~mean(., na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate_at(
    vars(at_31_cla_rate10000_grp_mean:rel_pov_rate100_grp_mean), 
    ~scale(., center = TRUE, scale = FALSE)
  ) %>%
  group_by(new_la_code) %>%
  arrange(new_la_code, time_period) %>% 
  mutate_at(vars(at_31_cla_rate10000:rel_pov_rate100), list(gmc = ~. - mean(., na.rm = TRUE))) %>% 
  # create lagged group mean centered variables
  mutate_at(vars(at_31_cla_rate10000_gmc:rel_pov_rate100_gmc), list(lag = ~lag(., 1))) %>% 
  ungroup()

d %>%
  select(la_name, gspend_noncla_nonsg_pc_grp_mean, gspend_noncla_nonsg_pc_gmc, gspend_noncla_nonsg_pc_gmc_lag) %>% view()



# Within-between model with English data ----------------------------------


model_null <- lmer(data = d, 
                 formula = at_31_cla_rate10000 ~ 1 + (1 | la_name)
)

summary(model_null)

# Example with within-between
model_wb <- lmer(data = d, 
               formula = at_31_cla_rate10000 ~ 
                             gspend_noncla_nonsg_pc_grp_mean + gspend_noncla_nonsg_pc_gmc + gspend_noncla_nonsg_pc_gmc_lag +
                             (1 | la_name)
)

summary(model_wb)

# Identical model from research project
model_wb_full <- lmer(data = d, 
                 formula = at_31_cla_rate10000 ~ 
                   at31_cpp_rate10000_grp_mean + at31_cin_ep_rate_10000_grp_mean +
                   I(imd19_idaci*100) +
                   gspend_noncla_nonsg_pc_grp_mean + gspend_noncla_nonsg_pc_gmc + gspend_noncla_nonsg_pc_gmc_lag +
                   (1 | la_name)
)

summary(model_wb_full)


# Use delta method to calculate two-year effect
car::deltaMethod(model_wb_full, "gspend_noncla_nonsg_pc_gmc+gspend_noncla_nonsg_pc_gmc_lag", rhs = 0)




# Finnish data ------------------------------------------------------------

# tidying
fi_care <- read_delim("data/finnish_data/children-in-ooh-care.csv", delim = ";", 
           col_names = c("varname", "id", "municipality", "m_code", "gender", "time_period", "percent_care", "count_care")) %>%
  mutate(
    percent_care = as.numeric(str_replace(percent_care, ",", "."))
  ) %>%
  select(
    -varname, -id, -gender
  )

fi_notif <- read_delim("data/finnish_data/cw_notifs.csv", delim = ";", 
           col_names = c("varname", "id", "municipality", "m_code", "gender", "time_period", "percent_cwnotif", "count_cwnotif")) %>%
  mutate(
    percent_cwnotif = as.numeric(str_replace(percent_cwnotif, ",", ".")),
    percent_cwnotif = ifelse(percent_cwnotif == 0, NA, percent_cwnotif),
    count_cwnotif = ifelse(count_cwnotif == 0, NA, count_cwnotif)
  ) %>%
  select(
    -varname, -id, -gender
  )

fi_0to17 <- read_delim("data/finnish_data/population-0-17.csv", delim = ";", 
           col_names = c("varname", "id", "municipality", "m_code", "gender", "time_period", "percent_0to17", "count_0to17")) %>%
  select(
    -varname, -id, -gender, -percent_0to17
  )

fi_nise <- read_delim("data/finnish_data/non-institutional-service-expenditure-1000s.csv", delim = ";", 
           col_names = c("varname", "id", "municipality", "m_code", "gender", "time_period", "spend_nise", "spend_nise2")) %>%
  select(
    -varname, -id, -gender, -spend_nise2
  ) %>%
  mutate(
    spend_nise = spend_nise*1000
  ) %>%
  drop_na(spend_nise)

fi_pov <- read_delim("data/finnish_data/child-at-risk-poverty-rate.csv", delim = ";", 
           col_names = c("varname", "id", "municipality", "m_code", "gender", "time_period", "poverty_percent", "poverty_count")) %>%
  select(
    -varname, -id, -gender
  )

# Merge data

fi_d <- left_join(fi_0to17, fi_care, by = c("municipality", "m_code", "time_period")) %>%
  left_join(., fi_notif, by = c("municipality", "m_code", "time_period")) %>%
  left_join(., fi_nise, by = c("municipality", "m_code", "time_period")) %>%
  mutate(
    spend_nise_pc = spend_nise / count_0to17
  ) %>% 
  left_join(., fi_pov, by = c("municipality", "m_code", "time_period"))

fi_d$country <- "Finland"
d$country <- "England"



# Visualise changes in this form of spending ------------------------------

fi_d %>%
  drop_na() %>%
  ggplot() +
  geom_line(aes(x = time_period, y = spend_nise_pc, group = municipality), size = 0.1, alpha = 0.4) +
  geom_smooth(data = . %>% group_by(time_period) %>% summarise(spend_nise_pc = median(spend_nise_pc, na.rm = TRUE)), 
              aes(x = time_period, y = spend_nise_pc), size = 1, alpha = 0.8, colour = "cornflowerblue", se = F) +
  theme_minimal()

d %>%
  filter(time_period %in% 2015:2022) %>%
  ggplot() +
  geom_line(aes(x = time_period, y = gspend_noncla_nonsg_pc, group = la_name), size = 0.1, alpha = 0.4) +
  geom_smooth(data = . %>% group_by(time_period) %>% summarise(gspend_noncla_nonsg_pc = median(gspend_noncla_nonsg_pc, na.rm = TRUE)), 
              aes(x = time_period, y = gspend_noncla_nonsg_pc), size = 1, alpha = 0.8, colour = "firebrick", se = F) +
  theme_minimal()


# Modelling - Finland

# Make spending into €100s
fi_d <- fi_d %>%
  mutate(
    spend_nise_pc = spend_nise_pc/100
  )

# Make care per 10,000
fi_d <- fi_d %>%
  mutate(
    care_per_10k = percent_care*100
  )

fi_d <- fi_d %>%
  mutate(
    notifs_per_10k = percent_cwnotif*100
  )

# create uncentered lags
fi_d <- fi_d %>%
  group_by(municipality) %>%
  arrange(municipality, time_period) %>%
  mutate_at(
    vars(count_0to17:notifs_per_10k), list(`unc_lag` = ~lag(., 1, order_by = time_period))
  ) %>%
  ungroup()



# Example model without any within-between --------------------------------

model_fi1 <- lm(data = fi_d, 
                  formula = care_per_10k ~ notifs_per_10k + notifs_per_10k_unc_lag + 
                    spend_nise_pc + spend_nise_pc_unc_lag + 
                    poverty_percent + poverty_percent_unc_lag)

summary(model_fi1)

# No random intercepts effect: +7.7 per 10,000 increase for a €100 per child increase

model_fi2 <- lmer(data = fi_d, 
               formula = care_per_10k ~ notifs_per_10k + notifs_per_10k_unc_lag + 
                                        spend_nise_pc + spend_nise_pc_unc_lag + 
                                        poverty_percent + poverty_percent_unc_lag + 
                                       (1 | municipality))

summary(model_fi2)

# Total estimate over two years = ~ 3.9 per 10,000 increase for a €100 per child increase
# Also note: no significant effect of poverty - why?


# Within-between tidying
fi_d_tidy <- fi_d %>%
  drop_na(spend_nise) %>%
  group_by(municipality) %>%
  mutate(
    notifs_per_10k_grp_mean = mean(notifs_per_10k, na.rm = TRUE),
    spend_nise_pc_grp_mean = mean(spend_nise_pc, na.rm = TRUE),
    poverty_percent_grp_mean = mean(poverty_percent, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    notifs_per_10k_gmc = notifs_per_10k - notifs_per_10k_grp_mean,
    spend_nise_pc_gmc = spend_nise_pc - spend_nise_pc_grp_mean,
    poverty_percent_gmc = poverty_percent - poverty_percent_grp_mean
  ) %>%
  # Grand mean centre group means
  mutate(
    notifs_per_10k_grp_mean = notifs_per_10k_grp_mean - mean(notifs_per_10k_grp_mean, na.rm = TRUE),
    spend_nise_pc_grp_mean = spend_nise_pc_grp_mean - mean(spend_nise_pc_grp_mean, na.rm = TRUE),
    poverty_percent_grp_mean = poverty_percent_grp_mean - mean(poverty_percent_grp_mean, na.rm = TRUE)
  ) %>%
  group_by(municipality) %>%
  arrange(municipality, time_period) %>%
  mutate(
    notifs_per_10k_gmc_lag = dplyr::lag(notifs_per_10k_gmc, 1, order_by = time_period),
    spend_nise_pc_gmc_lag = dplyr::lag(spend_nise_pc_gmc, 1, order_by = time_period),
    poverty_percent_gmc_lag = dplyr::lag(poverty_percent_gmc, 1, order_by = time_period)
  )


# Within-between model with Finnish data ----------------------------------

model_fi_wb1 <- lmer(data = fi_d_tidy, 
                    formula = care_per_10k ~ notifs_per_10k_grp_mean + notifs_per_10k_gmc + notifs_per_10k_gmc_lag + 
                                             spend_nise_pc_grp_mean + spend_nise_pc_gmc + spend_nise_pc_gmc_lag + 
                                             poverty_percent_grp_mean + poverty_percent_gmc + poverty_percent_gmc_lag +
                                             (1 | municipality)
                    )

summary(model_fi_wb1)

# Note now that the effect of non-institutional spending is around a 2.4 per 10,000 increase
# And also, we see that there is a significant and strong relationship between poverty at the 
# municipality level and rates of children in out of home care, even though there is no
# significant relationship *within* municipalities over time.

# Example of calculating a long run propensity SE/p value using delta method

car::deltaMethod(model_fi_wb1, "spend_nise_pc_gmc+spend_nise_pc_gmc_lag", rhs = 0)
car::deltaMethod(model_fi_wb1, "poverty_percent_gmc+poverty_percent_gmc_lag", rhs = 0)



# Variation in effects over time ------------------------------------------

model_fi_wb_timere <- lmer(data = fi_d_tidy, 
                     formula = care_per_10k ~ notifs_per_10k_grp_mean + notifs_per_10k_gmc_lag + 
                       spend_nise_pc_grp_mean + spend_nise_pc_gmc_lag + 
                       poverty_percent_grp_mean + poverty_percent_gmc_lag +
                       (1 | municipality) + (0 + spend_nise_pc_gmc_lag | time_period)
)

summary(model_fi_wb_timere)
ranef(model_fi_wb_timere)$time_period + 1.352

# For increased spending in 2015 and 2016, there were large increases on average predicted in OOH care
# in 2016 and 2017 respectively.

# However, for increased spending in 2018 and 2019, there were predicted decreases in OOH care




# Variation by municipality (spatial variation) ---------------------------

model_fi_spatial <- lmer(data = fi_d_tidy, 
                     formula = care_per_10k ~ notifs_per_10k_grp_mean + notifs_per_10k_gmc + notifs_per_10k_gmc_lag + 
                       spend_nise_pc_grp_mean + spend_nise_pc_gmc + spend_nise_pc_gmc_lag + 
                       poverty_percent_grp_mean + poverty_percent_gmc + poverty_percent_gmc_lag +
                       (1 + spend_nise_pc_gmc + spend_nise_pc_gmc_lag || municipality)
)

summary(model_fi_spatial)

spatial_inequalities_nise <- tibble(rownames_to_column(ranef(model_fi_spatial)$municipality)) %>% 
  mutate(
    lrp_spend_nise = spend_nise_pc_gmc + spend_nise_pc_gmc_lag + 3.214578 + -1.087305
  )

spatial_inequalities_nise <- left_join(spatial_inequalities_nise, 
                                       fi_d %>% select(municipality, m_code) %>% group_by(municipality, m_code) %>% summarise_all(first), 
                                       by = c("rowname" = "municipality"))

# Some of the largest and smallest estimates
spatial_inequalities_nise %>%
  arrange(lrp_spend_nise)

spatial_inequalities_nise %>%
  arrange(desc(lrp_spend_nise))




# Mapping spatial variation -----------------------------------------------

library(sf)

muni_sf <- read_sf("data/finnish_data/TietoaKuntajaosta_2020_1000k/SuomenKuntajako_2020_1000k.shp")
# muni_sf %>% ggplot() + geom_sf()

anti_join(muni_sf, spatial_inequalities_nise, by = c("NAMEFIN" = "rowname"))
anti_join(spatial_inequalities_nise, muni_sf, by = c("rowname" = "NAMEFIN"))
anti_join(muni_sf, spatial_inequalities_nise, by = c("NATCODE" = "m_code"))

muni_sf <- left_join(muni_sf, spatial_inequalities_nise, by = c("NAMEFIN" = "rowname"))

muni_sf %>%
  ggplot() +
  geom_sf(aes(fill = lrp_spend_nise), colour = "#f0f0f0") +
  scale_fill_gradientn(colours = c("#006353", "#f0f0f0", "#FC9C19"), 
                       values = scales::rescale(c(min(spatial_inequalities_nise$lrp_spend_nise, na.rm = TRUE), 
                                                  -5,
                                                  0, 
                                                  5, 
                                                  max(spatial_inequalities_nise$lrp_spend_nise, na.rm = TRUE)))
  ) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggeasy::easy_add_legend_title("Change in care rate per 10k\nfor €100 per child increase\n")


# Interactive map
library(leaflet)

muni_sf_ll <- muni_sf %>%
  st_transform(., crs = st_crs("+proj=longlat +datum=WGS84"))

fill_palette <- colorNumeric(palette = "RdBu",
                              domain = c(-17, 17),
                              na.color = "grey")

leaflet(muni_sf_ll) %>%
  addPolygons(weight = 0.1, stroke = FALSE,
              fillColor = ~fill_palette(lrp_spend_nise),
              fillOpacity = 0.7,
              popup = paste0(
                "<b>Kunta: </b>",
                muni_sf_ll$NAMEFIN, "<br>",
                "<b>Effect: </b>"
                , round(muni_sf_ll$lrp_spend_nise, 2)
              )
              ) %>%
  addTiles() %>%
  addLegend(pal = fill_palette,
            values = ~lrp_spend_nise)




# Notes -------------------------------------------------------------------

