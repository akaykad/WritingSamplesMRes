### May 5

### Processing GDP, seasonally adjusted chain linked 2010 millions for eea and members
### One sided HP filter will be applied for output gap estimation to log gdp.

####
library(tidyverse)
library(hpfilter)
library(ggplot2)
library(zoo)
####

### Data processing, removing flagged columns 
gdp <- gdp_millions_eu_and_eea %>% 
  select(-contains("...")) %>% 
  mutate(across(.cols = -time, .fns = as.numeric))

## Taking log to prep for hp
gdp_log <- gdp %>% 
  mutate(across(
    -time,
    ~ log(as.numeric(.x) * 1e6)
  ))

## Going long from wide

gdp_long <- gdp_log %>%
  pivot_longer(
    cols = -time,           
    names_to = "variable",   
    values_to = "value"      
  )

colnames(gdp_long) <- c("country", "time", "loggdp")

hp_prep <- gdp_long %>%
  pivot_wider(
    names_from = country,   
    values_from = loggdp      
  )

### Testing 1 sided HP filter on EEA

hp_eea <- hp_prep %>% 
  select(time, eea)

# Convert to time series

gdp_eea <- ts(hp_eea$eea, start = c(1995, 1), frequency = 4) 

hp_eea$eea <- as.numeric(hp_eea$eea)
hp_eea$time <- as.character(hp_eea$time)

# Apply the filter, extract the trend. This does not work, need to isolate the series.

 # gdp_hp <- hp1(hp_eea$eea, lambda = 1600, discard = 0)

# Isolate

gdp_eea_hp <- hp_eea %>% 
  select(eea)

gdp_hp <- hp1(gdp_eea_hp, lambda = 1600, discard = 0)

colnames(gdp_hp) <- c("trend")

hp_eea$trend <- gdp_hp$trend

hp_eea.1 <- hp_eea %>% 
  mutate(output_gap = round((eea-trend)*100,2))

# It works, print it
  
# write.csv(hp_eea.1, "hp1_eea.csv", row.names = FALSE)

# Plotting to check

hp_eea.1 <- hp_eea.1 %>% 
  mutate(index = 1:120)

colnames(hp_eea.1) <- c("time", "loggdp", "trend", "outputgap", "index_time")

filter_plot <- ggplot(hp_eea.1, aes(x = index_time)) + geom_line(aes(x = index_time, y = loggdp), color = "blue") +
  geom_line(aes(x = index_time, y = trend), color = "red") + theme_classic()

filter_plot

output_plot <- ggplot(hp_eea.1, aes(x = index_time)) + geom_line(aes(y = outputgap), color = "black") + 
  theme_classic()

output_plot

### Plots look as expected.

### Now, I will proceed with the member countries. Looping is risky. I will do every member one by one and append.

# We have to remove malta, netherlands will start from 1996-q1 - dataset will end at 2024 q3.

hp_prep <- hp_prep %>% 
  mutate(time_index = 1:120) %>% 
  filter(time_index >= 5 & time_index < 120)

hp_prep <- hp_prep %>% 
  select(-time_index, -Malta)

####### for member countries 

# Austria

austria <- hp_prep %>% 
  select(Austria)
ts_austria <- ts(austria, start = c(1996, 1), frequency = 4) 

trend <- hp1(austria, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

austria$trend <- trend$trend

austria <- austria %>% 
  mutate(output_gap = round((Austria-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Austria")

colnames(austria) <- c("loggdp", "trend", "outputgap", "time", "country")

# Belgium 

belgium <- hp_prep %>% 
  select(Belgium)
ts_belgium <- ts(belgium, start = c(1996, 1), frequency = 4) 

trend <- hp1(belgium, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

belgium$trend <- trend$trend

belgium <- belgium %>% 
  mutate(output_gap = round((Belgium-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Belgium")

colnames(belgium) <- c("loggdp", "trend", "outputgap", "time", "country")

# Croatia

croatia <- hp_prep %>% 
  select(Croatia)
ts_croatia <- ts(croatia, start = c(1996, 1), frequency = 4) 

trend <- hp1(croatia, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

croatia$trend <- trend$trend

croatia <- croatia %>% 
  mutate(output_gap = round((Croatia-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Croatia")

colnames(croatia) <- c("loggdp", "trend", "outputgap", "time", "country")

# Cyprus

cyprus <- hp_prep %>% 
  select(Cyprus)
ts_cyprus <- ts(cyprus, start = c(1996, 1), frequency = 4) 

trend <- hp1(cyprus, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

cyprus$trend <- trend$trend

cyprus <- cyprus %>% 
  mutate(output_gap = round((Cyprus-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Cyprus")

colnames(cyprus) <- c("loggdp", "trend", "outputgap", "time", "country")

# Estonia

estonia <- hp_prep %>% 
  select(Estonia)
ts_estonia <- ts(estonia, start = c(1996, 1), frequency = 4) 

trend <- hp1(estonia, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

estonia$trend <- trend$trend

estonia <- estonia %>% 
  mutate(output_gap = round((Estonia-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Estonia")

colnames(estonia) <- c("loggdp", "trend", "outputgap", "time", "country")

# Finland

finland <- hp_prep %>% 
  select(Finland)
ts_finland <- ts(finland, start = c(1996, 1), frequency = 4) 

trend <- hp1(finland, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

finland$trend <- trend$trend

finland <- finland %>% 
  mutate(output_gap = round((Finland-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Finland")

colnames(finland) <- c("loggdp", "trend", "outputgap", "time", "country")

# France

france <- hp_prep %>% 
  select(France)
ts_france <- ts(france, start = c(1996, 1), frequency = 4) 

trend <- hp1(france, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

france$trend <- trend$trend

france <- france %>% 
  mutate(output_gap = round((France-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "France")

colnames(france) <- c("loggdp", "trend", "outputgap", "time", "country")

# Germany

germany <- hp_prep %>% 
  select(Germany)
ts_germany <- ts(germany, start = c(1996, 1), frequency = 4) 

trend <- hp1(germany, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

germany$trend <- trend$trend

germany <- germany %>% 
  mutate(output_gap = round((Germany-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Germany")

colnames(germany) <- c("loggdp", "trend", "outputgap", "time", "country")

# Greece

greece <- hp_prep %>% 
  select(Greece)
ts_greece <- ts(greece, start = c(1996, 1), frequency = 4) 

trend <- hp1(greece, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

greece$trend <- trend$trend

greece <- greece %>% 
  mutate(output_gap = round((Greece-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Greece")

colnames(greece) <- c("loggdp", "trend", "outputgap", "time", "country")

# Ireland

ireland <- hp_prep %>% 
  select(Ireland)
ts_ireland <- ts(ireland, start = c(1996, 1), frequency = 4) 

trend <- hp1(ireland, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

ireland$trend <- trend$trend

ireland <- ireland %>% 
  mutate(output_gap = round((Ireland-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Ireland")

colnames(ireland) <- c("loggdp", "trend", "outputgap", "time", "country")

# Italy

italy <- hp_prep %>% 
  select(Italy)
ts_italy <- ts(italy, start = c(1996, 1), frequency = 4) 

trend <- hp1(italy, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

italy$trend <- trend$trend

italy<- italy %>% 
  mutate(output_gap = round((Italy-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Italy")

colnames(italy) <- c("loggdp", "trend", "outputgap", "time", "country")

# Latvia 

latvia <- hp_prep %>% 
  select(Latvia)
ts_latvia <- ts(latvia, start = c(1996, 1), frequency = 4) 

trend <- hp1(latvia, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

latvia$trend <- trend$trend

latvia <- latvia %>% 
  mutate(output_gap = round((Latvia-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Latvia")

colnames(latvia) <- c("loggdp", "trend", "outputgap", "time", "country")

# Lithuania

lithuania <- hp_prep %>% 
  select(Lithuania)
ts_lithuania <- ts(lithuania, start = c(1996, 1), frequency = 4) 

trend <- hp1(lithuania, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

lithuania$trend <- trend$trend

lithuania <- lithuania %>% 
  mutate(output_gap = round((Lithuania-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Lithuania")

colnames(lithuania) <- c("loggdp", "trend", "outputgap", "time", "country")

# Luxembourg

luxembourg <- hp_prep %>% 
  select(Luxembourg)
ts_luxembourg <- ts(luxembourg, start = c(1996, 1), frequency = 4) 

trend <- hp1(luxembourg, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

luxembourg$trend <- trend$trend

luxembourg <- luxembourg %>% 
  mutate(output_gap = round((Luxembourg-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Luxembourg")

colnames(luxembourg) <- c("loggdp", "trend", "outputgap", "time", "country")

# Netherlands

netherlands <- hp_prep %>% 
  select(Netherlands)
ts_netherlands <- ts(netherlands, start = c(1996, 1), frequency = 4) 

trend <- hp1(netherlands, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

netherlands$trend <- trend$trend

netherlands <- netherlands %>% 
  mutate(output_gap = round((Netherlands-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Netherlands")

colnames(netherlands) <- c("loggdp", "trend", "outputgap", "time", "country")

# Portugal

portugal <- hp_prep %>% 
  select(Portugal)
ts_portugal <- ts(portugal, start = c(1996, 1), frequency = 4) 

trend <- hp1(portugal, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

portugal$trend <- trend$trend

portugal <- portugal %>% 
  mutate(output_gap = round((Portugal-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Portugal")

colnames(portugal) <- c("loggdp", "trend", "outputgap", "time", "country")

# Slovakia 

slovakia <- hp_prep %>% 
  select(Slovakia)
ts_slovakia <- ts(slovakia, start = c(1996, 1), frequency = 4) 

trend <- hp1(slovakia, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

slovakia$trend <- trend$trend

slovakia <- slovakia %>% 
  mutate(output_gap = round((Slovakia-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Slovakia")

colnames(slovakia) <- c("loggdp", "trend", "outputgap", "time", "country")

# Slovenia

slovenia <- hp_prep %>% 
  select(Slovenia)
ts_slovenia <- ts(slovenia, start = c(1996, 1), frequency = 4) 

trend <- hp1(slovenia, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

slovenia$trend <- trend$trend

slovenia <- slovenia %>% 
  mutate(output_gap = round((Slovenia-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Slovenia")

colnames(slovenia) <- c("loggdp", "trend", "outputgap", "time", "country")

# Spain 

spain <- hp_prep %>% 
  select(Spain)
ts_spain <- ts(spain, start = c(1996, 1), frequency = 4) 

trend <- hp1(spain, lambda = 1600, discard = 0)

colnames(trend) <- c("trend")

spain$trend <- trend$trend

spain <- spain %>% 
  mutate(output_gap = round((Spain-trend)*100,2)) %>% 
  mutate(time = hp_prep$time) %>% 
  mutate(country = "Spain")

colnames(spain) <- c("loggdp", "trend", "outputgap", "time", "country")

### Combining

# list(austria, belgium, croatia, cyprus, estonia, finland, france, germany, greece, ireland, italy, latvia, lithuania, luxembourg, netherlands, portugal, slovakia, slovenia, spain)

output_gap_eea <- do.call(rbind, list(austria, belgium, croatia, cyprus, estonia, finland, france, germany, greece, ireland, italy, latvia, lithuania, luxembourg, netherlands, portugal, slovakia, slovenia, spain))

# Testing graphics

output_gap_eea.1 <- output_gap_eea

output_gap_eea.1$time <- as.yearqtr(output_gap_eea.1$time, format = "%Y-Q%q")

eea_plot <- ggplot(output_gap_eea.1, aes(x = time, y = outputgap, color = country)) + geom_line() + theme_classic()

eea_plot

# Looks good, print data.

write.csv(output_gap_eea.1, "output_gap_eea.csv", row.names = FALSE)

#### As a second measure, I will consider unemployment gaps (where gap is observed value - mean sample) and also forecast gaps (gap = forecast - observed)

## Import data 

# This is only euro level

eea_u.1 <- eea_u

eea_u.1$time <- as.yearqtr(eea_u.1$time, format = "%Y Q%q")

eea_u.1.1 <- eea_u.1 %>%
  mutate(
    meanu_gap = round(u_y0 - mean(u_y0), 2),
    forecast_gap = round(u_f_y2 - u_y0, 2)
  )



# Unemployment gaps compared for eurozone

unemp_plot <- ggplot(eea_u.1.1, aes(x = time)) + geom_line(aes(x = time, y = meanu_gap)) + geom_line(aes(x = time, y = forecast_gap)) + 
  theme_classic()


plot_data <- eea_u.1.1 %>%
  pivot_longer(cols = c(meanu_gap, forecast_gap),
               names_to = "gap_type",
               values_to = "gap_value")


unemp_plot.1 <- ggplot(plot_data, aes(x = time, y = gap_value, color = gap_type)) +
  geom_line(size = 1.5, alpha = 0.8) +
  theme_classic() +
  labs(y = "Unemployment Gap", color = "Gap Type")

unemp_plot.1


# From the plot, unemployment gap derived from mean explains the economic trends better, yet gap measure from forecasts can eliminate hindsight bias.

# Printing data 

write.csv(eea_u.1.1, "unemployment_gap_eea.csv", row.names = FALSE)

##### 

# Now, I will do the same to eurozone countries - dataset is monthly. I will compute monthly averages, then switch to quarterly. 

## import data

u_monthly_eea <- read_excel("Desktop/unemployment monthly euro.xlsx", 
                            na = ":")

u_monthly_eea.1 <- u_monthly_eea %>% 
  select(-contains("...")) %>% 
  mutate(across(.cols = -time, .fns = as.numeric))


u_long <- u_monthly_eea.1 %>%
  pivot_longer(
    cols = -time,           
    names_to = "variable",   
    values_to = "value"      
  )

colnames(u_long) <- c("country", "time", "unemp")

u_wide <- u_long %>%
  pivot_wider(
    names_from = country,   
    values_from = unemp      
  )

u_wide <- u_wide %>% 
  select(-euro_area)


u_wide$time <- as.yearmon(u_wide$time, format = "%Y-%m")

u_wide <- u_wide %>% 
  mutate(index = 1:350)

##### Writing a loop here

# Austria

austria_u <- u_wide %>% 
  select(time, Austria) %>% 
  mutate(ugap = round(Austria - mean(Austria, na.rm = TRUE),2)) %>% 
  mutate(country = "Austria")

colnames(austria_u) <- c("time", "unemp", "ugap", "country")

##### Loop is successful.

countries <- names(u_wide)[names(u_wide) != "time"]

unemp_long <- map_dfr(countries, function(cntry) {
  u_wide %>%
    select(time, unemp = all_of(cntry)) %>%  
    mutate(
      ugap = round(unemp - mean(unemp, na.rm = TRUE), 2),
      country = cntry
    )
})

# Switch to quarterly, average of 3 iterations months.


unemp_long.1 <- unemp_long %>%
  mutate(quarter = as.yearqtr(time))


unemp_long.1 <- unemp_long %>%
  mutate(quarter = as.yearqtr(time)) %>%
  group_by(country, quarter) %>%
  summarise(
    unemp = mean(unemp, na.rm = TRUE),
    ugap = mean(ugap, na.rm = TRUE),
    .groups = "drop"
  )

# replacing NaN

unemp_long.2 <- unemp_long.1 %>%
  mutate(across(-quarter, ~ ifelse(is.nan(.), NA, .)))

colnames(unemp_long.2)[2] <- "time"

unemp_long.2 <- unemp_long.2 %>% 
  filter(country != "index")


euro_ugap <- ggplot(unemp_long.2, aes(x = time, y = ugap, color = country)) +
  geom_line(size = 0.8, alpha = 0.8) +
  theme_classic() + labs(y = "Unemployment Gap", color = "Country", x = "Time")

euro_ugap

# Plot is good, print data

write.csv(unemp_long.2, "unemploymentgapeea.csv", row.names = FALSE)

##### Cleaning HICP for EEA and member countries - monthly

hicp_m_eea <- read_excel("Desktop/hicp_m_eea.xlsx")
View(hicp_m_eea)

hicp_m_eea.1 <- hicp_m_eea

hicp_m_eea.1$time <- as.yearmon(hicp_m_eea.1$time, format = "%Y-%m-%d")

hicp_m_eea.1 <- hicp_m_eea.1 %>% 
  select(-time_1)

hicp_m_eea.2 <- hicp_m_eea.1 %>% 
  mutate(quarter = as.yearqtr(time))

# Running a loop to compute inflation gaps

countries <- names(hicp_m_eea.2)[names(hicp_m_eea.2) != "time"]

hicp_m_eea.3 <- map_dfr(countries, function(cntry) {
  hicp_m_eea.2 %>%
    select(time, hicp_raw = all_of(cntry)) %>%
    mutate(
      hicp = as.numeric(hicp_raw),
      pi_gap = round((hicp - 2), 2),
      country = cntry
    ) %>%
    select(time, hicp, pi_gap, country)
})

# Taking quarterly averages

hicp_m_eea.4 <- hicp_m_eea.3 %>% 
  mutate(quarter = as.yearqtr(time))

hicp_m_eea.4 <- hicp_m_eea.4 %>%
  group_by(country, quarter) %>%
  summarise(
    hicp = round(mean(hicp, na.rm = TRUE), 2),
    pi_gap = round(mean(pi_gap, na.rm = TRUE),2),
    .groups = "drop"
  )

# Replace NaN 

hicp_m_eea.5 <- hicp_m_eea.4 %>%
  mutate(across(-quarter, ~ ifelse(is.nan(.), NA, .)))

colnames(hicp_m_eea.5)[2] <- "time"

# Results look good. Checking plot 

hicp_m_eea.5 <- hicp_m_eea.5 %>% 
  filter(country != "quarter")

hicp_m_eea.6 <- hicp_m_eea.5 %>%
  mutate(country_group = ifelse(country == "euro_area", "Euro area", "Member Countries"))

#pi_gap_plot <- ggplot(hicp_m_eea.5, aes(x = time, y = pi_gap, color = country)) +
  #geom_line(size = 0.8, alpha = 0.5) +
  #theme_classic() + labs(y = "Inflation Gap", color = "Countries", x = "Time")

pi_plot <- ggplot(hicp_m_eea.6, aes(x = time, y = pi_gap, group = country)) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(aes(color = country_group, size = country_group, linetype = country_group)) +
  scale_color_manual(values = c("Euro area" = "red", "Member Countries" = "gray70")) +
  scale_size_manual(values = c("Euro area" = 1.2, "Member Countries" = 0.4)) +
  scale_linetype_manual(values = c("Euro area" = "solid", "Member Countries" = "dotted")) + scale_y_continuous(limits = c(-5, 20)) +
  theme_classic() +
  labs(title = "Inflation Gap by Country", y = "Inflation Gap", x = "Quarter") +
  theme(legend.title = element_blank())

pi_plot

write.csv(hicp_m_eea.5, "inflation_quarterly_eea.csv", row.names = FALSE)

#### Core inflation for EEA and members, cleaning

pce_wide <- read_excel("Desktop/pce_wide.xlsx")
View(pce_wide)

pce_wide.1 <- pce_wide


pce_wide.1$time <- as.yearmon(pce_wide.1$time, format = "%Y-%m-%d")

pce_wide.1 <- pce_wide.1 %>% 
  select(-time_1)

# Running a loop to compute pce inflation gaps

countries_pce <- names(pce_wide.1)[names(pce_wide.1) != "time"]

pce_wide.2 <- map_dfr(countries_pce, function(cntry) {
  pce_wide.1 %>%
    select(time, pce_raw = all_of(cntry)) %>%
    mutate(
      pce = as.numeric(pce_raw),
      pce_gap = round((pce - 2), 2),
      country = cntry
    ) %>%
    select(time, pce, pce_gap, country)
})

# Taking quarterly averages

pce_wide.3 <- pce_wide.2 %>% 
  mutate(quarter = as.yearqtr(time))

pce_wide.4 <- pce_wide.3 %>%
  group_by(country, quarter) %>%
  summarise(
    pce = round(mean(pce, na.rm = TRUE), 2),
    pce_gap = round(mean(pce_gap, na.rm = TRUE),2),
    .groups = "drop"
  )

# Replace NaN 

pce_wide.5 <- pce_wide.4 %>%
  mutate(across(-quarter, ~ ifelse(is.nan(.), NA, .)))

colnames(pce_wide.5)[2] <- "time"

# Plotting PCE

pce_wide.6 <- pce_wide.5 %>%
  mutate(country_group = ifelse(country == "euro_area", "Euro area", "Member Countries"))

pce_plot <- ggplot(pce_wide.6, aes(x = time, y = pce_gap, group = country)) +
  geom_line(aes(color = country_group, size = country_group, linetype = country_group)) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Euro area" = "green", "Member Countries" = "gray70")) +
  scale_size_manual(values = c("Euro area" = 1.2, "Member Countries" = 0.4)) +
  scale_linetype_manual(values = c("Euro area" = "solid", "Member Countries" = "dotted")) + scale_y_continuous(limits = c(-5, 20)) +
  theme_classic() +
  labs(title = "Core Inflation Gap by Country", y = "Core Inflation Gap", x = "Quarter") +
  theme(legend.title = element_blank())

pce_plot

######################################################
######################################################
######################################################

# Compare Inflation Measures

library(patchwork)

inflation_wrapped <- pi_plot + pce_plot 

wrap_plots(inflation_wrapped, ncol = 1)

write.csv(pce_wide.5, "pce_quarterly_eea.csv", row.names = FALSE)

#### ECB forecast data

# survey_hicp

survey_hicp.1 <- survey_hicp

survey_hicp.1 <- survey_hicp.1 %>%
  mutate(time = as.Date(time, format = "%d.%m.%Y"))

survey_hicp.2 <- survey_hicp.1 %>%
  mutate(
    quarter = as.yearqtr(time),
    forecast_gap = round(hicp_f_1y - 2, 2)
  )

survey_hicp.3 <- survey_hicp.2 %>% 
  select(quarter, hicp_f_1y, forecast_gap)

# Save data

write.csv(survey_hicp.3, "forecasthicp.csv", row.names = FALSE)

### Quarterly transformation of Euribor with Wu-Xia (2017) Shadow Rates inserted already.

shadowrateecbmay <- read_excel("Desktop/shadowrateecbmay.xlsx")

shadow_monthly <- shadowrateecbmay %>% 
  mutate(time = as.Date(time, format = "%Y-%m-%d")) %>% 
  mutate(quarter = as.yearqtr(time))

shadow_monthly.1  <- shadow_monthly %>%
  group_by(quarter) %>%
  summarise(
    policy_rate = round(mean(policy_rate, na.rm = TRUE), 2),
    .groups = "drop"
  
  )

colnames(shadow_monthly.1) <- c("time", "pr_1_shadow")

# Print data 

write.csv(shadow_monthly.1, "quarterlyshadowrates.csv", row.names = FALSE)

#### External output gap for euro, Kalman filter HLW (Not included in thesis)

hlw_gap_quarter <- hlwmonthlygap %>% 
  mutate(
    time = as.Date(time, format = "%d.%m.%Y"),
    output_gap_hlw = round(output_ref - 100,2),
    quarter = as.yearqtr(time)
  )

hlw_quarter <- hlw_gap_quarter %>%
  group_by(quarter) %>%
  summarise(
    output_gap_hlw = round(mean(output_gap_hlw, na.rm = TRUE), 2),
    .groups = "drop"
  )

# Printing dataset

colnames(hlw_quarter)[1] <- "time"

write.csv(hlw_quarter, "hlwestimatesquarterly.csv", row.names = FALSE)


###### Cleaning controls and additional data 

# Convergence criterion series (Not included in thesis)

# Data loaded: convergence

convergence_long <- convergence %>%
  pivot_longer(
    cols = -time,           
    names_to = "variable",   
    values_to = "value"      
  )

colnames(convergence_long) <- c("country", "time", "convergence_i")

convergence_long.1 <- convergence_long

convergence_long.1$time <- as.yearqtr(convergence_long.1$time, format = "%Y-Q%q")

convergence_wide <- convergence_long.1 %>%
  pivot_wider(
    names_from = country,   
    values_from = convergence_i      
  )

## Looping 

countries <- names(convergence_wide)[!names(convergence_wide) %in% c("time", "euro_area")]

# Spread will be = member - euro area ####
# Taylor stress will be = member - ecb ###

interest_spread <- purrr::map_dfr(countries, function(cntry) {
  convergence_wide %>%
    select(time, euro_area = euro_area, member = all_of(cntry)) %>%
    mutate(
      rate_spread = as.numeric(member) - as.numeric(euro_area),
      country = cntry
    ) %>%
    select(time, country, rate_spread)
})

### Print dataset

write.csv(interest_spread, "euro_area_interest_spread.csv", row.names = FALSE)


#### Bond Spreads data: spread_eu (not included in dataset)

spread_eu <- spread_eu %>% 
  select(-contains("..."))

spread_long <- spread_eu %>%
  pivot_longer(
    cols = -time,           
    names_to = "variable",   
    values_to = "value"      
  )

colnames(spread_long) <- c("maturity", "time", "yield")

spread_wide <- spread_long %>%
  pivot_wider(
    names_from = maturity,   
    values_from = yield      
  )


spread_wide$time <- as.yearqtr(spread_wide$time, format = "%Y-Q%q")


colnames(spread_wide) <- c("time", "one_year", "ten_year")

spread_wide.1 <- spread_wide %>% 
  mutate(eea_bond_spread = ten_year - one_year) %>% 
  select(time, eea_bond_spread)

# Print data

write.csv(spread_wide.1, "ten_one_year_spread_eea.csv", row.names = FALSE)



################# BIG MERGE #################

# Commodity index: 1996Q1:2025Q2 = commodity
# Equity index: 1986Q4:2025Q2 = 
# Gov debt to GDP: 2000Q1:2024Q4
# Forecast inflation HICP (PCE forecasts were scarce), 1 year ahead: 1999Q4:2025Q4
# HICP (for GMM as well): 1992Q1:2025Q2
# HLW, external output gap estimate: 1997Q1:2022Q3
# HP Filtered output gap estimate, my calculation (take first 4 years as burner): 1995Q1:2024Q4
# M2 growth Y-o-Y: 1981Q1:2025Q1 
# M3 growth Y-o-Y: 1981Q1:2025Q1
# PCE Y-o-Y delta: 1997Q1:2025Q2
# Shadow rate (Wu-Xia-2016) inserted EURIBOR: 1999Q1:2025Q1
# Bond spread for EEA (10year - 1 year): 2004Q3:2024Q4
# Constructed unemployment gap measures: 1999Q1:2025Q2

########################################################################################################

# Processing - Commodity

commodity <- commodity %>% 
  select(-date)

commodity$time <- as.yearqtr(commodity$time, format = "%YQ%q")

# Processing - Equity

equity <- equity %>% 
  select(-date)

equity$time <- as.yearqtr(equity$time, format = "%YQ%q")


# Processing - govdebt

govdebt <- govdebt %>% 
  select(-date)

govdebt$time <- as.yearqtr(govdebt$time, format = "%YQ%q")

# Processing - forecasthicp - no need

# Processing - hicp

hicp <- hicp_quarterly_eea %>% 
  filter(country == "euro_area") %>% 
  select(-country)

# Processing - hlw - no need

# Processing - hp_eea

hp_eea$time <- as.yearqtr(hp_eea$time, format = "%Y-Q%q")

hp_eea <- hp_eea %>% 
  select(time, output_gap)

colnames(hp_eea) <- c("time", "output_gap_hp")

# Processing - m2

m2$time <- as.yearqtr(m2$time, format = "%YQ%q")

m2 <- m2 %>% 
  select(-date)

# Processing - m3

m3$time <- as.yearqtr(m3$time, format = "%YQ%q")

m3 <- m3 %>% 
  select(-date)

# Processing - pce

pce <- pce %>% 
  filter(country == "euro_area") %>% 
  select(-country)

# Processing - shadow_rate - no need

# Processing - bond_spread - no need

# Processing - unemp_gap

unemp_gap <- unemp_gap %>% 
  select(time, u_y0, u_f_y1, u_f_y2, meanu_gap,forecastu_gap)

########################################################################################################

######## Variable list ################################################################################

## comm_ind_g : Commodity index growth Y-o-Y, quarterly. Euro area.
## stoxx_50_index_g: Euro top 50 stock by capitalization index growth Y-o-Y, quarterly.
## gov_debt: Government debt to GDP ratio, in percentages. Quarterly, Euro area.
## hicp_f_1y: HICP growth forecast, contemporaneous mean estimate. 1 year ahead, Euro area. 
## forecast_gap: hicp_f_1y - 2. ( 2 = inflation target of ECB). In percentages.
## hicp: CPI index (includes food and energy) of euro area, Y-o-Y change. 
## pi_gap: hicp - 2. (2 = inflation target of ECB). In percentages.
## output_gap_hlw: HLW estimates of Euro area output gap.
## m2_g: M2 growth Y-o-Y. Euro area, in percentages.
## m3_g: M3 growth Y-o-Y. Euro area, in percentages.
## pce: Core Inflation (HICPX: Excluding food and energy prices.) of euro area, Y-o-Y change.
## pce_gap: pce - 2. (2 = inflation target of ECB). In percentages.
## pr_1_shadow: Policy rate of the ECB, where between 2004Q1 and 2022Q3 Wu-Xia (2016) shadow rates were inserted to capture ZLB periods.
## eea_bond_spread: 10 year yield - 1 year yield of AAA rated government bonds, euro area. Potential instrument for post 2012.
## u_y0: Unemployment rate, Euro Area.
## u_f_y1: Forecasted unemployment rate, 1 year ahead. Euro area.
## u_f_y2: Forecasted unemployment rate, 2 years ahead. Euro area.
## meanu_gap: Observed  unemployment rate - NAIRU (Non-accelerating inflation rate of unemployment). NAIRU is an non-observable variable.
#### To compute NAIRU (structural estimation is beyond the scope of this thesis), an assumption that takes the mean value of unemployment rates of the whole time period as NAIRU to compute unemployment gap.
## forecastu_gap: u_f_y2 - u_y0. 2 year ahead forecast - observed unemployment. This variable can be used as a potential instrument for output gap.

################################################################################################################################


# Merging.

# pce # hlw # hicp # hicpforecast # shadow # unemp # spread = time is character. Time should be in qtr. 

pce.1 <- pce

pce.1$time <- as.yearqtr(pce.1$time, format = "%Y Q%q")

pce <- pce.1


hlw.1 <- hlw

hlw.1$time <- as.yearqtr(hlw.1$time, format = "%Y Q%q")

hlw <- hlw.1


hicp.1 <- hicp

hicp.1$time <- as.yearqtr(hicp.1$time, format = "%Y Q%q")

hicp <- hicp.1


forecasthicp.1 <- forecasthicp

forecasthicp.1$time <- as.yearqtr(forecasthicp.1$time, format = "%Y Q%q")

forecasthicp <- forecasthicp.1


quarterlyshadowrates.1 <- quarterlyshadowrates

quarterlyshadowrates.1$time <- as.yearqtr(quarterlyshadowrates.1$time, format = "%Y Q%q")

quarterlyshadowrates <- quarterlyshadowrates.1


unemp_gap.1 <- unemp_gap

unemp_gap.1$time <- as.yearqtr(unemp_gap.1$time, format = "%Y Q%q")

unemp_gap <- unemp_gap.1


bond_spread.1 <- bond_spread

bond_spread.1$time <- as.yearqtr(bond_spread.1$time, format = "%Y Q%q")

bond_spread <- bond_spread.1

df_list <- list(
  m2,
  m3,
  commodity,
  equity,
  hp_eea,
  pce,
  hlw,
  hicp,
  forecasthicp,
  quarterlyshadowrates,
  unemp_gap,
  govdebt,
  bond_spread
)

merged_data_ecb <- reduce(df_list, left_join, by = "time")

# Perfect, almost ready for estimation. 

merged_data_ecb <- merged_data_ecb %>% 
  mutate(time_index = 1:178)

# Semi-clean dataset 

write.csv(merged_data_ecb, "macroestimation_semi_clean.csv", row.names = FALSE)

# Filtered dataset: Time index = 73:177

ecb_filtered <- merged_data_ecb %>% 
  filter(time_index >= 73 & time_index <= 177)

write.csv(ecb_filtered, "macroestimation_clean.csv", row.names = FALSE)

# ===================
# Checkpoint June 2025 
# Construct a Taylor style moving average, 4 quarter as a proxy for expected inflation

# June taylor - dataset checkpoint. For the moving average, 1997Q1 and t = 65 

# Subsetting dataset. 

# Moving average will be implemented on HICP and PCE. 
# Also shift 1 quarter the inflation. 

# For moving average

library(slider)

taylor <- june.taylor %>% 
  filter(time_index >= 65)


taylor <- taylor %>%
  mutate(
    rolling_hicp = slide_mean(hicp, before = 3, complete = TRUE),
    rolling_pce  = slide_mean(pce, before = 3, complete = TRUE)
  )

taylor <- taylor %>%
  mutate(
    fisher_rate = pr_1_shadow - lead(hicp, n = 1)
  )

taylor.1 <- taylor %>% 
  mutate(
    rolling_hicp_gap = rolling_hicp - 2,
    rolling_pce_gap = rolling_pce - 2
  )


# Graph for taylor vs real data 1 year ahead, inflation expectation 

taylor.2 <- taylor.1 %>%
  mutate(
    year = as.numeric(substr(time, 1, 4)),
    quarter = as.numeric(substr(time, 7, 7)),
    time_date = lubridate::yq(paste(year, quarter))
  )


# Graph 

# Taylor's (1993) takes a rolling average of 4 previous quarters of inflation to proxy for expected inflation 1 year ahead. Real time data for 1 year ahead is from Survey of Professional Forecasters, ECB. 
inflation_expectation <- ggplot(data = taylor.2, aes(x = time_date)) +
  geom_line(aes(y = forecast_gap, color = "Real Time Data"), na.rm = TRUE, size = 1, alpha = 0.8) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(aes(y = rolling_hicp_gap, color = "Taylor's (1993) Method"), na.rm = TRUE, size = 1, alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(
    title = "Taylor's (1993) Method vs. Survey Data for Inflation Expectations",
    y = "Expected Inflation Gap (%)",
    x = NULL,
    color = "",
    caption = ""
  ) +
  scale_color_manual(
    values = c("Real Time Data" = "green", "Taylor's (1993) Method" = "red")
  )

inflation_expectation

ggsave("inflation_exp.png", plot = inflation_expectation, 
  width = 10, height = 6, dpi = 300, bg = "white")

taylor_before_crisis <- taylor.2 %>% 
  filter(time_index >= 73 & time_index < 125)

taylor_after_crisis <- taylor.2 %>% 
  filter(time_index >= 125)



write.csv(taylor.2, "taylorestimation_june.csv", row.names = FALSE)
write.csv(taylor_before_crisis, "taylorestimation_june_b2012.csv", row.names = FALSE)
write.csv(taylor_after_crisis, "taylorestimation_june_a2012.csv", row.names = FALSE)

# Euro level done 

# adoption of euro: https://economy-finance.ec.europa.eu/euro/eu-countries-and-euro_en#:~:text=France%20is%20a%20founding%20member,euro%20on%201%20January%201999.

############## Member Countries #######################

# load hicp first 
# Jun 13, checkpoint ###############
# Croatia problematic, malta problematic

hicp_quarterly_eea.1 <- hicp_quarterly_eea %>% 
  filter(!country %in% c("Croatia", "Malta", "euro_area"))

hicp_quarterly_eea.2 <- hicp_quarterly_eea.1 %>% 
  filter(time >= "1997 Q1")

library(tsibble)

hicp_quarterly_eea.2 <- hicp_quarterly_eea.2 %>%
  mutate(time_qtr = tsibble::yearquarter(time))

hicp_quarterly_eea.2 <- hicp_quarterly_eea.2 %>%
  group_by(country) %>%
  arrange(country, time_qtr) %>%
  mutate(rolling_hicp = slide_dbl(hicp, mean, .before = 3, .complete = TRUE)) %>%
  ungroup()


hicp_quarterly_eea.2 <- hicp_quarterly_eea.2 %>% 
  mutate(rolling_hicp = round(rolling_hicp, 2)) %>% 
  mutate(rolling_hicp_gap = rolling_hicp - 2)

# Final = hicp_quarterly_eea.2

# Processing gdp and output gap

output_gap__eea <- output_gap_eea %>% 
  filter(!country %in% c("Croatia", "Malta", "euro_area"))

output_gap__eea.1 <- output_gap__eea %>%
  mutate(gdp_nominal = exp(loggdp))


output_and_pi <- left_join(hicp_quarterly_eea.2, output_gap__eea.1, by = c("country", "time"))

output_and_pi <- output_and_pi %>% 
  select(-loggdp, -trend)

# load shadow rate

member_dataset <- left_join(output_and_pi, quarterlyshadowrates, by = "time")

member_dataset <- member_dataset %>% 
  filter(time >= "1999 Q1")


member_dataset.1 <- member_dataset %>%
  group_by(country) %>%
  arrange(country, time) %>%
  mutate(
    fisher_rate = pr_1_shadow - lead(hicp, n = 1),
    rstar = mean(fisher_rate, na.rm = TRUE),
    alpha = rstar + 2
  ) %>%
  ungroup()

# Appending Geopolygon objects

eurostat_geodata <- get_eurostat_geospatial(
  nuts_level = 0,     
  resolution = "10",  
  year = 2016,        
  output_class = "sf" 
)

country_codes <- tibble::tibble(
  country = c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", "Ireland", 
              "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain"),
  geo = c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "EL", "IE", 
          "IT", "LV", "LT", "LU", "NL", "PT", "SK", "SI", "ES")
)

member_dataset.1 <- left_join(member_dataset.1, country_codes, by = "country")


geodata <- eurostat_geodata %>% 
  select(geo, geometry)

member_dataset.2 <- left_join(member_dataset.1, geodata, by = "geo")

write.csv(member_dataset.2, "geospatial_member.csv", row.names = FALSE)

# Calculate real gdp growth yoy euro level 

euro_real_long <- euro_real_gdp %>%
  pivot_longer(
    cols = -time,           
    names_to = "variable",   
    values_to = "value"      
  )

colnames(euro_real_long) <- c("country", "time", "level_gdp")

euro_real_long.1 <- euro_real_long %>%
  mutate(time_qtr = as.yearqtr(time, format = "%Y-Q%q")) %>%   
  group_by(country) %>%                                       
  arrange(country, time_qtr) %>%                              
  mutate(rgdp_g = (level_gdp / lag(level_gdp, 4) - 1) * 100) %>%       
  ungroup()

colnames(euro_real_long.1)[4] <- "timee"

euro_real_long.2 <- euro_real_long.1 %>% 
  select(country, timee, rgdp_g)

colnames(euro_real_long.2)[2] <- "time"

euro_real_long.3 <- euro_real_long.2 %>%
  filter(time >= "1999 Q1")

member_dataset.3 <- member_dataset.2 %>%
  mutate(time = as.yearqtr(time, format = "%Y Q%q"))

member_dataset.4 <- left_join(member_dataset.3, euro_real_long.3, 
                              by = c("time", "country"))


# To retain the next step in code, I rename the dataset.

twelve_country <- member_dataset.4
# Big checkpoint
# Access dates: Austria: 1999Q1, Belgium: 1999Q1, Cyprus: 2008Q1, Estonia: 2011Q1, Finland: 1999Q1, France: 1999Q1, Germany: 1999Q1, Greece: 2002Q1, Ireland: 1999Q1, Italy: 1999Q1, Latvia: 2014Q1, Lithuania: 2014Q1, Luxembourg: 1999Q1, Netherlands: 1999Q1, Portugal: 1999Q1, Slovakia: 2009Q1, Slovenia: 2007Q1, Spain: 1999Q1
# initial_access <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain") 


# twelve_country <- member_dataset.4 %>% 
  # filter(country %in% initial_access)


# write.csv(twelve_country, "initial_access.csv", row.names = FALSE)

# =============================================================================================================================

##########################################################################################
################## SIMULATION TAYLOR RATES PER MEMBER ####################################
##########################################################################################


#############################################
# Simulation 1, first GMM regression coefficients
rho <- 0.957
phi_pi <- 0.170
phi_y <- 0.097
#############################################


twelve_country <- twelve_country %>%
  arrange(country, time_qtr)

# Writing a loop
compute_taylor <- function(df_country) {
  i_taylor <- numeric(nrow(df_country))
  
  # initializing
  i_taylor[1] <- df_country$pr_1_shadow[1]
  
  # define alpha
  alpha_country <- df_country$alpha[1]
  
  # Recursive function
  for (t in 2:nrow(df_country)) {
    lag_i <- i_taylor[t - 1]
    infl_gap <- df_country$rolling_hicp_gap[t]
    out_gap  <- df_country$outputgap[t]
    
    taylor_target <- alpha_country + phi_pi * infl_gap + phi_y * out_gap
    
    i_taylor[t] <- rho * lag_i + (1 - rho) * taylor_target
  }
  
  return(i_taylor)
}

# Apply per country
twelve_country <- twelve_country %>%
  group_by(country) %>%
  mutate(i_taylor_country = compute_taylor(cur_data_all())) %>%
  ungroup()


############## Accession ############## ############## ############## ############## 

accession_dates <- tibble::tibble(
  country = c("Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Greece", 
              "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Netherlands", "Portugal", 
              "Slovakia", "Slovenia", "Spain"),
  join_date = as.yearqtr(c("1999 Q1", "1999 Q1", "2008 Q1", "2011 Q1", "1999 Q1", "1999 Q1", 
                           "1999 Q1", "2002 Q1", "1999 Q1", "1999 Q1", "2014 Q1", "2014 Q1", 
                           "1999 Q1", "1999 Q1", "1999 Q1", "2009 Q1", "2007 Q1", "1999 Q1"))
)

twelve_country.12 <- twelve_country %>% 
  left_join(accession_dates, by = "country") %>% 
  mutate(
    active_member = time >= join_date,  # Boolean: joined or not
    stress_shaded = ifelse(active_member, stress_quadratic, NA)  
  )



############## Accession ############## ############## ############## ############## 

# Compute stress variable
twelve_country <- twelve_country %>%
  mutate(stress = abs(pr_1_shadow - i_taylor_country),
         stress_0abs = pr_1_shadow - i_taylor_country,
         stress_quadratic = (pr_1_shadow - i_taylor_country)^2)

stress_plot_abs1 <- ggplot(twelve_country, aes(x = time_qtr, y = stress, color = country), size = 0.6, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Monetary Stress over Time, Absolute Value",
       x = NULL,
       y = "Stress Variable (%)") +
  theme_bw() +
  theme(legend.position = "bottom")

# no need = stress_plot_abs1


stress_plot_abs0 <- ggplot(twelve_country, aes(x = time_qtr, y = stress_0abs, color = country), size = 0.6, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(size = 1) +
  labs(title = "Monetary Stress over Time, No Absolute Value",
       x = NULL,
       y = "Stress Variable (%)") +
  theme_bw() +
  theme(legend.position = "bottom")

# no need = stress_plot_abs0

stress_plot_quadratic <- ggplot(twelve_country, aes(x = time_qtr, y = stress_quadratic, color = country), size = 0.6, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "Quadratic Stress over Time",
       x = NULL,
       y = "Quadratic Stress") +
  theme_bw() +
  theme(legend.position = "bottom")

# no need = stress_plot_quadratic

#ggsave("quadratic_stress_euro.png", plot = stress_plot_quadratic, 
       # width = 10, height = 6, dpi = 300, bg = "white")

belgium_df <- twelve_country %>% filter(country == "Belgium")

belgium_stress <- ggplot(belgium_df, aes(x = time_qtr)) +
  geom_line(aes(y = i_taylor_country, color = "Taylor Rate"), size = 1) +
  geom_line(aes(y = pr_1_shadow, color = "ECB Shadow Rate"), size = 1) +
  labs(title = "Belgium: Taylor Rate vs ECB Shadow Rate",
       x = "Time (Quarter)",
       y = "Interest Rate",
       color = "") + theme(legend.position = "bottom") + 
  theme_classic()

belgium_stress

ggsave("belgium_stress.png", plot = belgium_stress, 
       width = 10, height = 6, dpi = 300, bg = "white")


stress_grid <- ggplot(twelve_country, aes(x = time_qtr)) +
  geom_line(aes(y = i_taylor_country, color = "Taylor Rate"), size = 1) +
  geom_line(aes(y = pr_1_shadow, color = "ECB Shadow Rate"), size = 1) +
  labs(title = "Taylor Rate vs ECB Shadow Rate — All Member Countries",
       x = "Time (Quarter)",
       y = "Interest Rate (%)",
       color = "") +
  facet_wrap(~ country, ncol = 3) +  # adjust ncol (4 x 3 grid for 12 countries)
  theme_minimal() +
  theme(legend.position = "bottom")

stress_grid

ggsave("stress_grid.png", plot = stress_grid, 
       width = 10, height = 6, dpi = 300, bg = "white")

############### Suppressing countries before accession dates #################

twelve_country.12 <- twelve_country.12 %>%
  mutate(
    stress_abs_shaded  = ifelse(active_member, stress, NA),
    stress_raw_shaded  = ifelse(active_member, stress_0abs, NA),
    stress_quad_shaded = ifelse(active_member, stress_quadratic, NA)
  )



# Absolute stress
stress_plot_abs1 <- ggplot(twelve_country.12, aes(x = time, y = stress_abs_shaded, color = country)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    title = "Absolute Stress Over Time",
    x = NULL,
    y = "Absolute Stress"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

stress_plot_abs1

ggsave("stress_plot_abs1.png", plot = stress_plot_abs1, 
       width = 10, height = 6, dpi = 300, bg = "white")


# Level stress
stress_plot_abs0 <- ggplot(twelve_country.12, aes(x = time, y = stress_raw_shaded, color = country)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    title = "Level Stress Over Time",
    x = NULL,
    y = "Level Stress"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

stress_plot_abs0

ggsave("stress_plot_level.png", plot = stress_plot_abs0, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Quadratic stress
stress_plot_quadratic <- ggplot(twelve_country.12, aes(x = time, y = stress_quad_shaded, color = country)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    title = "Quadratic Stress Over Time",
    x = NULL,
    y = "Quadratic Stress"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

stress_plot_quadratic

ggsave("quadratic_dynamic.png", plot = stress_plot_quadratic, 
       width = 10, height = 6, dpi = 300, bg = "white")


############ Core and Peripheral distinction ############################################################

core_countries <- c(
  "Austria", "Belgium", "France", "Finland", "Germany", "Netherlands", "Italy"
)

peripheral_countries <- c(
  "Greece", "Ireland", "Portugal", "Spain",
  "Cyprus", "Estonia", "Latvia", "Lithuania", "Slovakia", "Slovenia"
)

twelve_country.13 <- twelve_country.12 %>%
  mutate(country_group = case_when(
    country %in% core_countries ~ "Core",
    country %in% peripheral_countries ~ "Periphery",
    TRUE ~ NA_character_
  ))

stress_grouped <- twelve_country.13 %>%
  filter(active_member, !is.na(country_group)) %>%  # Exclude inactive & NA group
  group_by(time, country_group) %>%
  summarise(
    avg_stress_abs  = mean(stress, na.rm = TRUE),
    avg_stress_raw  = mean(stress_0abs, na.rm = TRUE),
    avg_stress_quad = mean(stress_quadratic, na.rm = TRUE),
    .groups = "drop"
  )


grouped_absolute <- ggplot(stress_grouped, aes(x = time, y = avg_stress_abs, color = country_group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Absolute Monetary Stress by Group",
    y = "Absolute Stress",
    x = NULL,
    color = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

grouped_absolute 

ggsave("grouped_levelstress.png", plot = grouped_absolute, 
       width = 10, height = 6, dpi = 300, bg = "white")


grouped_abs0 <- ggplot(stress_grouped, aes(x = time, y = avg_stress_raw, color = country_group)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(size = 1.2) +
  labs(
    title = "Level Monetary Stress by Group of Countries",
    y = "Level Stress",
    x = NULL,
    color = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

grouped_abs0

ggsave("grouped_level.png", plot = grouped_abs0, 
       width = 10, height = 6, dpi = 300, bg = "white")

grouped_quadratic <- ggplot(stress_grouped, aes(x = time, y = avg_stress_quad, color = country_group)) +
  geom_line(size = 1.2) +
  labs(
    title = "Quadratic Stress by Group of Countries Over Time",
    y = "Quadratic Stress",
    x = NULL,
    color = "Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

grouped_quadratic

ggsave("grouped_quadratic.png", plot = grouped_quadratic, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Adding real gdp level now to twelve_country.13


euro_real_long.4 <- euro_real_long

colnames(euro_real_long.4)[3] <- "realgdp_mil"

euro_real_long.4 <- euro_real_long.4 %>%
  mutate(time = as.yearqtr(time, format = "%Y-Q%q"))

twelve_country.14 <- left_join(twelve_country.13, euro_real_long.4, by = c("country", "time"))


twelve_country.15 <- twelve_country.14 %>%
  group_by(time) %>%
  mutate(
    total_gdp_active = sum(realgdp_mil[active_member], na.rm = TRUE),
    dynamic_weight = ifelse(
      active_member,
      realgdp_mil / total_gdp_active,
      NA_real_ 
    )
  ) %>%
  ungroup()

twelve_country.16 <- twelve_country.15 %>%
  mutate(
    weighted_stress_abs  = dynamic_weight * stress,
    weighted_stress_raw  = dynamic_weight * stress_0abs,
    weighted_stress_quad = dynamic_weight * stress_quadratic
  )


euro_aggregate_stress <- twelve_country.16 %>%
  filter(active_member) %>%
  group_by(time) %>%
  summarise(
    euro_stress_abs  = sum(weighted_stress_abs, na.rm = TRUE),
    euro_stress_raw  = sum(weighted_stress_raw, na.rm = TRUE),
    euro_stress_quad = sum(weighted_stress_quad, na.rm = TRUE),
    .groups = "drop"
  )


euro_aggregate_stress_equal <- twelve_country.16 %>%
  filter(active_member) %>%
  group_by(time) %>%
  summarise(
    euro_stress_abs_equal  = mean(stress_abs_shaded, na.rm = TRUE),
    euro_stress_raw_equal  = mean(stress_raw_shaded, na.rm = TRUE),
    euro_stress_quad_equal = mean(stress_quad_shaded, na.rm = TRUE),
    .groups = "drop"
  )

aggregate_comparison <- left_join(euro_aggregate_stress, euro_aggregate_stress_equal, by = "time")


aggr_quadratic_stress <- ggplot(aggregate_comparison, aes(x = time)) +
  geom_line(aes(y = euro_stress_quad, color = "GDP-weighted"), size = 1.2) +
  geom_line(aes(y = euro_stress_quad_equal, color = "Equal-weighted"), size = 1.2) +
  labs(
    title = "Euro Area — GDP-weighted vs Equal Weighted Quadratic Monetary Stress",
    x = NULL,
    y = "Quadratic Stress",
    color = "Aggregation Method"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("GDP-weighted" = "darkblue", "Equal-weighted" = "orange"))

aggr_quadratic_stress

ggsave("aggr_quadratic_stress1.png", plot = aggr_quadratic_stress, 
       width = 10, height = 6, dpi = 300, bg = "white")

aggr_stress_abs <- ggplot(aggregate_comparison, aes(x = time)) +
  geom_line(aes(y = euro_stress_abs, color = "GDP-weighted"), size = 1.2) +
  geom_line(aes(y = euro_stress_abs_equal, color = "Equal-weighted"), size = 1.2) +
  labs(
    title = "Euro Area — GDP-weighted vs Equal Weighted Absolute Monetary Stress",
    x = NULL,
    y = "Absolute Stress",
    color = "Aggregation Method"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("GDP-weighted" = "darkblue", "Equal-weighted" = "orange"))

aggr_stress_abs

ggsave("aggr_stress_abs1.png", plot = aggr_stress_abs, 
       width = 10, height = 6, dpi = 300, bg = "white")

aggr_stress_raw <- ggplot(aggregate_comparison, aes(x = time)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(aes(y = euro_stress_raw, color = "GDP-weighted"), size = 1.2) +
  geom_line(aes(y = euro_stress_raw_equal, color = "Equal-weighted"), size = 1.2) +
  labs(
    title = "Euro Area — GDP-weighted vs Equal Weighted Level Monetary Stress",
    x = NULL,
    y = "Level Stress",
    color = "Aggregation Method"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("GDP-weighted" = "darkblue", "Equal-weighted" = "orange"))

aggr_stress_raw

ggsave("aggregatestressraw1.png", plot = aggr_stress_raw, 
       width = 10, height = 6, dpi = 300, bg = "white")


# Line graph animation 

euro_aggregate_stress <- euro_aggregate_stress %>%
  mutate(time_num = as.numeric(time))

line_anim <- ggplot(euro_aggregate_stress, aes(x = time_num, y = euro_stress_quad)) +
  geom_line(color = "blue", size = 1, alpha = 0.5) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Euro Area — GDP-weighted Quadratic Monetary Stress",
    subtitle = "Simulated Taylor Rule vs ECB",
    x = "Quarter",
    y = "Weighted Quadratic Stress"
  ) +
  theme_bw() +
  transition_reveal(time_num)

line_anim

############## ############## ############## ############## ############## ############## 

# Simulation 1 = animation

library(gganimate)
library(sf)


#### Main animation here.
twelve_country_sf <- st_as_sf(twelve_country.12)

range_vals <- range(twelve_country$stress_quadratic, na.rm = TRUE)

gifplot <- ggplot(twelve_country_sf) +
  geom_sf(aes(fill = stress_shaded), color = "black") +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log1p",
    limits = range_vals,
    na.value = "gray70", 
    guide = guide_colorbar(barwidth = 20, barheight = 0.5)
  ) +
  labs(
    title = 'Quadratic Stress — {closest_state}',
    subtitle = "ECB vs Taylor Rule — Country Level",
    fill = ""
  ) +
  theme_void() +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 65)) +
  theme(legend.position = "bottom") +
  transition_states(time_qtr, state_length = 1, wrap = FALSE) +
  ease_aes('linear')

anim <- animate(
  gifplot,
  nframes = length(unique(twelve_country$time_qtr)) * 5,
  fps = 20,
  width = 800,
  height = 600
)

anim

# Combining gifs

library(magick)

# Shared parameters
nframes_total <- 200
fps_val <- 20

# Rendering map
map_anim <- animate(gifplot, nframes = nframes_total, fps = fps_val, width = 800, height = 600)

# Rendedring line 
line_anim_rendered <- animate(line_anim, nframes = nframes_total, fps = fps_val, width = 800, height = 600)

anim_save("map_anim.gif", animation = map_anim)
anim_save("line_anim.gif", animation = line_anim_rendered)


gif_map  <- image_read("map_anim.gif")
gif_line <- image_read("line_anim.gif")

n <- min(length(gif_map), length(gif_line))

# Appending frames now.
combined_gif <- image_append(c(gif_map[1], gif_line[1]), stack = FALSE)

# Looping frames.
for (i in 2:n) {
  combined_frame <- image_append(c(gif_map[i], gif_line[i]), stack = FALSE)
  combined_gif <- c(combined_gif, combined_frame)
}


image_write(combined_gif, "combined_stress_animation.gif")

image_optimized <- image_animate(combined_gif, fps = 10)
image_write(image_optimized, "optimized_combined.gif")
image_optimized
combined_gif


#######################################################################################################################
# 2012 BEFORE 2012 AFTER - quadratic stress first

cutoff_qtr <- yearquarter("2012 Q1")

# Cutoff point generation.
twelve_country.17 <- twelve_country.16 %>%
  mutate(period = factor(ifelse(time_qtr <= cutoff_qtr, "Before 2012Q1", "After 2012Q1"),
                         levels = c("Before 2012Q1", "After 2012Q1")))

# Aggregating.
avg_stress_by_period.1 <- twelve_country.17 %>%
  group_by(country, period) %>%
  summarise(
    avg_abs_stress = mean(stress_abs_shaded, na.rm = TRUE),
    avg_quadratic_stress = mean(stress_quad_shaded, na.rm = TRUE),
    avg_stress = mean(stress_raw_shaded, na.rm = TRUE),
    geometry = first(geometry)
  ) %>%
  ungroup()

avg_stress_by_period.1 <- avg_stress_by_period.1 %>% 
  st_as_sf()

# Average Quadratic stress

perfect_map_quad <- ggplot(avg_stress_by_period.1) +
  geom_sf(aes(fill = avg_quadratic_stress), color = "black") +
  scale_fill_viridis_c(option = "plasma", trans = "log1p") +
  labs(title = "                                                                         Average Quadratic Stress",
       subtitle = "",
       fill = "") +
  theme_void() +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 65)) +
  facet_wrap(~ period) +  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(2, "cm")
  )

perfect_map_quad

ggsave("quadraticstressaveragemap.png", plot = perfect_map_quad, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Average Absolute stress 

perfect_map_abs <- ggplot(avg_stress_by_period.1) +
  geom_sf(aes(fill = avg_abs_stress), color = "black") +
  scale_fill_viridis_c(option = "plasma", trans = "log1p") +
  labs(title = "                                                                             Average Absolute Stress",
       subtitle = "",
       fill = "") +
  theme_void() +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 65)) +
  facet_wrap(~ period) +  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(2, "cm")
  )

perfect_map_abs

ggsave("absolutestressaveragemap.png", plot = perfect_map_abs, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Average Level Stress

# Positive stress = Brighter, Deflationary ; Negative stress = Darker, Inflationary
perfect_map_level <- ggplot(avg_stress_by_period.1) +
  geom_sf(aes(fill = avg_stress), color = "black") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "                                                                                   Average Level Stress",
       subtitle = "",
       fill = "") +
  theme_void() +
  coord_sf(xlim = c(-10, 35), ylim = c(35, 65)) +
  facet_wrap(~ period) + 
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(2, "cm")
  )

perfect_map_level

ggsave("levelstressaveragemap.png", plot = perfect_map_level, 
       width = 10, height = 6, dpi = 300, bg = "white")


# Now summarizing at multiple plots wrapped, core and periphery
# Starts from twelve_country.16 

periphery_core <- twelve_country.16 %>%
  mutate(
    country_group = case_when(
      country %in% core_countries ~ "Core",
      country %in% peripheral_countries ~ "Periphery",
      TRUE ~ NA_character_
    ),
    period = factor(
      ifelse(time_qtr <= yearquarter("2012 Q1"), "Before 2012Q1", "After 2012Q1"),
      levels = c("Before 2012Q1", "After 2012Q1")
    )
  )

# Removing NA country_group now.
periphery_core.1 <- periphery_core %>%
  filter(active_member, !is.na(country_group)) %>%  
  group_by(country_group, period) %>%
  summarise(
    weighted_abs  = sum(weighted_stress_abs, na.rm = TRUE),
    weighted_raw  = sum(weighted_stress_raw, na.rm = TRUE),
    weighted_quad = sum(weighted_stress_quad, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot longer for plotting...
periphery_core.2 <- periphery_core.1 %>%
  pivot_longer(
    cols = starts_with("weighted_"),
    names_to = "stress_type",
    values_to = "avg_weighted_stress"
  ) %>%
  mutate(
    stress_type = recode(
      stress_type,
      "weighted_abs" = "Absolute",
      "weighted_raw" = "Level",
      "weighted_quad" = "Quadratic"
    )
  )

weighted_facet_plot <- ggplot(periphery_core.2, aes(x = country_group, y = avg_weighted_stress, fill = country_group)) +
  geom_col(width = 0.6, show.legend = FALSE) + geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  facet_grid(stress_type ~ period, scales = "free_y") +
  labs(
    title = "",
    x = NULL,
    y = "GDP-weighted Average Stress"
  ) +
  scale_fill_manual(values = c("Core" = "#1f78b4", "Periphery" = "#e31a1c")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(4, "lines")  # ← add this
  )

weighted_facet_plot

ggsave("weighted_stress_plot.png", plot = weighted_facet_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Now without weight. ################################################################################

periphery_core_unweighted <- twelve_country.16 %>%
  mutate(
    country_group = case_when(
      country %in% core_countries ~ "Core",
      country %in% peripheral_countries ~ "Periphery",
      TRUE ~ NA_character_
    ),
    period = factor(
      ifelse(time_qtr <= yearquarter("2012 Q1"), "Before 2012Q1", "After 2012Q1"),
      levels = c("Before 2012Q1", "After 2012Q1")
    )
  )

# Filtering active eurozone members and drop the NA group.
periphery_core_unweighted.1 <- periphery_core_unweighted %>%
  filter(active_member, !is.na(country_group)) %>%
  group_by(country_group, period) %>%
  summarise(
    avg_abs  = mean(stress, na.rm = TRUE),
    avg_raw  = mean(stress_0abs, na.rm = TRUE),
    avg_quad = mean(stress_quadratic, na.rm = TRUE),
    .groups = "drop"
  )

# Pivoting for plotting.
periphery_core_unweighted.2 <- periphery_core_unweighted.1 %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "stress_type",
    values_to = "avg_stress"
  ) %>%
  mutate(
    stress_type = recode(
      stress_type,
      "avg_abs" = "Absolute",
      "avg_raw" = "Level",
      "avg_quad" = "Quadratic"
    )
  )


unweighted_facet_plot <- ggplot(periphery_core_unweighted.2, aes(x = country_group, y = avg_stress, fill = country_group)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  facet_grid(stress_type ~ period, scales = "free_y") +
  labs(
    title = "",
    x = NULL,
    y = "Unweighted Average Stress"
  ) +
  scale_fill_manual(values = c("Core" = "#1f78b4", "Periphery" = "#e31a1c")) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(4, "lines")  # ← add this
  )

unweighted_facet_plot

ggsave("unweighted_stress_plot.png", plot = unweighted_facet_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")


###### LAST CHECKPOINT ##### Data Preperation for Fixed Effect Regression #####


# Merging gov debt to gdp = initial dataset = twelve_country.17
# Loading dataset from ecb
# https://data.ecb.europa.eu/search-results?searchTerm=government%20debt

# data cleaning

# Convert time to timeqtr

gov_debt <- gov_debt_gdp_emu

gov_debt <- gov_debt %>%
  mutate(
    time_clean = gsub("(Q[1-4]) (\\d{4})", "\\2 \\1", time),  
    time_qtr = as.yearqtr(time_clean, format = "%Y Q%q")       
  )

gov_debt <- gov_debt %>% 
  select(-time, -time_clean)

colnames(gov_debt) <- c("gov_debt_gdp", "geo", "time")

fixed_regression <- left_join(twelve_country.17, gov_debt, by = c("geo", "time"))


# Importing unemployment data
# readxl 


unemployment_eea <- unemployment_eea %>%
  mutate(
    time_clean = gsub("(Q[1-4]) (\\d{4})", "\\2 \\1", time),  
    time_qtr = as.yearqtr(time_clean, format = "%Y Q%q")       
  )

unemployment_eea <- unemployment_eea %>% 
  select(-time, -time_clean)


colnames(unemployment_eea) <- c("unemp", "geo", "time")

unemployment_eea <- unemployment_eea %>% 
  mutate(unemp = round(unemp, 2)) 

# fixed_regression.2 <- fixed_regression.1

fixed_regression.1 <- fixed_regression %>%
  mutate(geo = ifelse(country == "Greece", "GR", geo))


fixed_regression.2 <- left_join(fixed_regression.1, unemployment_eea, by = c("geo", "time"))

fixed_regression.3 <- left_join(fixed_regression.2, gov_debt, by = c("geo", "time"))

fixed_regression.3 <- fixed_regression.3 %>% 
  select(-gov_debt_gdp.x) %>% 
  mutate(gov_debt_gdp = gov_debt_gdp.y) %>% 
  select(-gov_debt_gdp.y)

fixed_regression_clean <- fixed_regression.3 %>% 
  select(-pi_gap, -rolling_hicp, -rolling_hicp_gap, -gdp_nominal, -fisher_rate, -rstar, -alpha, -geometry)

write.csv(fixed_regression_clean, "core_periphery.csv", row.names = FALSE)


# Inspired from Wachter et al (2024) fixed effect regression

library(stargazer)
library(plm)

# Including: Austria, Belgium, Finland, France, Germany, Greece, Ireland, Italy, Netherlands, Portugal, Spain

# Omitting late accession countries 

include_country <- c("Austria", "Belgium", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Netherlands", "Portugal", "Spain")

fixed_regression_restricted <- fixed_regression_clean %>% 
  filter(country %in% include_country)

# Austria: Core, Belgium: Core, Finland: Core, France: Core, Germany: Core, Greece: Periphery, Ireland: Periphery, Italy: Core, Netherlands: Core, Portugal: Periphery, Spain: Periphery  
# 6 Core, 5 periphery
# Greece adopts in 2002Q1 but it will not matter much. 

fixed_panel <- pdata.frame(fixed_regression_restricted, index = c("geo", "time"))

############# Controls will have 2 lags, stress 1,2,3 lag.

# GDP: no time FE
model_formula_gdp <- rgdp_g ~ 
  lag(stress_0abs, 1) * factor(country_group) +
  lag(rgdp_g, 2) + 
  lag(hicp, 2) + 
  lag(gov_debt_gdp, 2)

# HICP: no time FE
model_formula_hicp <- hicp ~ 
  lag(stress_0abs, 1) * factor(country_group) +
  lag(hicp, 2) + 
  lag(rgdp_g, 2) + 
  lag(gov_debt_gdp, 2)

model_formula_gdp_timefe <- update(model_formula_gdp, . ~ . + factor(time))
model_formula_hicp_timefe <- update(model_formula_hicp, . ~ . + factor(time))

# GDP without time FE
model_gdp <- plm(model_formula_gdp, data = fixed_panel,
                 index = c("geo", "time"), model = "within")

# GDP with time FE
model_gdp_timefe <- plm(model_formula_gdp_timefe, data = fixed_panel,
                        index = c("geo", "time"), model = "within")

# HICP without time FE
model_hicp <- plm(model_formula_hicp, data = fixed_panel,
                  index = c("geo", "time"), model = "within")

# HICP with time FE
model_hicp_timefe <- plm(model_formula_hicp_timefe, data = fixed_panel,
                         index = c("geo", "time"), model = "within")

se_gdp        <- vcovHC(model_gdp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_gdp_timefe <- vcovHC(model_gdp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp       <- vcovHC(model_hicp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp_timefe<- vcovHC(model_hicp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)


stargazer(model_gdp, model_gdp_timefe,
          model_hicp, model_hicp_timefe,
          se = list(
            sqrt(diag(se_gdp)),
            sqrt(diag(se_gdp_timefe)),
            sqrt(diag(se_hicp)),
            sqrt(diag(se_hicp_timefe))
          ),
          title = "FE Models for Real GDP Growth and Inflation (HICP)",
          dep.var.labels = c("Real GDP Growth", "", "Inflation (HICP)", ""),
          column.labels = c("GDP Country FE", "GDP Time + Country FE",
                            "HICP Country FE", "HICP Time + Country FE"),
          column.separate = c(1, 1, 1, 1),
          omit = "factor\\(time\\)",
          omit.stat = c("f", "ser"),
          digits = 3,
          type = "latex",
          out = "combined_fe_models.tex")

####### stress lag 2, controls lag 2 

model_formula_gdp <- rgdp_g ~ 
  lag(stress_0abs, 2) * factor(country_group) +
  lag(rgdp_g, 2) + 
  lag(hicp, 2) + 
  lag(gov_debt_gdp, 2)

# HICP: no time FE
model_formula_hicp <- hicp ~ 
  lag(stress_0abs, 2) * factor(country_group) +
  lag(hicp, 2) + 
  lag(rgdp_g, 2) + 
  lag(gov_debt_gdp, 2)

model_formula_gdp_timefe <- update(model_formula_gdp, . ~ . + factor(time))
model_formula_hicp_timefe <- update(model_formula_hicp, . ~ . + factor(time))

# GDP without time FE
model_gdp <- plm(model_formula_gdp, data = fixed_panel,
                 index = c("geo", "time"), model = "within")

# GDP with time FE
model_gdp_timefe <- plm(model_formula_gdp_timefe, data = fixed_panel,
                        index = c("geo", "time"), model = "within")

# HICP without time FE
model_hicp <- plm(model_formula_hicp, data = fixed_panel,
                  index = c("geo", "time"), model = "within")

# HICP with time FE
model_hicp_timefe <- plm(model_formula_hicp_timefe, data = fixed_panel,
                         index = c("geo", "time"), model = "within")

se_gdp        <- vcovHC(model_gdp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_gdp_timefe <- vcovHC(model_gdp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp       <- vcovHC(model_hicp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp_timefe<- vcovHC(model_hicp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)


stargazer(model_gdp, model_gdp_timefe,
          model_hicp, model_hicp_timefe,
          se = list(
            sqrt(diag(se_gdp)),
            sqrt(diag(se_gdp_timefe)),
            sqrt(diag(se_hicp)),
            sqrt(diag(se_hicp_timefe))
          ),
          title = "FE Models for Real GDP Growth and Inflation (HICP)",
          dep.var.labels = c("Real GDP Growth", "", "Inflation (HICP)", ""),
          column.labels = c("GDP Country FE", "GDP Time + Country FE",
                            "HICP Country FE", "HICP Time + Country FE"),
          column.separate = c(1, 1, 1, 1),
          omit = "factor\\(time\\)",
          omit.stat = c("f", "ser"),
          digits = 3,
          type = "latex",
          out = "combined_fe_models.tex")


# three lags 

model_formula_gdp <- rgdp_g ~ 
  lag(stress_0abs, 3) * factor(country_group) +
  lag(rgdp_g, 2) + 
  lag(hicp, 2) + 
  lag(gov_debt_gdp, 2)

# HICP: no time FE
model_formula_hicp <- hicp ~ 
  lag(stress_0abs, 3) * factor(country_group) +
  lag(hicp, 2) + 
  lag(rgdp_g, 2) + 
  lag(gov_debt_gdp, 2)

model_formula_gdp_timefe <- update(model_formula_gdp, . ~ . + factor(time))
model_formula_hicp_timefe <- update(model_formula_hicp, . ~ . + factor(time))

# GDP without time FE
model_gdp <- plm(model_formula_gdp, data = fixed_panel,
                 index = c("geo", "time"), model = "within")

# GDP with time FE
model_gdp_timefe <- plm(model_formula_gdp_timefe, data = fixed_panel,
                        index = c("geo", "time"), model = "within")

# HICP without time FE
model_hicp <- plm(model_formula_hicp, data = fixed_panel,
                  index = c("geo", "time"), model = "within")

# HICP with time FE
model_hicp_timefe <- plm(model_formula_hicp_timefe, data = fixed_panel,
                         index = c("geo", "time"), model = "within")

se_gdp        <- vcovHC(model_gdp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_gdp_timefe <- vcovHC(model_gdp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp       <- vcovHC(model_hicp, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)
se_hicp_timefe<- vcovHC(model_hicp_timefe, method = "arellano", type = "HC1", cluster = "group", maxlag = 4)


stargazer(model_gdp, model_gdp_timefe,
          model_hicp, model_hicp_timefe,
          se = list(
            sqrt(diag(se_gdp)),
            sqrt(diag(se_gdp_timefe)),
            sqrt(diag(se_hicp)),
            sqrt(diag(se_hicp_timefe))
          ),
          title = "FE Models for Real GDP Growth and Inflation (HICP)",
          dep.var.labels = c("Real GDP Growth", "", "Inflation (HICP)", ""),
          column.labels = c("GDP Country FE", "GDP Time + Country FE",
                            "HICP Country FE", "HICP Time + Country FE"),
          column.separate = c(1, 1, 1, 1),
          omit = "factor\\(time\\)",
          omit.stat = c("f", "ser"),
          digits = 3,
          type = "latex",
          out = "combined_fe_models.tex")

##### Creating summary statistics 
# Macro level = start with taylor.2 

unwanted_var <- c("m3_g", "output_gap_hlw", "u_f_y1", "eea_bond_spread", "time_index", "rolling_pce_gap", "year", "quarter", "time_date")

taylor_macro_summary <- taylor.2 %>% 
  select(-unwanted_var) %>% 
  filter(time >= "1999 Q1")

write.csv(taylor_macro_summary, "macro_summary.csv", row.names = FALSE)

# Code chunk: creating a summary table
# Necessary packages

install.packages("xtable")     
install.packages("knitr")       
install.packages("kableExtra")

# Summary stats. 
stargazer(taylor_macro_summary, type = "latex", summary.stat = c("mean", "sd", "min", "max", "median", "n"), out = "summarystats.tex")


# Member level = start with twelve_country.17 + add gov_debt_gdp_emu


gov_debt_gdp_emu.1 <- gov_debt_gdp_emu %>%
  mutate(
    time_clean = gsub("(Q[1-4]) (\\d{4})", "\\2 \\1", time),  
    time_qtr = as.yearqtr(time_clean, format = "%Y Q%q")       
  )


colnames(gov_debt_gdp_emu.1) <- c("abc", "gov_debt_gdp", "geo", "abcd", "time")

gov_debt_gdp_emu.1 <- gov_debt_gdp_emu.1 %>% 
  select(-abc, -abcd)

micro_gov_unemp <- left_join(unemployment_eea, gov_debt_gdp_emu.1, by = c("geo", "time"))


micro_summary_data <- left_join(twelve_country.17, micro_gov_unemp, by = c("geo", "time"))


unwanted_micro <- c("time_qtr", "gdp_nominal", "geometry", "active_member", "stress", "stress_0abs", "stress_quadratic", "stress_shaded", "total_gdp_active", "period")

micro_summary <- micro_summary_data %>% 
  select(-unwanted_micro)


micro_summary.1 <- micro_summary %>% 
  filter(time >= "1999 Q1")


micro_summary.2 <- micro_summary.1 %>%
  mutate(across(
    .cols = where(is.numeric),
    .fns = ~ round(.x, 2)
  ))


write.csv(micro_summary.2, "micro_summary.csv", col.names = FALSE)

micro_summary.3 <- micro_summary.2 %>%
  group_by(country) %>%
  summarise(across(where(is.numeric), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  ungroup()

# Necessary libraries
library(knitr)
library(kableExtra)

# Bad output
kable(micro_summary.3, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))


micro_summary.4 <- micro_summary.2 %>%
  group_by(country) %>%
  summarise(across(where(is.numeric), list(
    mean = ~round(mean(., na.rm = TRUE), 2),
    sd = ~round(sd(., na.rm = TRUE), 2),
    min = ~round(min(., na.rm = TRUE), 2),
    max = ~round(max(., na.rm = TRUE), 2)
  ), .names = "{.col}_{.fn}")) %>%
  ungroup()

# Latex output is bad
kable(micro_summary.4, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "scale_down", "hold_position"))

write.csv(micro_summary.4, "microdata_summary.csv", row.names = FALSE)

micro_summary_long <- micro_summary.4 %>%
  pivot_longer(
    cols = -country,
    names_to = c("variable", "stat"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = country,
    values_from = value
  )
