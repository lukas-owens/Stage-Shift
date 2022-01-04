library(tidyverse)
library(ggplot2)
library(ggrepel)
source('analysis\\core.R')

# Cancer sites to include
site_list <- c('Liver', 'Pancreas', 'Ovary', 'Lung', 'Stomach', 
               'Bladder', 'Lymphoma')

# Time period of mortality query
mort_interval <- 5

# Import raw data
file <- file.path('data', 'Incidence - Hypothetical.csv')
col_names <- c('site', 'ageband', 'stage', 'rate', 'se', 'lci', 'uci', 
               'count', 'population')
inc_data_raw <- read_csv(file, col_types = 'cccdddddd', 
                         skip = 1, col_names = col_names)

filepath <- file.path('data','Mortality - Hypothetical.csv')
col_names <- c('page', 'site', 'ageband', 'stage', 'interval', 'n', 'median', 
               'survival', 'se', 'lci', 'uci', 'count', 'population')
mort_data_raw <- read_csv(filepath, col_types = 'cccccdddddd', 
                          skip=1, col_names = col_names)

# Clean raw data
cleanup <- function(raw_data, percent_vars) {
  
  cleaned_data <- raw_data %>%
    
  # Relabel categorical variables
  mutate(site = ifelse(site=='Colon and Rectum','Colorectal', site),
         site = ifelse(site=='Lung and Bronchus','Lung', site),
         site = ifelse(site=='Melanoma of the Skin','Melanoma', site),
         site = ifelse(site=='Urinary Bladder','Bladder', site),
         ageband = sub(' years$', '', ageband),
         stage = factor(str_sub(stage, 1, 1), levels = c('I', 'L', 'R', 'D', 'U', 'B'))) %>%
  
  # Filter to relevant sites
  filter(site %in% site_list) %>%
  
  # Exclude unknown stage
  filter(stage != 'U', stage != 'B') %>%
    
  # Convert percentages
  mutate_at(percent_vars, function(x){x / 100})  
  
  return(cleaned_data)
}

inc_data <- cleanup(inc_data_raw, c('rate', 'se', 'lci', 'uci'))
mort_data <- cleanup(mort_data_raw, c('survival', 'se', 'lci', 'uci'))%>%
  select(-page, -interval, -median)


# Calculate incidence rates and hazards
inc_interval <- 5
inc_rates <- inc_data %>% 
  group_by(site, ageband) %>% 
  summarise(count = sum(count), population = sum(population)) %>%
  mutate(inc_rate = count / population,
         hazard_inc = -log(1-inc_rate) / inc_interval)

# Calculate stage split (Early = L and R, Late = D)
stage_split <- inc_data %>%
  mutate(stage = ifelse(stage == 'D', 'late', 'early')) %>%
  group_by(site, ageband, stage) %>% summarise(count = sum(count)) %>%
  pivot_wider(names_from = stage, values_from = count) %>%
  mutate(P = late / (early + late))


# Calculate mortality hazards by stage
mort_rates <- mort_data %>%
  mutate(stage = ifelse(stage == 'D', 'late', 'early')) %>%
  group_by(site, ageband, stage) %>%
  summarise(survival = weighted.mean(survival, n)) %>%
  pivot_wider(names_from = stage, values_from = survival, names_prefix = 'surv_') %>%
  mutate(hazard_early = -log(surv_early) / mort_interval,
         hazard_late = -log(surv_late) / mort_interval)

# Compute K-values
study_length <- 10
model_data <- full_join(inc_rates, mort_rates, by = c('site', 'ageband')) %>%
  left_join(stage_split %>% select(-early, -late), by = c('site', 'ageband')) %>%
  mutate(K = calc_K(study_length, hazard_inc, hazard_early, hazard_late, P))

# Plot output

sub <- model_data %>% filter(ageband == '50-54') %>% select(site, K) 
left <- sub %>% mutate(x=0, y=0)
right <- sub %>% mutate(x=1, y=K)
data <- bind_rows(left, right)

pdf(file='output\\figure1.pdf', width=6, height=5)
ggplot(data=bind_rows(left, right)) + 
  geom_line(aes(x=x, y=y, group=site)) +
  geom_label_repel(data=right, aes(x=1, y=y, label=site), size=3,
                   direction='y',hjust='left', nudge_x = 0.1) +
  geom_segment(aes(x=0, y=0, xend=1, yend=1), linetype='dashed') +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(limits = c(0,1.35), labels = scales::percent) + 
  xlab("Reduction in late-stage disease") + 
  ylab("Reduction in mortality") +
  theme_classic() + 
  theme(plot.title = element_text(size=12),
        text=element_text(size=12),
        legend.position = "none") 
dev.off()
  
outdata <- model_data %>% filter(ageband == '50-54') %>% select(site, count, hazard_inc, hazard_early, hazard_late, P, K) %>%
  arrange(desc(K))
write_csv(outdata, file.path('output', 'table1.csv'))




