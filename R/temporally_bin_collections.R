library(tidyverse)
library(here)
library(divDyn)


# read data ---------------------------------------------------------------

# read all files at once and put them into one file
dat_pbdb <- read_rds(here("data",
                          "pbdb_data_raw.rds"))

# stage information
data("stages")

# table with keys to link stage information
data("keys")




# bin occurrences ---------------------------------------------------------

# bin to stages
dat_stages <- dat_pbdb %>% 
  map(~ .x %>% 
        # according to PBDB API, if late interval is empty,
        # it means that early interval is sufficient to assign age estimates
        mutate(late_interval = if_else(is.na(late_interval), 
                                       early_interval, 
                                       late_interval)) %>% 
        # get bin numbers based on look-up table
        mutate(early_bin = categorize(early_interval, keys$stgInt), 
               late_bin = categorize(late_interval, keys$stgInt), 
               across(c(early_bin, late_bin), as.numeric)) %>% 
        drop_na(early_bin, late_bin) %>% 
        filter(early_bin == late_bin) %>% 
        select(collection_no, n_occs, early_interval, late_interval, 
               max_ma, min_ma, early_bin, late_bin))

# assign same class
dat_stages <- dat_stages %>% 
  map(~ .x %>% 
        mutate(collection_no  = as.double(collection_no ), 
               early_interval = as.character(early_interval), 
               late_interval = as.character(late_interval)))

# assign guild ------------------------------------------------------------

# assing taxon name
for (i in 1:length(dat_stages)) {
  dat_stages[[i]]$taxon <-  names(dat_stages[i])
}


# merge and assign
dat_guild <- dat_stages %>% 
  bind_rows() %>% 
  mutate(guild = case_when(
    taxon %in% c("Ascocerida", "Discosorida", "Oncocerida", "Ellesmerocerida", 
                 "Plectronocerida", "Protactinocerida", "Yanhecerida", "Actinocerida",
                 "Endocerida", "Intejocerida", "Radiodonta", "Eurypterida", "Cephalochordata", 
                 "Agnatha") ~ "demersal", 
    taxon %in% c("Orthocerida", "Dacryoconarida", "Homoctenida", "Graptoloidea") ~ "plankton",
    taxon %in% c("Ammonoidea", "Nautilida", "Tarphycerida", "Acanthodii", "Chondrichthyes", 
                 "Osteichthyes", "Placodermi") ~ "nekton"
  )) %>% 
  rename(stg = early_bin) %>% 
  # get mid of stages
  left_join(stages %>% 
              select(stg, mid)) 


# save as csv
dat_guild %>% 
  write_csv(here("data", 
                 "collections_per_stage.csv"))

