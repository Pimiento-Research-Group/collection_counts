library(tidyverse)
library(here)


# define taxa to download from 2010 Klug et al.
tax_names <- c(
  # demersal
  "Ascocerida", "Discosorida", "Oncocerida", "Ellesmerocerida", 
  "Plectronocerida", "Protactinocerida", "Yanhecerida", "Actinocerida",
  "Endocerida", "Intejocerida", "Radiodonta", "Eurypterida", "Cephalochordata", 
  "Agnatha", 
  # plankton
  "Orthocerida", "Dacryoconarida", "Homoctenida", "Graptoloidea", 
  # nekton
  "Ammonoidea", "Nautilida", "Tarphycerida", "Acanthodii", "Chondrichthyes", 
  "Osteichthyes", "Placodermi"
  )


# set up function
get_pbdb_url <- function(taxon){
  params <- paste(
    # select group
    paste("base_name=",taxon, sep = ""),
    # minimum age
    "min_ma=289",
    # maximum age
    "max_ma=445",
    # use scotese atlas for plate reconstructions
    # "pgm=scotese",
    sep="&")
  
  # get url
  uri <- paste("https://paleobiodb.org/data1.2/colls/list.tsv?", params, sep="")
  
  uri
}

# get urls
url_list <- get_pbdb_url(tax_names) %>% 
  str_replace(" ", "")

# download data from the PBDB using the urls accessing the API
pbdb_data_raw <- map(url_list, ~read_tsv(.x, 
                                         show_col_types = FALSE),
                     .progress = TRUE)

# save download
pbdb_data_raw %>% 
  write_rds(here("data",
                 "pbdb_data_raw.rds"),
            compress = "gz")


