library(tidyverse)
library(here)
library(deeptime)

# read data
dat_guild <- read_csv(here("data",
                           "collections_per_stage.csv"))


# for all groups ----------------------------------------------------------

plot_guild <- dat_guild %>%
  # get mid stage age
  count(mid, guild) %>% 
  ggplot(aes(mid, n, 
             colour = guild)) +
  geom_point() +
  geom_line() +
  labs(y = "Collection count", 
       x = "Age [Ma]", 
       colour = "Guild") +
  coord_geo(dat = list("epochs", "periods"),
            pos = list("b", "b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list(5/.pt, 9/.pt),
            lab_color = "grey20", 
            color = "grey50", 
            abbrv = list(TRUE, FALSE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.1, 0.2)) +
  theme_minimal() +
  scale_x_reverse() +
  theme(legend.position = c(0.2, 0.8), 
        legend.background = element_rect(fill = "white", 
                                         colour = "white"))


# save plot
ggsave(plot_guild, filename = here("figures",
                                   "collections_per_stage.png"), 
       bg = "white", device = ragg::agg_png)



# split by group ----------------------------------------------------------

plot_guild_taxon <- dat_guild %>%
  # get mid stage age
  count(mid, taxon) %>% 
  left_join(dat_guild %>% 
              distinct(taxon, guild)) %>% 
  filter(taxon != "Radiodonta") %>% 
  ggplot(aes(mid, n, 
             colour = guild)) +
  geom_line() +
  labs(y = "Collection count", 
       x = "Age [Ma]", 
       colour = "Guild") +
  coord_geo(dat = list("periods"),
            pos = list("b"),
            alpha = 0.2, 
            height = unit(0.8, "line"), 
            size = list( 9/.pt),
            lab_color = "grey20", 
            color = "grey50", 
            abbrv = list(TRUE), 
            fill = "white",
            expand = TRUE, 
            lwd = list(0.1)) +
  theme_minimal() +
  scale_x_reverse() +
  theme(legend.position = c(0.9, 0.04), 
        legend.background = element_rect(fill = "white", 
                                         colour = "white")) +
  facet_wrap(~taxon, 
             scales = "free_y")

ggsave(plot_guild_taxon, filename = here("figures",
                                   "collections_per_stage_and_taxon.png"), 
       height = 100, 
       width = 183,
       units = "mm",
       bg = "white", device = ragg::agg_png)
