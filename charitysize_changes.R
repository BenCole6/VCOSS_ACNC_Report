source("Visualisations.R")

charityname_VCOSScharitysize <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                   charity_name, Year, VCOSS_charitysize),
                                          count = n())

charityname_VCOSScharitysize <- arrange(charityname_VCOSScharitysize,
                                        charity_name, Year)

charityname_VCOSScharitysize <- mutate(group_by(charityname_VCOSScharitysize,
                                                charity_name),
                                       previous_size = lag(VCOSS_charitysize, n = 1),
                                       charitysize_change = if_else(VCOSS_charitysize != previous_size,
                                                                    true = TRUE, false = FALSE))


charityname_VCOSScharitysize_changes <- filter(charityname_VCOSScharitysize,
                                               charitysize_change == TRUE)

charityname_VCOSScharitysize_changes <- mutate(charityname_VCOSScharitysize_changes,
                                               numeric_size = case_when(VCOSS_charitysize == "Extra Small" ~ 1,
                                                                        VCOSS_charitysize == "Small" ~ 2,
                                                                        VCOSS_charitysize == "Medium" ~ 3,
                                                                        VCOSS_charitysize == "Large" ~ 4,
                                                                        VCOSS_charitysize == "Extra Large" ~ 5,
                                                                        VCOSS_charitysize == "Extra Extra Large" ~ 6),
                                               previous_numeric_size = case_when(previous_size == "Extra Small" ~ 1,
                                                                                 previous_size == "Small" ~ 2,
                                                                                 previous_size == "Medium" ~ 3,
                                                                                 previous_size == "Large" ~ 4,
                                                                                 previous_size == "Extra Large" ~ 5,
                                                                                 previous_size == "Extra Extra Large" ~ 6),
                                               numeric_size_change = numeric_size - previous_numeric_size,
                                               change_direction = case_when(numeric_size < previous_numeric_size ~ "lesser",
                                                                            numeric_size == previous_numeric_size ~ "none",
                                                                            numeric_size > previous_numeric_size ~ "greater"))

total_changing_charitysizes <- summarise(group_by(charityname_VCOSScharitysize_changes,
                                                  Year),
                                         "Decreased Charity Size" = sum(change_direction == "lesser"),
                                         "Increased Charity Size" = sum(change_direction == "greater"))

total_charities <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                      Year),
                             "total charities" = n())

total_changing_charitysizes <- left_join(total_changing_charitysizes,
                                         total_charities,
                                         by = "Year")

changing_charitysizes_bysize <- summarise(group_by(charityname_VCOSScharitysize_changes,
                                                   previous_size, Year),
                                          "Decreased Charity Size" = sum(change_direction == "lesser"),
                                          "Increased Charity Size" = sum(change_direction == "greater"))

changing_charitysizes_bysize <- pivot_longer(changing_charitysizes_bysize,
                                             cols = c("Decreased Charity Size", "Increased Charity Size"),
                                             names_to = "Change type",
                                             values_to = "Number of Charities")

gg_changing_charitysizes_bysize <- ggplot(changing_charitysizes_bysize,
                                          aes(x = Year, y = `Number of Charities`,
                                              colour = previous_size, group = previous_size)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = `Number of Charities`),
            inherit.aes = TRUE,
            vjust = -0.75,
            size = 3.25,
            position = position_dodge(width = 0.075),
            show.legend = FALSE) +
  facet_wrap(~`Change type`,
             nrow = 1,
             scales = "fixed") +
  scale_colour_manual(values = c(VCOSS_colours, "cyan3"),
                      "Size in previous year",
                      guide = guide_legend(title.position = "top",
                                           title.hjust = 0.5,
                                           ncol = 3, byrow = TRUE)) +
  scale_y_continuous(expand = expansion(mult = c(0.025, 0.05)),
                     breaks = breaks_extended(n = 4)) +
  scale_x_discrete(expand = expansion(mult = c(0.125, 0.125))) +
  labs(title = "Changing charity sizes in Victorian community service sector",
       subtitle = "Total number of charities that changed their VCOSS-defined charity size between years",
       caption = paste(sep = "\n",
                       "VCOSS defines charity sizes based on Total Gross Income:",
                       "Extra Small: < $50,000",
                       "Small: >= $50,000 & < $250,000",
                       "Medium: >= $250,000 & < $1m",
                       "Large: >= $1m & < $10m",
                       "Extra Large: >= $10m & < $100m",
                       "Extra Extra Large: >= $100m")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey80"),
        plot.caption.position = "plot",
        legend.position = c(0.1, -0.1))

ggsave(gg_changing_charitysizes_bysize,
       filename = "R_Visualisations/changing_charitysizes_by_year.png",
       height = 9, width = 14,
       units = "in", dpi = 750)


change_direction_year <- summarise(group_by(charityname_VCOSScharitysize_changes,
                                            previous_size, Year, numeric_size_change),
                                   count = n())


charitysize_levels <- levels(change_direction_year$previous_size)

change_direction_year <- mutate(change_direction_year,
                                previous_size_index = case_when(previous_size == "Extra Small" ~ 1,
                                                                previous_size == "Small" ~ 2,
                                                                previous_size == "Medium" ~ 3,
                                                                previous_size == "Large" ~ 4,
                                                                previous_size == "Extra Large" ~ 5,
                                                                previous_size == "Extra Extra Large" ~ 6),
                                new_size_index = previous_size_index + numeric_size_change)

change_direction_year$new_size <- charitysize_levels[change_direction_year$new_size_index]

change_direction_year <- mutate(change_direction_year,
                                change_type = paste(previous_size, "->", new_size),
                                size_text = case_when(abs(numeric_size_change) == 1 ~ "size",
                                                      abs(numeric_size_change) != 1 ~ "sizes"),
                                change_direction = case_when(numeric_size_change < 0 ~ "Decreased",
                                                             numeric_size_change > 0 ~ "Increased"),
                                change_desc = paste(change_direction, abs(numeric_size_change), size_text))


change_direction_year <- mutate(group_by(change_direction_year,
                                         Year, previous_size),
                                proportion = (count / sum(count, na.rm = TRUE)),
                                total = sum(count, na.rm = TRUE))

levels(change_direction_year$new_size) <- charitysize_levels

levels(change_direction_year$change_desc) <- c("Decreased 1 size", "Decreased 2 sizes", "Decreased 3 sizes",
                                               "Increased 1 size", "Increased 2 sizes", "Increased 3 sizes")


orange_grad <- c("#eabb9f", "#ea8c54", VCOSS_colours[1])

blue_grad <- c("#8689a1", "#696fa1", "#4c56a1")



gg_change_direction_year <- ggplot(filter(change_direction_year,
              as.integer(as.character(Year)) >= 2016),
       aes(x = Year, y = proportion,
           fill = fct_rev(change_desc), group = fct_rev(change_desc))) +
  geom_bar(position = position_fill(reverse = FALSE),
           stat = "identity") +
  geom_text(aes(label = paste(sep = "; ",
                              percent(proportion, 0.1),
                              count)),
            position = position_stack(vjust = 0.5),
            size = 3, colour = "black",
            check_overlap = TRUE) +
  geom_text(aes(label = paste("total charities\nchanging size =", total),
                y = 1),
            vjust = -1/3, size = 3,
            check_overlap = TRUE, colour = "black") +
  facet_rep_wrap(~previous_size,
                 scales = "free_y",
                 repeat.tick.labels = "x") +
  scale_fill_manual("Type of charity size change",
                    values = c(rev(orange_grad), rev(blue_grad)),
                    guide = guide_legend(title.position = "top",
                                         ncol = 2, reverse = TRUE,
                                         byrow = FALSE)) +
  scale_y_continuous(labels = percent,
                     "Percent of Charities",
                     expand = expansion(mult = c(0.05, 0.1)),
                     breaks = seq(0, 1, 0.25)) +
  labs(title = "Types of changes in charity size of Victorian community service sector charities",
       subtitle = "Number of charities that changed their VCOSS-defined charity size between years",
       caption = paste(sep = "\n",
                       "VCOSS defines charity sizes based on Total Gross Income:",
                       "Extra Small: < $50,000",
                       "Small: >= $50,000 & < $250,000",
                       "Medium: >= $250,000 & < $1m",
                       "Large: >= $1m & < $10m",
                       "Extra Large: >= $10m & < $100m",
                       "Extra Extra Large: >= $100m")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey80"),
        panel.border = element_rect(colour = VCOSS_colours[3],
                                    fill = NA),
        strip.background = element_rect(fill = VCOSS_colours[3],
                                        colour = VCOSS_colours[3]),
        legend.position = c(0.1, -0.085))

ggsave(gg_change_direction_year,
       filename = "R_Visualisations/changing_charitysizes_by_type_year.png",
       height = 12, width = 15,
       units = "in", dpi = 750)


gg_alluv_change_direction_year <- ggplot(change_direction_year,
                                         aes(y = count,
                                             axis1 = previous_size,
                                             axis2 = new_size,
                                             fill = change_desc)) +
  geom_alluvium(colour = "grey50") +
  geom_stratum(fill = "aliceblue",
               colour = "grey80") +
  geom_fit_text(stat = "stratum",
                aes(label = paste(sep = "\n",
                                  comma(count, 1),
                                  after_stat(stratum),
                                  "charities")),
                width = 1/7, min.size = 6,
                show.legend = FALSE) +
  scale_fill_manual("Size change",
                    values = c(blue_grad, orange_grad)) +
  scale_x_discrete(limits = c("Size in\nprevious year", "Size in\ncurrent year"),
                   expand = c(0.025, 0.025)) +
  scale_y_continuous("", labels = NULL,
                     expand = expansion(c(0.0125, 0.0125))) +
  labs(title = "Changes in charity sizes in Victorian community service sector",
       subtitle = "Based on VCOSS defined charity sizes",
       caption = paste(sep = "\n",
                       "VCOSS defines charity sizes based on Total Gross Income:",
                       "Extra Small: < $50,000",
                       "Small: >= $50,000 & < $250,000",
                       "Medium: >= $250,000 & < $1m",
                       "Large: >= $1m & < $10m",
                       "Extra Large: >= $10m & < $100m",
                       "Extra Extra Large: >= $100m")) +
  facet_rep_wrap(~Year,
                 scales = "free_y",
                 repeat.tick.labels = "x") +
  theme_minimal() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(colour = "grey80",
                                        fill = "aliceblue"),
        plot.caption.position = "plot")
  
ggsave(gg_alluv_change_direction_year,
       filename = "R_Visualisations/sankey_changing_charitysizes_by_type_year.png",
       height = 12, width = 12,
       units = "in", dpi = 750)

gg_alluv_change_direction_2018 <- ggplot(filter(change_direction_year,
                                                Year == 2018),
                                         aes(y = count,
                                             axis1 = previous_size,
                                             axis2 = new_size,
                                             fill = change_desc)) +
  geom_alluvium(colour = "grey50") +
  geom_stratum(fill = "aliceblue",
               colour = "grey80") +
  geom_fit_text(stat = "stratum",
                aes(label = paste(comma(count, 1),
                                  after_stat(stratum),
                                  "charities")),
                width = 1/4, min.size = 6, reflow = TRUE,
                show.legend = FALSE) +
  scale_fill_manual("Size change",
                    values = c(blue_grad, orange_grad)) +
  scale_x_discrete(limits = c("Size in 2017", "Size in 2018"),
                   expand = c(0.025, 0.025),
                   position = "top") +
  scale_y_continuous("", labels = NULL,
                     expand = expansion(c(0.0125, 0.0125))) +
  labs(title = "Changes in charity sizes in Victorian community service sector in 2018",
       subtitle = "Based on VCOSS defined charity sizes",
       caption = paste(sep = "\n",
                       "VCOSS defines charity sizes based on Total Gross Income:",
                       "Extra Small: < $50,000",
                       "Small: >= $50,000 & < $250,000",
                       "Medium: >= $250,000 & < $1m",
                       "Large: >= $1m & < $10m",
                       "Extra Large: >= $10m & < $100m",
                       "Extra Extra Large: >= $100m")) +
  theme_minimal() +
  theme(plot.margin = unit(c(5, 5, 5, 5), "mm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption.position = "plot")

ggsave(gg_alluv_change_direction_2018,
       filename = "R_Visualisations/sankey_changing_charitysizes_by_type_2018.png",
       height = 12, width = 12,
       units = "in", dpi = 750)

beepr::beep(5)
