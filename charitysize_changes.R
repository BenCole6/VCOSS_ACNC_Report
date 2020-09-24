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

ggplot(changing_charitysizes_bysize,
       aes(x = Year, y = `Number of Charities`,
           colour = previous_size, group = previous_size)) +
  geom_point() +
  geom_line() +
  geom_text(aes(label = `Number of Charities`),
            inherit.aes = TRUE,
            vjust = -0.75,
            position = position_dodge(width = 0.075),
            show.legend = FALSE) +
  facet_wrap(~`Change type`,
             nrow = 1,
             scales = "fixed") +
  scale_colour_manual(values = c(VCOSS_colours, "cyan3"),
                      "Size in previous year") +
  scale_y_continuous(expand = c(0.025, 0.6)) +
  scale_x_discrete(expand = c(0.125, 0.125)) +
  ggtitle("Changing charity sizes in Victorian community service sector",
          "VCOSS-defined charity size",
          caption = paste(sep = "\n",
                          )) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey80"))
