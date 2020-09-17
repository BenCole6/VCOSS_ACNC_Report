if(!("VCOSS_ACNC_Datasets_Combined" %in% ls())) {
  source("Data_Cleaning.R")
}

library(pacman)

p_load(ggplot2,
       tidyverse,
       scales,
       lemon,
       gganimate,
       gridExtra)

VCOSS_colours <- c("#ea5d0a", "#4b55a1", "#f6a400", "black", "grey50")

## Coercing Year to factor
VCOSS_ACNC_Datasets_Combined$Year <- as.factor(VCOSS_ACNC_Datasets_Combined$Year)

## Data Overview

## Types of Organisations

Orgs_by_MainAct_Year <- arrange(summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                   main_activity, Year),
                                          "No. of Orgs" = n()),
                                Year, main_activity)



Orgs_by_MainAct_Year <- rename(arrange(pivot_wider(Orgs_by_MainAct_Year,
                                                   names_from = Year, names_prefix = "No. of Orgs. ",
                                                   values_from = `No. of Orgs`),
                                       main_activity),
                               "Main Activity as specificied in the AIS" = main_activity)

Orgs_by_MainAct_Year <- mutate(Orgs_by_MainAct_Year,
                               "Change from 2017 to 2018" = (`No. of Orgs. 2018` - `No. of Orgs. 2017`))

rbind(Orgs_by_MainAct_Year,
      colSums(Orgs_by_MainAct_Year[2:ncol(Orgs_by_MainAct_Year)]))



## Income

totgrosinc_mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                              main_activity, Year),
                                     total_gross_income = sum(total_gross_income, na.rm = TRUE))

gg_totgrosinc_mainact_year <- ggplot(totgrosinc_mainact_year,
                                     aes(x = str_wrap(main_activity, 18), y = total_gross_income,
                                         fill = Year)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_discrete("Main Activity") +
  scale_y_continuous("Total Gross Income",
                     labels = dollar_format(scale = 1/1000000000,
                                            accuracy = 0.1,
                                            suffix = "bn"),
                     expand = c(0.025, 0.05)) +
  scale_fill_manual(values = rev(VCOSS_colours)) +
  ggtitle("Victorian community services industry total gross income 2014 - 2018",
          subtitle = "Main activity as reported in the AIS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55,
                                   hjust = 1),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(colour = "grey50"))

ggsave(filename = "R_Visualisations/totalgrossincome_by_mainactivity_year.png",
       plot = gg_totgrosinc_mainact_year,
       height = 8, width = 12,
       units = "in", dpi = 750)

# cleaning up main beneficiaries

VCOSS_ACNC_Datasets_Combined$main_beneficiaries <- recode(VCOSS_ACNC_Datasets_Combined$main_beneficiaries,
                                                          "People at risk of homelessness/ people experiencing homelessness" = "People at risk of homelessness / people experiencing homelessness",
                                                          "People at risk of homelessness/people experiencing homelessness" = "People at risk of homelessness / people experiencing homelessness",
                                                          "Youth - 15 to under 25" = "Youth - aged 15 to under 25")

beneficiaries_mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                 Year, main_beneficiaries),
                                        "Number of Charities" = n())

beneficiaries_mainact_year <- filter(beneficiaries_mainact_year,
                                     !is.na(main_beneficiaries))

gg_beneficiaries_mainact_year <- ggplot(beneficiaries_mainact_year,
                                        aes(x = `Number of Charities`, fill = fct_rev(Year),
                                            y = fct_rev(str_wrap(main_beneficiaries, 30)))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_continuous("Number of Charities",
                     labels = comma,
                     expand = c(0.005, 0.05)) +
  scale_y_discrete("Main Activity") +
  scale_fill_manual(values = VCOSS_colours,
                    "Year",
                    guide = guide_legend(reverse = TRUE)) +
  ggtitle("Beneficiaries of Victorian community services industry 2017 - 2018",
          subtitle = "Number of charities servicing beneficiaries as reported in the AIS") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_line(colour = "grey50"))

ggsave(filename = "R_Visualisations/charitycount_by_beneficiaries.png",
       plot = gg_beneficiaries_mainact_year,
       height = 12, width = 8,
       units = "in", dpi = 750)

budgetstatus_mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                main_activity, Year),
                                       deficit = sum(net_surplus_deficit < 0),
                                       balanced = sum(net_surplus_deficit == 0),
                                       surplus = sum(net_surplus_deficit > 0))

budgetstatus_mainact_year <- pivot_longer(budgetstatus_mainact_year,
                                          cols = c(deficit, balanced, surplus),
                                          names_to = "Budget status",
                                          values_to = "Number of Charities")

gg_budgetstatus_mainact_year <- ggplot(budgetstatus_mainact_year,
                                       aes(x = Year, y = `Number of Charities`,
                                           fill = `Budget status`, group = `Budget status`)) +
  geom_area(stat = "identity")  +
  facet_rep_wrap(~str_wrap(main_activity, 20),
                 scales = "free_y",
                 repeat.tick.labels = "x",
                 ncol = 3) +
  scale_fill_manual(values = VCOSS_colours) +
  ggtitle("Victorian community service industry budget status",
          subtitle = "Main activity as reported in AIS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

ggsave(filename = "R_Visualisations/budgetstatus_by_year_mainact.png",
       plot = gg_budgetstatus_mainact_year,
       height = 12, width = 8,
       units = "in", dpi = 750)

mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                   main_activity, Year),
                          count = n())

gg_mainact_year <- ggplot(group_by(mainact_year, main_activity),
                          aes(x = Year, y = count,
                              group = main_activity,
                              colour = main_activity)) +
  geom_line(colour = VCOSS_colours[1],
            size = 7/8) +
  geom_point(colour = VCOSS_colours[1],
             size = 14/8) +
  facet_rep_wrap(~str_wrap(main_activity, 28),
                 scales = "free_y",
                 repeat.tick.labels = "x",
                 ncol = 3) +
  scale_y_continuous("Number of Charities",
                     breaks = breaks_extended(3)) +
  ggtitle("Change in Victorian community service charities over time",
          subtitle = "Main activity as reported in the AIS") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

ggsave(filename = "R_Visualisations/charitycount_by_mainact_year.png",
       plot = gg_mainact_year,
       height = 12, width = 8,
       units = "in", dpi = 750)  

gg_employeeexpenses <- ggplot(VCOSS_ACNC_Datasets_Combined,
                              aes(x = employeeexpenses_per_employee,
                                  fill = Year, colour = Year, frame = Year)) +
  geom_density() +
  facet_rep_wrap(~main_activity,
                 scales = "free",
                 repeat.tick.labels = "x",
                 ncol = 3) +
  scale_x_continuous("Employee Expenses per Employee",
                     labels = dollar,
                     trans = "log10") +
  scale_y_continuous("Proportion of charities", labels = NULL) +
  scale_fill_manual(values = rev(VCOSS_colours)) +
  scale_colour_manual(values = rev(VCOSS_colours)) +
  guides(colour = "none", fill = "none") +
  ggtitle("Employee expenses of Victorian community service charities",
          "Average expenses per employee as reported in the AIS\nYear: {closest_state}") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "aliceblue", size = 2,
                                    fill = NA),
        text = element_text(size = 18)) +
  transition_states(Year,
                    state_length = 10)

anim_save(filename = "R_Visualisations/vic_commservice_employeeexpenses.gif",
          animation = gg_employeeexpenses,
          end_pause = 25, fps = 4,
          height = 1750, width = 1250)

gg_totalgrossincome <- ggplot(VCOSS_ACNC_Datasets_Combined,
                              aes(x = total_gross_income,
                                  fill = Year, colour = Year, frame = Year)) +
  geom_density() +
  facet_rep_wrap(~main_activity,
                 scales = "free",
                 repeat.tick.labels = "x",
                 ncol = 3) +
  scale_x_continuous("Total Gross Income",
                     labels = dollar,
                     trans = "log10") +
  scale_y_continuous("Proportion of charities", labels = NULL) +
  scale_fill_manual(values = rev(VCOSS_colours)) +
  scale_colour_manual(values = rev(VCOSS_colours)) +
  guides(colour = "none", fill = "none") +
  ggtitle("Distribution of total gross income of Victorian community service charities",
          "Total gross income as reported in the AIS\nYear: {closest_state}") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "aliceblue", size = 2,
                                    fill = NA),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 20, hjust = 1),
        axis.ticks.x = element_line(colour = "grey"))  +
  transition_states(Year,
                    state_length = 10)

anim_save(filename = "R_Visualisations/vic_commservice_totalgrossincome.gif",
          animation = gg_totalgrossincome,
          end_pause = 25, fps = 4,
          height = 1750, width = 1250)

mainact_year_donationsmade <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                 main_activity, Year),
                                        total_donations_made = sum(grants_and_donations_made_for_use_in_australia,
                                                                   grants_and_donations_made_for_use_outside_australia,
                                                                   na.rm = TRUE))

gg_mainact_year_donationsmade <- ggplot(mainact_year_donationsmade,
                                        aes(x = str_wrap(main_activity, 20),
                                            y = total_donations_made,
                                            fill = Year)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_y_continuous("Total donations made",
                     labels = dollar_format(scale = 0.000001,
                                            suffix = "m"),
                     expand = c(0.0125, 0.025)) +
  scale_x_discrete("Main activity") +
  ggtitle("All donations made by Victorian community service charities",
          subtitle = "Main activity as reported in the AIS") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        axis.ticks.x = element_line(colour = "grey"),
        panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = rev(VCOSS_colours))

ggsave(filename = "R_Visualisations/alldonationsmade_by_mainact_year.png",
       plot = gg_mainact_year_donationsmade,
       dpi = 750,
       height = 8, width = 12,
       units = "in")

VCOSS_ACNC_Datasets_Combined$VCOSS_charitysize <- factor(VCOSS_ACNC_Datasets_Combined$VCOSS_charitysize,
                                                         levels = c("Extra Small", "Small", "Medium", "Large", "Extra Large", "Extra Extra Large"))


ggplot(VCOSS_ACNC_Datasets_Combined,
       aes(x = Year, fill = fct_rev(VCOSS_charitysize))) +
  geom_bar(position = "stack") +
  geom_text(stat = "count",
            aes(label = ..count..,
                y = ..count..),
            colour = "white", position = position_stack(vjust = 0.5),
            check_overlap = TRUE) +
  scale_fill_manual(values = c("cyan", VCOSS_colours),
                    "Charity Size") +
  scale_y_continuous("Number of charities", labels = comma,
                     expand = c(0.0125, 0.025)) +
  ggtitle("Changes in charity sizes throughout the years",
          subtitle = "VCOSS-defined charity size using total gross income as reported in the AIS") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

vcoss_charitysize_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                             VCOSS_charitysize, Year),
                                    count = n())

gg_vcoss_charitysize_year <- ggplot(vcoss_charitysize_year,
                                    aes(x = Year, y = count,
                                        colour = VCOSS_charitysize,
                                        group = VCOSS_charitysize)) +
  geom_line(position = "identity",
            size = 7/8) +
  geom_point(size = 14/8) +
  scale_colour_manual(values = c("cyan", VCOSS_colours),
                      "Charity Size") +
  scale_y_continuous("Number of charities", labels = comma,
                     expand = c(0.0125, 0.025)) +
  ggtitle("Changes in charity sizes throughout the years",
          subtitle = "VCOSS-defined charity size using total gross income as reported in the AIS") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

ggsave(filename = "R_Visualisations/charitycount_by_VCOSScharitysize_year.png",
       plot = gg_vcoss_charitysize_year,
       height = 8, width = 12,
       units = "in", dpi = 750)

VCOSS_ACNC_Datasets_Combined$cleaned_charitysize <- factor(VCOSS_ACNC_Datasets_Combined$cleaned_charitysize,
                                                           levels = c("S", "M", "L"),
                                                           labels = c("Small", "Medium", "Large"))

incomesource_charsize_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                 cleaned_charitysize, Year),
                                        government_grants = sum(government_grants, na.rm = TRUE),
                                        donations_and_bequests = sum(donations_and_bequests, na.rm = TRUE),
                                        other_income_and_revenue = sum(other_income, revenue_from_goods_and_services,
                                                                       revenue_from_investments, all_other_revenue,
                                                                       na.rm = TRUE))

incomesource_charsize_year <- pivot_longer(incomesource_charsize_year,
                                           cols = c(government_grants, donations_and_bequests,
                                                    other_income_and_revenue),
                                           names_to = "income_source",
                                           values_to = "income_amount")

incomesource_charsize_year$income_source <- factor(incomesource_charsize_year$income_source,
                                                   levels = c("government_grants", "donations_and_bequests",
                                                              "other_income_and_revenue"),
                                                   labels = c("Government\ngrants", "Donations and\nbequests",
                                                              "Other income\nand revenue"))

incomesource_charsize_year <- mutate(group_by(incomesource_charsize_year,
                                              cleaned_charitysize, Year),
                                     income_proportion = (income_amount / sum(income_amount)))

gg_incomesource_charsize_year <- ggplot(group_by(incomesource_charsize_year,
                                                 Year, cleaned_charitysize),
                                        aes(x = Year, y = income_proportion,
                                            fill = fct_rev(income_source))) +
  geom_bar(stat = "identity",
           position = "fill") +
  geom_text(aes(label = percent(income_proportion,
                                0.1),
                y = income_proportion),
            position = position_stack(vjust = 0.5),
            check_overlap = TRUE,
            colour = "white") +
  facet_wrap(~cleaned_charitysize,
             nrow = 1) +
  scale_y_continuous("Proportion of Income",
                     labels = percent,
                     expand = c(0.0025, 0.0025)) +
  scale_fill_manual(values = VCOSS_colours[c(3, 1, 2)],
                    "Income source") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        legend.key.size = unit(8, "mm"))

gg_line_incomesource_charsize_year <- ggplot(incomesource_charsize_year,
                                             aes(x = Year, y = income_amount,
                                                 group = income_source, colour = fct_rev(income_source))) +
  geom_line() +
  geom_point() +
  facet_wrap(~cleaned_charitysize,
             nrow = 1,
             scales = "free_y") +
  scale_color_manual("Income Source",
                     values = VCOSS_colours[c(3, 1, 2)]) +
  scale_y_continuous("Total Income Amount",
                     labels = dollar_format(scale = 0.000001,
                                            suffix = "m"),
                     limits = c(0, NA),
                     breaks = breaks_extended(n = 6)) +
  scale_x_discrete("") +
  ggtitle("Breakdown of income amount by community service charity size and income source") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank())

gg_income_combined <- grid.arrange(gg_line_incomesource_charsize_year,
                                   gg_incomesource_charsize_year,
                                   nrow = 2,
                                   heights = c(2, 3))

ggsave(filename = "R_Visualisations/breakdown_incomesource_by_charitysize_year.png",
       plot = gg_income_combined,
       width = 16, height = 10,
       units = "in", dpi = 750)


incomesource_mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                main_activity, Year),
                                       government_grants = sum(government_grants, na.rm = TRUE),
                                       donations_and_bequests = sum(donations_and_bequests, na.rm = TRUE),
                                       other_income_and_revenue = sum(other_income, revenue_from_goods_and_services,
                                                                      revenue_from_investments, all_other_revenue,
                                                                      na.rm = TRUE))

incomesource_mainact_year <- pivot_longer(incomesource_mainact_year,
                                          cols = c(government_grants, donations_and_bequests, other_income_and_revenue),
                                          names_to = "income_source",
                                          values_to = "income_amount")

incomesource_mainact_year$income_source <- factor(incomesource_mainact_year$income_source,
                                                   levels = c("government_grants", "donations_and_bequests",
                                                              "other_income_and_revenue"),
                                                   labels = c("Government\ngrants", "Donations and\nbequests",
                                                              "Other income\nand revenue"))

incomesource_mainact_year <- mutate(group_by(incomesource_mainact_year,
                                             main_activity, Year),
                                    income_proportion = (income_amount / sum(income_amount)))

gg_incomesource_mainact_year <- ggplot(incomesource_mainact_year,
                                       aes(x = Year, y = income_proportion,
                                           fill = fct_rev(income_source), group = fct_rev(income_source))) +
  geom_area() +
  facet_rep_wrap(~str_wrap(main_activity),
                 ncol = 3,
                 repeat.tick.labels = "x") +
  scale_y_continuous("Proportion of Income",
                     labels = percent,
                     limits = c(0, 1)) +
  scale_x_discrete(expand = c(0.025, 0.025)) +
  scale_fill_manual("Income source",
                    values = VCOSS_colours[c(3, 1, 2)]) +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.1))

ggsave(filename = "R_Visualisations/incomeshare_by_mainactivity_year.png",
       plot = gg_incomesource_mainact_year,
       height = 12, width = 10,
       units = "in", dpi = 750)
