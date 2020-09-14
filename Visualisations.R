if(!("VCOSS_ACNC_Datasets_Combined" %in% ls())) {
  source("Data_Cleaning.R")
}

library(pacman)

p_load(ggplot2,
       tidyverse,
       scales,
       lemon)

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

ggplot(totgrosinc_mainact_year,
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

ggplot(beneficiaries_mainact_year,
       aes(x = `Number of Charities`, fill = Year,
           y = fct_rev(str_wrap(main_beneficiaries, 30)))) +
  geom_bar(stat = "identity",
           position = "dodge") +
  scale_x_continuous("Number of Charities",
                     labels = comma,
                     expand = c(0.005, 0.05)) +
  scale_y_discrete("Main Activity") +
  scale_fill_manual(values = VCOSS_colours) +
  ggtitle("Victorian community services industry total gross income 2014 - 2018",
          subtitle = "Main activity as reported in the AIS") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.ticks.y = element_line(colour = "grey50"))

budgetstatus_mainact_year <- summarise(group_by(VCOSS_ACNC_Datasets_Combined,
                                                main_activity, Year),
                                       deficit = sum(net_surplus_deficit < 0),
                                       balanced = sum(net_surplus_deficit == 0),
                                       surplus = sum(net_surplus_deficit > 0))

budgetstatus_mainact_year <- pivot_longer(budgetstatus_mainact_year,
                                          cols = c(deficit, balanced, surplus),
                                          names_to = "Budget status",
                                          values_to = "Number of Charities")

ggplot(budgetstatus_mainact_year,
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

