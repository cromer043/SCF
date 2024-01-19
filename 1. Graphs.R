#Date: 2024/01/03
#Author: Carl Romer
#This file takes the SCF data from 1. Data Cleaning and makes graphs for the blog

#########################################################
#Setup
#########################################################
package_list <- c("tidyverse",
                  "purrr",
                  "scales",
                  "openxlsx", 
                  "scales",
                  "devtools", 
                  "extrafont",
                  "tidycensus")

#install.packages(package_list) #if you need to install remove first # on this line
#webshot::install_phantomjs()
lapply(package_list,
       require,
       character.only = T)
#########################################################
#Build functions
#########################################################
S_sqrt <- function(x){sign(x)*sqrt(abs(x))}
IS_sqrt <- function(x){x^2*sign(x)}
S_sqrt_trans <- function() trans_new("S_sqrt",S_sqrt,IS_sqrt)

#########################################################
#Data importation
#########################################################
setwd("C:/Users/csromer/OneDrive - National Bankers Association/Blogs/2024/Survey of Consumer Finances")
net_worth <- read_csv(
  "2. Data Output/net worth.csv"
  )

homeowner_net_worth <- read_csv(
  "2. Data Output/homeowner_net_worth.csv"
)

home_equity <- read_csv(
  "2. Data Output/home_equity.csv"
)


household_population <- read_csv(
  "2. Data Output/household pop.csv"
)

homeownership <- read_csv(
  "2. Data Output/homeownership.csv"
)

#########################################################
#graphs
#########################################################

#########################################################
#Population
#########################################################
#Population
#Overall

overall_pop <- household_population %>% 
  filter(race == "Overall" &
           income == "Overall")

write_csv(overall_pop,
          "2. Data Output/overall_pop.csv")

overall_popggplot <- overall_pop %>% 
  ggplot(
    aes(
      x = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                 )
      ),
      y = population - 128000000,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin = low - 128000000,
      ymax = high - 128000000
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
  )+
  scale_y_continuous(labels = c("128,000,000",
                                "129,000,0000", 
                                "129,000,000",
                                "130,000,000",
                                "131,000,000"
                                ),
                     #trans = scales::sqrt_trans(),
                     limits = c(0,4000000),
                     expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Household population\n(note y axis does not start at zero)",
    title = "Figure 1 Overall: Household Population, 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(overall_popggplot, 
       filename = "3. Graphs/Figure 1A.pdf",
       width = 10,
       height = 6)
ggsave(overall_popggplot, 
       filename = "3. Graphs/Figure 1A.jpg",
       width = 10,
       height = 6)

#By race group


overall_pop_by_race <- household_population %>% 
  filter(income == "Overall", 
         race != "Overall")

write_csv(overall_pop_by_race,
          "2. Data Output/overall_pop_by_race.csv")

overall_pop_by_raceggplot <- overall_pop_by_race %>% 
  rowwise() %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = population,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    limits = c(0,1),
    labels = scales::percent,
    expand = c(0, 0)
    ) +
  labs(
    x = "",
    y = "Household population percentage\n(note y axis in square-root scale)",
    title = "Figure 1 by race: Household population,  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: Asian data was not disaggregated in the 2019 SCF")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(overall_pop_by_raceggplot, 
       filename = "3. Graphs/Figure 1C.pdf",
       width = 10,
       height = 6)
ggsave(overall_pop_by_raceggplot, 
       filename = "3. Graphs/Figure 1C.jpg",
       width = 10,
       height = 6)

#Population by income and race
overall_pop_by_racecat <- household_population %>% 
  filter(race != "Other",
         race != "Asian",
         race != "Overall",
         income != "Overall")

write_csv(overall_pop_by_racecat,
          file = "2. Data Output/overall_pop_by_racecat.csv")

overall_pop_by_racecatggplot <- overall_pop_by_racecat %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = population,
      ymin = low,
      ymax = high
    )
  )+
  facet_wrap(
    ~factor(
      income,
      levels = c(
        "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
      )
    ),
    scales = "free")+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, .4),
    labels = scales::percent
  ) +
  labs(
    x = "",
    y = "Household population percentage",
    title = "Figure 1 by race and income quantile: Household population,  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: There were not enough Asian or Other households\nto proprely disagregate by income quartile")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(overall_pop_by_racecatggplot, 
       filename = "3. Graphs/Figure 1D.pdf",
       width = 10,
       height = 6)
ggsave(overall_pop_by_racecatggplot, 
       filename = "3. Graphs/Figure 1D.jpg",
       width = 10,
       height = 6)


#########################################################
#Homeownership
#########################################################
#Homeownership

#Overall
overall_homeownership <- homeownership %>% 
  filter(race == "Overall",
         income == "Overall")

write_csv(overall_homeownership,
          "2. Data Output/overall_homeownership.csv")

overall_homeownershipggplot <- overall_homeownership %>% 
  ggplot(
    aes(
      x = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                 )
      ),
      y = percent-.60,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin =low-.6,
      ymax = high-.6
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
  )+
  scale_y_continuous( labels = c("60%",
                                 "62.5%",
                                 "65%",
                                 "67.5%"),
                      breaks = c(0,
                                 .025,
                                 .05,
                                 .075),
  #trans = scales::sqrt_trans(),
  limits = c(0,.075),
  expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Percent of households that own their home\n(note y axis does not start at zero)",
    title = "Figure 3A: Homeownership percentage, 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(overall_homeownershipggplot, 
       filename = "3. Graphs/Figure 3A.pdf",
       width = 10,
       height = 6)
ggsave(overall_homeownershipggplot, 
       filename = "3. Graphs/Figure 3A.jpg",
       width = 10,
       height = 6)


#By income
income_homeownership <- homeownership %>% 
  filter(race == "Overall",
         income != "Overall",)

write_csv(income_homeownership,
          "2. Data Output/income_homeownership.csv")

income_homeownershipggplot <- income_homeownership %>% 
  ggplot(
    aes(
      x = factor(
        income,
        levels = c(
          "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
        )
      ),
      y = percent,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin =low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous( labels = scales::percent,
                      #trans = scales::sqrt_trans(),
                      limits = c(0,1),
                      expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Percent of households that own their home",
    title = "Figure 3B: Homeownership percentage by income quantile, 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(income_homeownershipggplot, 
       filename = "3. Graphs/Figure 3B.pdf",
       width = 10,
       height = 6)
ggsave(income_homeownershipggplot, 
       filename = "3. Graphs/Figure 3B.jpg",
       width = 10,
       height = 6)


#By Race
race_homeownership <- homeownership %>% 
  filter(income == "Overall",
         race != "Overall",)

write_csv(race_homeownership,
          "2. Data Output/race_homeownership.csv")

race_homeownershipggplot <- race_homeownership %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      y = percent,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin =low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous( labels = scales::percent,
                      #trans = scales::sqrt_trans(),
                      limits = c(0,1),
                      expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Percent of households that own their home",
    title = "Figure 3C: Homeownership percentage by race, 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(race_homeownershipggplot, 
       filename = "3. Graphs/Figure 3C.pdf",
       width = 10,
       height = 6)
ggsave(race_homeownershipggplot, 
       filename = "3. Graphs/Figure 3C.jpg",
       width = 10,
       height = 6)


#By Racecat
race_income_homeownership <- homeownership %>% 
  filter(income != "Overall",
         race != "Overall",
         race!= "Asian",
         race != "Other")

write_csv(race_homeownership,
          "2. Data Output/race_homeownership.csv")

race_income_homeownershipggplot <- race_income_homeownership %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      y = percent,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin =low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous( labels = scales::percent,
                      #trans = scales::sqrt_trans(),
                      limits = c(0,1),
                      expand = c(0, 0)
  ) +
  facet_wrap(
    ~factor(
      income,
      levels = c(
        "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
      )
    )
    )+
  labs(
    x = "",
    y = "Percent of households that own their home",
    title = "Figure 3D: Homeownership percentage by race, 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(race_income_homeownershipggplot, 
       filename = "3. Graphs/Figure 3D.pdf",
       width = 10,
       height = 6)
ggsave(race_income_homeownershipggplot, 
       filename = "3. Graphs/Figure 3D.jpg",
       width = 10,
       height = 6)

#########################################################
#Net worth
#########################################################
#net worth

#Overall
median_net_worth_table <- net_worth %>% 
  filter(race == "Overall" &
           income == "Overall")

write_csv(median_net_worth_table,
          "2. Data Output/median_net_worth.csv")

median_net_worthggplot <- median_net_worth_table %>% 
  ggplot(
    aes(
      x = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                   )
                 ),
      y = median,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
          )
        ),
      ymin = low,
      ymax = high
      )
    )+
  geom_bar(
    position = "dodge",
    stat = "identity"
    )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
      ))+
  geom_errorbar(
    )+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0,220000),
                     expand = c(0, 0)
                     ) +
  labs(
    x = "",
    y = "Median net worth",
    title = "Figure 2 Overall: Median net worth 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(median_net_worthggplot, 
       filename = "3. Graphs/Figure 2A.pdf",
       width = 10,
       height = 6)
ggsave(median_net_worthggplot, 
       filename = "3. Graphs/Figure 2A.jpg",
       width = 10,
       height = 6)

#By income group


income_net_worth <- net_worth %>% 
  filter(race == "Overall",
         income != "Overall",)

write_csv(income_net_worth,
          "2. Data Output/income_net_worth.csv")

income_net_worthggplot <- income_net_worth %>% 
  ggplot(
    aes(
      x = factor(
        income,
        levels = c(
          "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
          )
        ),
      fill = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                   )
                 ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
  
    position = "dodge",
    stat = "identity"
    )+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    breaks = c(
      10000,
      50000,
      100000,
      250000,
      500000,
      750000,
      1500000),
    limits = c(0,1500001),
    expand = c(0, 0),
    labels = scales::dollar
    ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 2 by income quantile: Median net worth 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(income_net_worthggplot, 
       filename = "3. Graphs/Figure 2B.pdf",
       width = 10,
       height = 6)
ggsave(income_net_worthggplot,
       filename = "3. Graphs/Figure 2B.jpg",
       width = 10,
       height = 6)

#By race group


race_net_worth <- net_worth %>% 
  filter(income == "Overall",
         race != "Overall")

write_csv(race_net_worth,
          "2. Data Output/race_net_worth.csv")

race_net_worthggplot <- race_net_worth %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    limits = c(0,725000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth",
    title = "Figure 2 by race: Median net worth 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: Asian data was not disaggregated in the 2019 SCF")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(race_net_worthggplot, 
       filename = "3. Graphs/Figure 2C.pdf",
       width = 10,
       height = 6)
ggsave(race_net_worthggplot, 
       filename = "3. Graphs/Figure 2C.jpg",
       width = 10,
       height = 6)

#By race and income

racecat_net_worth <- net_worth %>% 
  filter(income != "Overall",
         race != "Overall",
         race != "Asian",
         race != "Other"
         )

write_csv(racecat_net_worth,
          "2. Data Output/racecat_net_worth.csv")

racecat_net_worthggplot <- racecat_net_worth %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  facet_wrap(
    ~factor(
      income,
      levels = c(
        "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
      )
    ),
    scales = "free"
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    limits = c(0,1750000),
    breaks = c(10000, 
               75000,
               250000,
               500000,
               1000000,
               1500000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 2 by race and income quantile: Median net worth  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: There were not enough Asian or Other households\nto proprely disagregate by income quartile")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 2D.pdf",
       width = 10,
       height = 6)
ggsave(racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 2D.jpg",
       width = 10,
       height = 6)

#########################################################
#Homeowner Net worth
#########################################################
#Homeowner net worth

#Overall
homeowner_median_net_worth <- homeowner_net_worth %>% 
  filter(race == "Overall" &
           income == "Overall")

write_csv(homeowner_median_net_worth,
          "2. Data Output/homeowner_median_net_worth.csv")

homeowner_median_net_worthggplot <- homeowner_median_net_worth %>% 
  ggplot(
    aes(
      fill = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                 )
      ),
      y = median,
      x = factor(
        homeowner,
        levels = c(0,1),
        labels = c("Not a homeowner",
                   "Homeowner")
      ),
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0,430000),
                     breaks = c(10000,
                                100000,
                                200000,
                                300000,
                                400000),
                     expand = c(0, 0),
                     trans = scales::sqrt_trans()
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 4A: Median net worth by home ownership (in 2022 dollars), 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(homeowner_median_net_worthggplot, 
       filename = "3. Graphs/Figure 4A.pdf",
       width = 10,
       height = 6)
ggsave(homeowner_median_net_worthggplot, 
       filename = "3. Graphs/Figure 4A.jpg",
       width = 10,
       height = 6)

#By income group


homeowner_income_net_worth <- homeowner_net_worth %>% 
  filter(race == "Overall",
         income != "Overall",)

write_csv(homeowner_income_net_worth,
          "2. Data Output/homeowner_income_net_worth.csv")

homeowner_income_net_worthggplot <- homeowner_income_net_worth %>% 
  ggplot(
    aes(
      x = factor(
        income,
        levels = c(
          "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  facet_wrap(~factor(homeowner,
                     levels = c(0,1),
                     labels = c("Not a homeowner",
                                "Homeowner")),
             scales = "free_y")+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    limits = c(0,3500000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 4B: Median net worth by home ownership and income quartile (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(homeowner_income_net_worthggplot, 
       filename = "3. Graphs/Figure 4B.pdf",
       width = 10,
       height = 6)
ggsave(homeowner_income_net_worthggplot,
       filename = "3. Graphs/Figure 4B.jpg",
       width = 10,
       height = 6)

#By race group


homeowner_race_net_worth <- homeowner_net_worth %>% 
  filter(income == "Overall",
         race != "Overall")

write_csv(homeowner_race_net_worth,
          "2. Data Output/homeowner_race_net_worth.csv")

homeowner_race_net_worthggplot <- homeowner_race_net_worth %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  facet_wrap(~factor(homeowner,
                     levels = c(0,1),
                     labels = c("Not a homeowner",
                                "Homeowner")),
             scales = "free_y")+
  scale_y_continuous(
    trans = "S_sqrt",
    limits = c(-15000,1000000),
    breaks = c(
      -10000,
      0,
      10000,
      100000,
      250000,
      500000,
      1000000
    ),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 4C: Median net worth by homeownership and race (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: Asian data was not disaggregated in the 2019 SCF")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(homeowner_race_net_worthggplot, 
       filename = "3. Graphs/Figure 4C.pdf",
       width = 10,
       height = 6)
ggsave(homeowner_race_net_worthggplot, 
       filename = "3. Graphs/Figure 4C.jpg",
       width = 10,
       height = 6)

#By race and income

homeowner_racecat_net_worth <- homeowner_net_worth %>% 
  filter(income != "Overall",
         race != "Overall",
         race != "Asian",
         race != "Other"
  )

write_csv(homeowner_racecat_net_worth,
          "2. Data Output/homeowner_racecat_net_worth.csv")

#have to filter by homeowner status

#homeowner racecat 

homeowner_racecat_net_worthggplot <- homeowner_racecat_net_worth %>% 
  filter(homeowner == 1) %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "White",
          "Black",
          "Hispanic"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  facet_wrap(~ factor(
    income,
    levels = c(
      "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
    )
  ),
  scales = "free")+
 
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = "S_sqrt",
    expand = c(0, 0),
    limits = c(0, 1900000),
    breaks = c(
               0,
               100000,
               500000,
               100000,
               1500000),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 4D1: Median net worth of homeowners, by race\nand income quartile (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: There were not enough Asian or Other households\nto proprely disagregate by home ownership and income quartile")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(homeowner_racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 4D1.pdf",
       width = 10,
       height = 6)
ggsave(homeowner_racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 4D1.jpg",
       width = 10,
       height = 6)


#Non homeowner racecat

nonhomeowner_racecat_net_worthggplot <- homeowner_racecat_net_worth %>% 
  filter(homeowner == 0) %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "White",
          "Black",
          "Hispanic"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  facet_wrap(~ factor(
    income,
    levels = c(
      "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
    )
  ),
             scales = "free")+
  
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = "S_sqrt",
    expand = c(0, 0),
    breaks = c(-100000,
               -10000,
               10000,
               100000,
               250000,
               500000,
               750000,
               1000000),
    limits = c(-137000, 1000001),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median net worth\n(note y axis in square-root scale)",
    title = "Figure 4D2: Median net worth of non-homeowners, by race\nand income quartile (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: There were not enough Asian or Other households\nto proprely disagregate by home ownership and income quartile")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(nonhomeowner_racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 4D2.pdf",
       width = 10,
       height = 6)
ggsave(nonhomeowner_racecat_net_worthggplot, 
       filename = "3. Graphs/Figure 4D2.jpg",
       width = 10,
       height = 6)

#########################################################
#Home equity
#########################################################
#Home equity

#Overall
median_home_equity <- home_equity %>% 
  filter(race == "Overall" &
           income == "Overall")

write_csv(median_home_equity,
          "2. Data Output/median_home_equity.csv")

median_home_equityggplot <- median_home_equity %>% 
  ggplot(
    aes(
      x = factor(year,
                 levels = c(
                   "2019",
                   "2022"
                 )
      ),
      y = median,
      fill = factor(
        year,
        levels = c(
          "2019",
          "2022"
        )
      ),
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
  )+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(0,220000),
                     expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = "Median home equity",
    title = "Figure 5A: Median home equity of homeowners (in 2022 dollars), 2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text = element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(median_home_equityggplot, 
       filename = "3. Graphs/Figure 5A.pdf",
       width = 10,
       height = 6)
ggsave(median_home_equityggplot, 
       filename = "3. Graphs/Figure 5A.jpg",
       width = 10,
       height = 6)

#By income group


income_home_equity <- home_equity %>% 
  filter(race == "Overall",
         income != "Overall",)

write_csv(income_home_equity,
          "2. Data Output/income_home_equity.csv")

income_home_equityggplot <- income_home_equity %>% 
  ggplot(
    aes(
      x = factor(
        income,
        levels = c(
          "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    #trans = scales::sqrt_trans(),
    breaks = c(
      100000,
      200000,
      300000,
      400000),
    limits = c(0,450000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median home equity",
    title = "Figure 5B: Median home equity by income quartile (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis \n of Federal Reserve Board's Survey of Consumer Finances")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(income_home_equityggplot, 
       filename = "3. Graphs/Figure 5B.pdf",
       width = 10,
       height = 6)
ggsave(income_home_equityggplot,
       filename = "3. Graphs/Figure 5B.jpg",
       width = 10,
       height = 6)

#By race group


race_home_equity <- home_equity %>% 
  filter(income == "Overall",
         race != "Overall",)

write_csv(race_home_equity,
          "2. Data Output/race_home_equity.csv")

race_home_equityggplot <- race_home_equity %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    limits = c(-30000,450000),
    breaks = c(-25000,
               25000,
               50000,
               100000,
               200000,
               300000,
               400000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median home equity",
    title = "Figure 5C: Median home equity by race (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: Asian data was not disaggregated in the 2019 SCF")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(race_home_equityggplot, 
       filename = "3. Graphs/Figure 5C.pdf",
       width = 10,
       height = 6)
ggsave(race_home_equityggplot, 
       filename = "3. Graphs/Figure 5C.jpg",
       width = 10,
       height = 6)

#By race and income

racecat_home_equity <- net_worth %>% 
  filter(income != "Overall",
         race != "Overall",
         race != "Asian",
         race != "Other"
  )

write_csv(racecat_home_equity,
          "2. Data Output/racecat_net_worth.csv")

racecat_home_equityggplot <- racecat_home_equity %>% 
  ggplot(
    aes(
      x = factor(
        race,
        levels = c(
          "Overall",
          "White",
          "Black",
          "Hispanic",
          "Asian",
          "Other"
        )
      ),
      fill = factor(year,
                    levels = c(
                      "2019",
                      "2022"
                    )
      ),
      y = median,
      ymin = low,
      ymax = high
    )
  )+
  facet_wrap(
    ~factor(
      income,
      levels = c(
        "Income: 0-20th percentile" , "20-40", "40-60", "60-80", "80-100"
      )
    ),
    scales = "free"
  )+
  geom_bar(
    position = "dodge",
    stat = "identity"
  )+
  scale_fill_manual(
    values = c(
      "light gray",
      "#2FB3a1"
    ))+
  geom_errorbar(
    
    position = "dodge",
    stat = "identity"
  )+
  scale_y_continuous(
    trans = scales::sqrt_trans(),
    limits = c(0,1750000),
    breaks = c(
               10000,
               100000,
               500000,
               1000000,
               1500000),
    expand = c(0, 0),
    labels = scales::dollar
  ) +
  labs(
    x = "",
    y = "Median home equity\n(note y axis in square-root scale)",
    title = "Figure 5D: Median home equity by race and income quartile (in 2022 dollars),  2019\u00ad2022",
    fill = "",
    caption = "Source: National Bankers Association Foundation analysis\nof Federal Reserve Board's Survey of Consumer Finances\nNote: There were not enough Asian or Other households\nto proprely disagregate by income quartile")+
  theme_bw()+
  theme(axis.text.x = element_text(#angle = 45,
    # vjust=  .8,
    # hjust = .9,
    face="bold"),
    title = element_text(face="bold"),
    text=element_text(family="Arial Rounded MT Bold"),
    legend.position = "bottom")

ggsave(racecat_home_equityggplot, 
       filename = "3. Graphs/Figure 5D.pdf",
       width = 10,
       height = 6)
ggsave(racecat_home_equityggplot, 
       filename = "3. Graphs/Figure 5D.jpg",
       width = 10,
       height = 6)
