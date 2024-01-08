library(readxl)
QNA <- read_excel("C:/Users/92300/Downloads/QNA Tables for website.xlsx",
                                          skip = 1)
#View(QNA)
    ## Rename first two columns
colnames(QNA)[1:2] <- c("Serial", "sector")

## Rename columns 3:33 as 2015Q1, 2015Q2 and so on till 2023Q4
colnames(QNA)[3:35] <- c("2015Q1", "2015Q2", "2015Q3","2015Q4", "2016Q1", "2016Q2", "2016Q3","2016Q4", "2017Q1", "2017Q2", "2017Q3","2017Q4", "2018Q1", "2018Q2", "2018Q3","2018Q4", "2019Q1", "2019Q2", "2019Q3","2019Q4", "2020Q1", "2020Q2", "2020Q3","2020Q4", "2021Q1", "2021Q2", "2021Q3","2021Q4", "2022Q1", "2022Q2", "2022Q3","2022Q4", "2023Q1")

QNA <- QNA |> select(-c(36:38))
QNA$Serial <- na.locf(QNA$Serial)



## Convert this data to long format

QNA_long <- QNA |>
  pivot_longer(cols = -c(Serial, sector),
               names_to = "Year",
               values_to = "Value")



## Convert Year to yearqtr format and read as date
QNA_long$Year <- as.Date(as.yearqtr(QNA_long$Year, format = "%YQ%q"))

#QNA_long$Year <- as.yearqtr(QNA_long$Year, format = "%YQ%q")

library(janitor)
QNA_long <- QNA_long |>
  clean_names()

QNA_long |> glimpse()

QNA_long |> group_by(sector) |>
  summarise(n = n())

## Remove rows with missing values
QNA_long <- QNA_long |>
  drop_na()

QNA_long |> glimpse()

## Plot the data for Agriculture sector
QNA_long |>
  filter(sector == "Agriculture sector (1 to 4)") |>
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  labs(title = "Agriculture sector",
       x = "Year",
       y = "Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

## Plot the data for Manufacturing sector
QNA_long |>
  filter(sector == "2. Manufacturing (i+ii+iii)") |>
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  labs(title = "Manufacturing sector(i+ii+iii)",
       x = "Year",
       y = "Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



# Assuming your column is named 'sector'
QNA_final <- QNA_long %>%
  mutate(
    sector = case_when(
      grepl("Agriculture Sector", sector) ~ "Agriculture sector",
      grepl("Important Crops", sector) ~ "Important Crops",
      grepl("Other Crops", sector) ~ "Other Crops",
      grepl("Cotton Ginning", sector) ~ "Cotton Ginning",
      grepl("Crops", sector) ~ "Crops",
      grepl("Livestock", sector) ~ "Livestock",
      grepl("Forestry", sector) ~ "Forestry",
      grepl("Fishing", sector) ~ "Fishing",
      grepl("Industrial Sector ",sector) ~ "Industrial Sector",
      grepl("Mining & Quarrying", sector) ~ "Mining & Quarrying",
      grepl("Manufacturing", sector) ~ "Manufacturing",
      grepl("Large Scale", sector) ~ "Large Scale",
      grepl("Small Scale", sector) ~ "Small Scale",
      grepl("Slaughtering", sector) ~ "Slaughtering",
      grepl("3.  Electricity, gas and water supply", sector) ~ "Electricity, Gas and Water Supply",
      grepl("Construction", sector) ~ "Construction",
      grepl("Commodity Producing sector", sector) ~ "Commodity Producing sector",
      grepl("Services Sector", sector) ~ "Services Sector",
      grepl("Wholesale & Retail Trade", sector) ~ "Wholesale & Retail Trade",
      grepl("2. Transport& Storage", sector, ignore.case = TRUE) ~ "Transport & Storage",
      grepl("3. Accomodation and Food Services Activities (Hotels & Restaurants)", sector) ~ "Accommodation and Food Services",
      grepl("Information and Communication", sector) ~ "Information and Communication",
      grepl("Finance & Insurance Activities", sector) ~ "Finance & Insurance Activities",
      grepl("Real Estate Activities", sector) ~ "Real Estate Activities",
      grepl("Public Administration and Social Security", sector) ~ "Public Administration and Social Security",
      grepl("Education", sector) ~ "Education",
      grepl("Human Health and Social Work Activities", sector) ~ "Human Health and Social Work Activities",
      grepl("Other Private Services", sector) ~ "Other Private Services",
      grepl("Total of GVA", sector) ~ "Total of GVA",
      TRUE ~ sector  # keep the original value if none of the conditions are met
    )
  )

QNA_final |>  group_by(sector) |>
  summarise(n = n()) |> View()

