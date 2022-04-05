library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)


info = "https://raw.githubusercontent.com/JauniusI/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv"
duomenys = read_csv(info)
summary(duomenys)
duomenys = duomenys %>%
  filter(ecoActCode == 702200)

# Pirma uzduotis----

ggplot(duomenys,aes(avgWage)) + geom_histogram(bins = 40, color = "black") +
  labs(title = "Histogram of average wage", x = "Average wage", y = "Count") +
  theme_bw()


# Antra uzduotis----

class(duomenys$month)
duomenys$month = as.Date(paste0(as.character(duomenys$month), '01'), format = "%Y%m%d")

PerMetus = duomenys %>%
  group_by(name) %>%
  select(name,avgWage,month) %>%
  mutate(IsViso = tapply(avgWage,name,sum))
  
top5 = PerMetus %>%
  select(name,IsViso) %>%
  arrange(desc(IsViso)) %>%
  unique %>%
  head(5)


duomenys %>%
  filter(name %in% top5$name) %>%
  ggplot(aes(x=month,y=avgWage, group = name)) +
  geom_line(aes(color = name), size=0.75) +
  labs(x= "Month", y = "Average Wage", color = "Company name") +
  theme_bw()
  
# Trecia uzduotis----

Apdrausti = duomenys %>%
  filter(name %in% top5$name) %>%
  group_by(name) %>%
  mutate(ApdraustiPerMetus = tapply(numInsured,name,sum))

Apdrausti = Apdrausti %>%
  select(name, ApdraustiPerMetus) %>%
  arrange(desc(ApdraustiPerMetus)) %>%
  unique

Apdrausti %>%
ggplot() + geom_col(data = Apdrausti, aes(x=reorder(name, -ApdraustiPerMetus),y=ApdraustiPerMetus,color = name,fill = name))+
  labs(title = "Number of workers insured during the year",
       x = "Company name",
       y = "Insured workers") +
  theme_bw()







