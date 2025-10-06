#Librerías 
library(tidyverse)
library(janitor)
library(haven)
library(readxl)
library(stringr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(viridis)
library(showtext)

font_add_google("Montserrat", "Montserrat")  # O cualquier fuente similar, p. ej. "Poppins"
showtext_auto()

#Radio de Dependencia de Adultos mayores
dep_ratio <- read_excel("WPP2019-Excel-files/1_Population/WPP2019_POP_F13_A_OLD_AGE_DEPENDENCY_RATIO_1564.xlsx", 
                        sheet = "ESTIMATES", na = "0", skip = 16)
View(dep_ratio)

old_age <- dep_ratio %>% 
  clean_names() %>% 
  filter(country_code == 410) %>% 
  pivot_longer(c(x1950:x2020), names_to = "year", values_to = "oldage_dpr") %>% 
  mutate(year = str_remove(year, "x")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(country = "South Korea") %>% 
  select(country, year, oldage_dpr) %>% 
  mutate(oldage_dpr = as.numeric(oldage_dpr))

#Radio de Dependencia de Infantes 

dep_ratio <- read_excel("WPP2019-Excel-files/1_Population/WPP2019_POP_F12_A_CHILD_DEPENDENCY_RATIO_1564.xlsx", 
                        sheet = "ESTIMATES", na = "0", skip = 16)


infant <- dep_ratio %>% 
  clean_names() %>% 
  filter(country_code == 410) %>% 
  pivot_longer(c(x1950:x2020), names_to = "year", values_to = "infant_dpr") %>% 
  mutate(year = str_remove(year, "x")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(country = "South Korea") %>% 
  select(country, year, infant_dpr) %>% 
  mutate(infant_dpr = as.numeric(infant_dpr))

#Radio de dependencia total 

dep_ratio <- read_excel("WPP2019-Excel-files/1_Population/WPP2019_POP_F11_A_TOTAL_DEPENDENCY_RATIO_1564.xlsx", 
                        sheet = "ESTIMATES", na = "0", skip = 16)
total <- dep_ratio %>% 
  clean_names() %>% 
  filter(country_code == 410) %>% 
  pivot_longer(c(x1950:x2020), names_to = "year", values_to = "total_dpr") %>% 
  mutate(year = str_remove(year, "x")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(country = "South Korea") %>% 
  select(country, year, total_dpr) %>% 
  mutate(total_dpr = as.numeric(total_dpr))

dependency_ratio <- cbind(old_age, infant, total)

dependency_ratio$anio <- dependency_ratio$year

sk_dpr <- dependency_ratio %>% 
  select(anio, oldage_dpr, infant_dpr, total_dpr)


sk_dpr %>% 
  mutate(total_check = oldage_dpr + infant_dpr) %>% #Verificamos que se correcto
  
  sk_dpr <- sk_dpr %>% 
  mutate(P_oldage= oldage_dpr/total_dpr) %>% 
  mutate(P_infant = infant_dpr/total_dpr)



library(ggplot2)


ggplot(sk_dpr) +
  aes(x = anio) +
  geom_line(aes(y = infant_dpr, color = "Infantil"), linewidth = 1) +
  geom_line(aes(y = total_dpr, color = "Total"), linewidth = 1) +
  geom_line(aes(y = oldage_dpr, color = "Edad mayor"), linewidth = 1) +
  geom_vline(xintercept = 2016, linewidth = 0.85, color = "red", linetype = "dashed") +
  scale_color_manual(
    name = "Tipo de dependencia",
    values = c(
      "Infantil" = "#DC4F4F",
      "Total" = "#000000",
      "Edad mayor" = "#167742"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = seq(0, 100, 20)
  ) +
  labs(
    x = "Año",
    y = "Radio de dependencia",
    title = "Radios de dependencia en Corea del Sur: Infantil, De edad mayor y Total (1950–2020)",
    caption = "Fuente: Elaboración propia con estimaciones de World Population Prospects"
  ) +
  theme_light(base_family = "Montserrat") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1)
  )


