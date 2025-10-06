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

#Importamos datos
fertility <- read_excel("WPP2019-Excel-files/2_Fertility/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx",
sheet = "ESTIMATES", na = "0", skip = 16)
View(fertility)

#Estimación de la tasa global de fecundidad 
fertility <- fertility %>% 
  clean_names() %>% 
  filter(country_code == 410) #filtro 

fertility %>% 
  select(country_code, period, c(x15_19:x45_49)) %>% 
  mutate(across(x15_19:x45_49, as.numeric)) %>% 
  group_by(period) %>% 
  rowwise() %>% 
  mutate(TGF = sum(c_across(x15_19:x45_49), na.rm = TRUE) / 1000 * 5) %>% 
  ungroup() %>% 
  ggplot(aes(x = period)) +
  geom_line(aes(x= period, y=TGF, group = country_code), 
            linewidth = 1, 
            color= "#dc4f4f") +
  geom_hline(yintercept = 2.1, linewidth = 0.85, color = "gray", linetype = "dashed") +
  scale_y_continuous(
    limits = c(0, 7),
    breaks = seq(0, 7, 0.5),
    labels = seq(0, 7, 0.5)
  ) +
  labs(
    x = "Año",
    y = "Tasa Global de Fecundidad",
    title = "Tasa Global de Fecundidad 1950-2020, Corea del Sur",
    caption = "Fuente: Elaboración propia con estimaciones de World Population Prospects"
  ) +
  annotate(geom = "text", x = 9, y = 2.5, label = "Nivel de Reemplazo (2.1)", size = 4, fontface = "italic") +
  annotate(geom = "text", x = 2, y = 6.5, label = "6.3", size = 4, fontface = "bold", color =  "#dc4f4f") +
  annotate(geom = "text", x = 14, y = 1.4, label = "1.11", size = 4, fontface = "bold", color =  "#dc4f4f") +
  theme_light(base_family = "Montserrat") +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 11),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



