#Librerias 
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

#Importamos familia de letra
font_add_google("Montserrat", "Montserrat")  # O cualquier fuente similar, p. ej. "Poppins"
showtext_auto()

#Link de descarga de datos
#https://population.un.org/wpp/assets/Excel%20Files/5_Archive/WPP2019-Excel-files.zip

#Importar datos 
#Hombres 
males <- read_excel("WPP2019-Excel-files/1_Population/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx",
sheet = "ESTIMATES", skip = 16)

males <- clean_names(males) 

males <- males %>% 
  filter(country_code == 410) %>%  #Filtramos corea
  filter(reference_date_as_of_1_july == 2020) %>% #Filtramos estimaciones 2020
  pivot_longer(c(x0_4:x100), names_to = "grupo_edad", values_to = "pop") %>%  #Pasamos a formato long 
  mutate(grupo_edad = str_replace(grupo_edad, "_", " a ")) %>% #quitamos las equis y los guiones
  mutate(grupo_edad = str_remove(grupo_edad, "x")) %>%  
  select(region_subregion_country_or_area, reference_date_as_of_1_july, grupo_edad, pop) %>% #seleccionamos variables 
  rename(country = region_subregion_country_or_area ) %>% #cambiamos nombres
  rename(yea = reference_date_as_of_1_july) %>% 
  mutate(sex = "male") #agregamos una columna de sexo 


#Mujeres 
females <- read_excel("WPP2019-Excel-files/1_Population/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx",
                    sheet = "ESTIMATES", skip = 16)

females <- clean_names(females) 

females <- females %>% 
  filter(country_code == 410) %>%  #Filtramos corea
  filter(reference_date_as_of_1_july == 2020) %>% #Filtramos estimaciones 2020
  pivot_longer(c(x0_4:x100), names_to = "grupo_edad", values_to = "pop") %>%  #Pasamos a formato long 
  mutate(grupo_edad = str_replace(grupo_edad, "_", " a ")) %>% #quitamos las equis y los guiones
  mutate(grupo_edad = str_remove(grupo_edad, "x")) %>%  
  select(region_subregion_country_or_area, reference_date_as_of_1_july, grupo_edad, pop) %>% #seleccionamos variables 
  rename(country = region_subregion_country_or_area ) %>% #cambiamos nombres
  rename(yea = reference_date_as_of_1_july) %>% 
  mutate(sex = "female") #agregamos una columna de sexo 


#Merge 
pop <- rbind(males, females)
pop <- pop %>%
  mutate(pop_p = if_else(sex == "male", as.numeric(pop) * -1, as.numeric(pop)))

pop2 <- pop %>%
  mutate(grupo_edad = str_replace_all(grupo_edad, c("^x" = "", "_" = "a")),
         age_start = as.integer(str_extract(grupo_edad, "\\d+")),
         grupo_edad = fct_reorder(grupo_edad, age_start))


pop2 %>%
  ggplot(aes(x = grupo_edad, y = pop_p / 1000, fill = sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  geom_hline(yintercept = 0, linewidth = 0.85, color = "white") +
  scale_y_continuous(
    limits = c(-3.25, 3.25),
    breaks = seq(-3, 3, 0.5),
    labels = abs(seq(-3, 3, 0.5))) +
  annotate(geom = "text", x = 7, y = -2.5, label = "Hombres", size = 5) +
  annotate(geom = "text", x = 7, y = 2.5, label = "Mujeres", size = 5) +
  annotate(geom = "text", x = 21, y = 2, label = "Población total: 51.26 millones", size = 5, fontface = "bold") +
  scale_fill_manual(values = c("male" = "#DC4F4F", "female" = "#167742")) +
  theme_minimal(base_family = "Montserrat") +
  labs(title = "Pirámide de Población 2020 — Corea del Sur",
       x = "Grupo de edad (Años)", y = "Población (100 mil habitantes)", 
       caption = "Fuente: Elaboración Propia con estimaciones de World Population Prospects, 2019")+
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # título grande y en negritas
    axis.title = element_text(size = 14),                             # títulos de ejes más grandes
    axis.text = element_text(size = 11),                              # etiquetas de ejes legibles
    plot.caption = element_text(size = 10, face = "italic", hjust = 1) # caption en itálica
  )



