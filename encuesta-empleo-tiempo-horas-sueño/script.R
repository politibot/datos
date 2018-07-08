library(here)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

setwd(here('encuesta-empleo-tiempo-horas-sueño/'))

# http://www.ine.es/dyngs/INEbase/es/operacion.htm?c=Estadistica_C&cid=1254736176815&menu=resultados&idp=1254735976608
# Actividades que se realizan en el transcurso del día y duración media diaria >
# 1.5 CUIDADOS PERSONALES. Dormir: Principales variables y sexo
df <- read_csv('2009-2010.csv') %>%
  rename(group = X1) %>%
  mutate(
    all = paste(all_hours, str_pad(all_minutes, 2, pad = "0"), sep = ':'),
    men = paste(men_hours, str_pad(men_minutes, 2, pad = "0"), sep = ':'),
    women = paste(women_hours, str_pad(women_minutes, 2, pad = "0"), sep = ':'),
    all_hours = NULL,
    all_minutes = NULL,
    men_hours = NULL,
    men_minutes = NULL,
    women_hours = NULL,
    women_minutes = NULL
  ) %>%
  mutate(
    all = as.POSIXct(all, format="%H:%M"),
    men = as.POSIXct(men, format="%H:%M"),
    women = as.POSIXct(women, format="%H:%M")
  )

# Trimestres
ggplot(df[7:10, ], aes(group, group = 1)) +
  geom_line(aes(y = all), colour="#66c2a5") +
  geom_line(aes(y = men), colour = "#8da0cb") +
  geom_line(aes(y = women), colour = "#fc8d62") +
  scale_x_discrete(labels= c('Enero a marzo', 'Abril a junio', 'Julio a septiembre', 'Octubre a diciembre')) +
  labs(x = "Trimestre", y = 'Horas') +
  scale_y_datetime() +
  theme_minimal()
  # ggsave('trimestres.pdf', width = 25.4, height = 14.816666667, units = "cm")

# Grupos de ead
ggplot(df[42:46, ], aes(group, group = 1)) +
  geom_line(aes(y = all), colour="#66c2a5") +
  geom_line(aes(y = men), colour = "#8da0cb") +
  geom_line(aes(y = women), colour = "#fc8d62") +
  scale_x_discrete(
    limits = c('- De 10 a 15 años', '- De 16 a 24 años', 'Edad: De 25 a 44 años', 'Edad: De 45 a 64 años', 'Edad: 65 ó más años'),
    labels= c('10-15 años', '16-24', '25-44', '45-64', '65+')
  ) +
  labs(x = "Grupos de edad", y = 'Horas') +
  scale_y_datetime() +
  theme_minimal()
  # ggsave('grupos_edad.pdf', width = 25.4, height = 14.816666667, units = "cm")

# Situación laboral
  ggplot(df[c(75,78,80,81), ], aes(group, group = 1)) +
  geom_line(aes(y = all), colour="#66c2a5") +
  geom_line(aes(y = men), colour = "#8da0cb") +
  geom_line(aes(y = women), colour = "#fc8d62") +
  scale_x_discrete(
    limits = c('Relación con la actividad y situación profesional respecto al trabajo principal: Ocupados', 'Relación con la actividad y situación profesional respecto al trabajo principal: Parados', 'Relación con la actividad y situación profesional respecto al trabajo principal: Estudiantes', 'Relación con la actividad y situación profesional respecto al trabajo principal: Jubilados o pensionistas'),
    labels= c('Ocupados', 'Parados', 'Estudiantes', 'Jubilados')
  ) +
  labs(x = "Situación laboral", y = 'Horas') +
  scale_y_datetime(limits = c(as.POSIXct('08:00', format="%H:%M"), as.POSIXct('09:40', format="%H:%M"))) +
  theme_minimal()
  # ggsave('situacion_laboral.pdf', width = 25.4, height = 14.816666667, units = "cm")

# Tipo de hogar
  ggplot(df[13:15, ], aes(group, group = 1)) +
  geom_line(aes(y = all), colour="#66c2a5") +
  geom_line(aes(y = men), colour = "#8da0cb") +
  geom_line(aes(y = women), colour = "#fc8d62") +
  scale_x_discrete(
    limits = c('Tipo de hogar en el que viven: Hogar unipersonal', 'Tipo de hogar en el que viven: Pareja sola', 'Tipo de hogar en el que viven: Pareja con hijos'),
    labels= c('Unipersonal', 'Pareja sola', 'Pareja con hijos')
  ) +
  labs(x = "Tipo de hogar", y = 'Horas') +
  scale_y_datetime() +
  theme_minimal()
  # ggsave('tipo_hogar.pdf', width = 25.4, height = 14.816666667, units = "cm")
