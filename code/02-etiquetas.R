# Etiquetas y variables
#
# Obtener una planilla en que se pueda homologar las preguntas de las distintas
# fuentes.

library(tidyverse)
library(writexl)

df_data <- readRDS('data/01-df_data.rds')

# Código de variables de cada base de datos.
df_var_wide <- df_data %>%
  rowwise() %>%
  mutate(var_names = list(names(data))) %>%
  select(!data) %>%
  unnest_wider(var_names)

df_var_long <- df_var_wide %>%
  t() %>%
  as_tibble(.name_repair = 'minimal')

names(df_var_long) <- df_var_long[3, ]


write_xlsx(df_var_long,
           'data/02-df_var_long.xlsx')
