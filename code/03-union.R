# Unión de bases a partir de revisión de variables
#

library(tidyverse)
library(readxl)

# Base de datos list-col
df_data <- readRDS('data/01-df_data.rds')
sort(df_data$ano)

# Empalme de variables
df_var_long <- read_excel('data/02-df_var_long_revisado.xlsx')


sort(df_var_wide$empresa_file)

tipo_base <- df_var_wide %>%
  pull(tipo, name = empresa_file)

# Caso base ----

label_vector <- function(.df,
                         empresa_filter = FALSE){
  # Función que crea vector para cambio de nombre según variables.
  df_labels <- .df %>%
    filter(empresa_file == empresa_filter) %>%
    select(X1:last_col()) %>%
    t() %>%
    as_tibble(rownames = 'preg')

  # Excluir variables NA.
  if(ncol(df_labels) == 2){
    df_labels %>%
      filter(!is.na(V1)) %>%
      pull(V1, name = preg)
  } else {
    tibble(preg = NA_character_)
  }
}

label_vector(df_var_wide, 'BASE MODELO') %>%
  head()

# Empresas que caen bajo BASE MODELO
df_data$empresa_file[!(df_data$empresa_file %in% df_var_wide$empresa_file)]

# Empresas particulares
df_data$empresa_file[df_data$empresa_file %in% df_var_wide$empresa_file]

# Agregar dato de filtro
df_data <- df_data %>%
  mutate(filtro = if_else(!(empresa_file %in% df_var_wide$empresa_file),
                          'BASE MODELO',
                          empresa_file),
         tipo = if_else(!(empresa_file %in% df_var_wide$empresa_file),
                        'base',
                        tipo_base[empresa_file]),
         .after = empresa_file)

table(df_data$tipo)

# Agregar vector de nombres

df_data <- df_data %>%
  rowwise() %>%
  dplyr::mutate(var_rename = list(label_vector(df_var_wide,
                                               empresa_filter = filtro)))
# Revisión de empresa repetida.
# df_var_wide %>%
#   filter(empresa_file == 'CCU (papel)')
#
# label_vector(df_var_wide,
#              empresa_filter == 'CCU (papel)')
#
# df_data$var_rename[[4]]

df_data <- df_data %>%
  mutate(data = list(rename(data, any_of(var_rename))))

# Base conjunta con respuestas múltiples por variable

var_strings <- c('empresa_file', 'filtro', 'tipo', 'archivo',
                 'P05_A_ABIERTA',
                 'P11_ABIERTA',
                 'P23_A_ABIERTA',
                 'P24_1_A_ABIERTA',
                 'P25_A_ABIERTA',
                 'P26_ABIERTA',
                 'NOMBRE', 'EMPRESA', 'CORREO', 'FECHA', 'IP',
                 'AGENT')

df_multiple_unnest <- df_data %>%
  filter(tipo %in% c('base', 'múltiples')) %>%
  select(empresa_file:data) %>%
  unnest(data) %>%
  type.convert() %>%
  mutate(across(c(any_of(var_strings),
                  starts_with('CAMPO')),
                as.character))

# Reemplazar missings por NA
df_multiple_unnest <- df_multiple_unnest %>%
  mutate(across(where(is.numeric), ~ na_if(., 99)))

empresa_1 <- empresa %>%
  filter(!is.na(6))


names(df_multiple_unnest)

df_multiple_unnest %>%
  count(empresa_file,
        '42.1.- (1) 8) RESPETO A SINDICATOS ¿Los empresarios y/o ejecutivos respetan la libertad de asociación de un empleado a un sindicato y no lo discriminan por afiliarse?')


df_multiple_unnest %>%
  count(P17_4_A_5,
        '22.1.- (1) Los criterios para definir quienes tienen la posibilidad de hacer teletrabajo y en qué medida, son justos y transparentes')


# Base conjunta con respuestas únicas por variable
df_unica_unnest <- df_data %>%
  filter(tipo %in% c('unica')) %>%
  select(empresa_file:data) %>%
  unnest(data) %>%
  type.convert() %>%
  mutate(across(c(any_of(var_strings),
                  starts_with('CAMPO')),
                as.character))

# Reemplazar missings por NA
df_unica_unnest <- df_unica_unnest %>%
  mutate(across(where(is.numeric), ~ na_if(., 99)))


# Guardar bases de datos ----

df_multiple_unnest %>%
  saveRDS('data/03-df_multiple_unnest.rds')

df_unica_unnest %>%
  saveRDS('data/03-df_unica_unnest.rds')

