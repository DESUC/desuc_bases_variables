# Unión de bases a partir de revisión de variables
#

library(tidyverse)
library(readxl)

# Base de datos list-col
df_data <- readRDS('data/01-df_data.rds')
sort(df_data$ano)

# Empalme de variables
df_var_long <- read_excel('data/02-df_var_long_revisado.xlsx')

# bases de datos presentes en nuestro diccionario
head(df_var_long)


# Caso base ----

label_vector <- function(.df,
                         var_select = NULL){

  # Función que crea vector para cambio de nombre según variables.
  df_labels <- .df %>%
    select(base, {{ var_select }}) %>%
    na.omit()

  # Excluir variables NA.
  if(ncol(df_labels) == 2){
    df_labels %>%
      filter(!is.na(base)) %>%
      pull({{ var_select }}, name = base)
  } else {
    tibble(preg = NA_character_)
  }
}

label_vector(df_var_long, bicentenario_2018.csv) %>%
  head()

# Nombres de base de diccionario de etiquetas.
names_df_var_long <- names(df_var_long)

# Bases que caen bajo Caso general
df_data$archivo[!(df_data$archivo %in% names_df_var_long)]

# Bases que caen en casos particulares
df_data$archivo[df_data$archivo %in% names_df_var_long]

# Agregar dato de tipo_var para cada base
df_data <- df_data %>%
  mutate(var_codes = if_else(!(archivo %in% names_df_var_long),
                             'caso_general',
                             archivo),
         .after = archivo)


# Agregar vector de nombres
df_data <- df_data %>%
  rowwise() %>%
  dplyr::mutate(var_rename = list(label_vector(df_var_long,
                                               var_select = all_of(var_codes))))


# Hago el cambio de nombre gracias al vector nominado agregado en la etapa anterior.
df_data <- df_data %>%
  mutate(data = list(rename(data, any_of(var_rename))))

df_data$data[[1]] %>%
  head()


# Base conjunta con respuestas múltiples por variable

df_multiple_unnest <- df_data %>%
  select(archivo:data) %>%
  unnest(data) %>%
  type.convert(as.is = TRUE)


df_multiple_unnest %>%
  head()

df_multiple_unnest %>%
  count(ano)


# Ajustar variables
df_multiple_unnest %>%
  count(sexo)

cambio_var <- c(Hombre = 1,
                Mujer = 2)

df_multiple_unnest <- df_multiple_unnest %>%
  mutate(sexo = case_when(sexo == 'Hombre' ~ 1L,
                          sexo == 'Mujer' ~ 2L,
                          TRUE ~ as.integer(sexo)))

df_multiple_unnest %>%
  count(sexo)


# Reemplazar missings por NA
df_multiple_unnest <- df_multiple_unnest %>%
  mutate(across(where(is.numeric), ~ na_if(., 9)))

head(df_multiple_unnest)


# Guardar bases de datos ----

df_multiple_unnest %>%
  saveRDS('data/03-df_multiple_unnest.rds')
