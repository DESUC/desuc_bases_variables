# Lectura de bases de datos.

library(tidyverse)
library(writexl)

files_path <- list.files(path = 'data',
                         pattern = '.*csv$',
                         recursive = TRUE, full.names = TRUE)

# Separar datos a partir de path de archivo
files_path <- tibble(files_path = files_path) %>%
  separate(files_path,
           into = c('folder', 'archivo'),
           sep = '/',
           remove = FALSE)


# Remover el nombre nombre de la encuesta.
files_path$ano <- str_extract(files_path$archivo, '20\\d{2}')


test <- read_csv2(files_path$files_path[[1]],
                  skip = 4,
                  locale = locale(encoding = 'UTF-8'),
                  col_types = cols(.default = 'c'),
                  progress = FALSE)

head(test)


f_read_file <- function(path) {
  # Lectura de base de datos separada por tab.

  df <- read_csv2(path,
                  skip = 4, #salto de filas vacias
                  locale = locale(encoding = 'UTF-8'),
                  col_types = cols(.default = 'c'),
                  progress = FALSE)

  print(paste0(path, ' n = ', nrow(df)))

  # Eliminar espacios extra en nombre de variables.
  names(df) <- str_squish(names(df))

  return(df)
}

# Base de datos general
df_data <- files_path %>%
  rowwise() %>%
  mutate(data = list(f_read_file(files_path)))

# Base de bases
head(df_data)

# Base de Bicentenario 2015
head(df_data$data[[1]])


# Agrego información sobre variables y casos por base.
df_data <- df_data %>%
  mutate(casos = list(dim(data)), .after = archivo,
         n_casos = pluck(casos, 1),
         n_var = pluck(casos, 2)) %>%
  select(!casos)

head(df_data)

# Eliminar planillas poco significativas:
df_data_filter <- df_data %>%
  filter(n_var > 2, # Con más de dos variables
         n_casos > 10, # Con más de 10 respuestas
  ) %>%
  ungroup()

head(df_data_filter)

# Tibble de archivos con listcol con bases
saveRDS(df_data_filter,
        'data/01-df_data.rds')


# Se pueden unir las bases de datos en este momento pero tienen variables
# con diferente nombre.
df_casos <- df_data_filter %>%
  unnest(data) %>%
  type_convert()

glimpse(df_casos)

head(df_casos)
