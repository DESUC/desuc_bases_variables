# Unificar variables y agregar etiquetas.

library(tidyverse)
library(labelled)
library(readxl)

# Etiquetas

df_var_long <- read_excel('data/02-df_var_long_revisado.xlsx')

df_labels <- df_var_long %>%
  mutate(var = base,
         value_raw = tipo,
         label_raw = caso_general,
         .keep = 'none') %>%
  filter(!is.na(var))


df_labels <- df_labels %>%
  mutate(label = str_remove_all(label_raw,
                                pattern = str_c('.\\d.*\\.-',
                                                ' \\d$',
                                                sep = '|')))


v_label <- df_labels %>%
  pull(label, name = var)

v_values <- df_labels %>%
  pull(value_raw, name = var)

# Base con datos múltiples

df_multiple <- readRDS('data/03-df_multiple_unnest.rds')

df_multiple %>%
  head()

# Base con variables que están en la base unida, sobre las que se pueda agregar
# las etiquetas posteriormente.
df_var <- tibble(var = names(df_multiple)) %>%
  mutate(label = v_label[var],
         t_value = v_values[var])

# Agrupar preguntas multiples según el nombre de las variables entregadas
df_var <- df_var %>%
  mutate(grupo_preg = str_remove(var, '_\\d{1,2}$'))

df_var %>%
  slice(1, 9:13)


# Pregunta de respuesta única
df_multiple %>%
  count(tramo_edad)

# Pregunta de respuesta múltiple
df_multiple %>%
  count(across(starts_with('s1')))


# Unificar variables de elección simple ----

coacross <- function(...) {
  coalesce(!!!across(...))
}

# Grupo de variables por pregunta madre
df_var_nest <- df_var %>%
  nest(var = var) %>%
  rowwise() %>%
  mutate(var = list(flatten_chr(var)),
         n_var = length(var))

df_var_nest %>%
  count(label, grupo_preg, value) %>%
  filter(n > 1)

# Solo preguntas con 2 o más componentes
df_var_nest <- df_var_nest %>%
  filter(n_var > 1)

df_var_nest %>%
  head()

df_preg <- map2_dfc(df_var_nest$grupo_preg,
                    df_var_nest$var,
                    ~mutate(df_multiple, "{.x}" := coacross(any_of(.y)),
                            .keep = 'none'))

# Revisar preguntas missing.
df_preg %>%
  sample_frac(1/4) %>%
  naniar::vis_miss()

df_preg %>%
  count(s1)


df_base <- bind_cols(select(df_multiple, !c(unlist(df_var_nest$var))),
                     df_preg)

df_base <- df_base %>%
  relocate(df_var_nest$grupo_preg,
           .after = tramo_edad)

head(df_base)


# Etiquetar variables ----

v_label <- df_var_nest %>%
  pull(label, grupo_preg) %>%
  as.list()

var_label(df_base) <- v_label

var_label(df_base)


# Etiquetar niveles ----

v_values <- df_var %>%
  filter(!is.na(t_value)) %>%
  distinct(t_value, grupo_preg) %>%
  pull(t_value, grupo_preg) %>%
  as.list()

values <- list(t_edad = c('18-24'    = 1,
                          '25-34'    = 2,
                          '35-44'    = 3,
                          '45-54'    = 4,
                          '55 y más' = 5),
               t_sexo = c('Hombre' = 1,
                          'Mujer'  = 2),
               t_alzanzado = c('Se habrá retrocedido' = 1,
                               'Se estará igual'      = 2,
                               'Se habrá avanzado'    = 3,
                               'Se habrá alcanzado'   = 4))

value_list <- structure(values[as.character(v_values)],
                        names = names(v_values))

df_base <- df_base %>%
  set_value_labels(!!!value_list)

count(df_base,
      tramo_edad)

count(df_base,
      s1)


# Grabar base ----

saveRDS(df_base,
        'data/04-df_base_etiquetada.rds')
