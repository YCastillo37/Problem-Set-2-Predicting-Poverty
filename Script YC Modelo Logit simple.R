#Cargamos las bases de datos
base_test <- read_csv("test_unido.csv")
base_training <- read_csv("train_unido.csv")

# Quitamos la columna índice sin nombre
base_training <- base_training %>% select(-`...1`)
base_test     <- base_test %>% select(-`...1`)

#Exploramos los datos
prop.table(table(base_training$pobre))*100

#Agrupamos para graficar
data <- base_training %>% group_by(pobre) %>% tally()
data <- data %>% mutate(pobre = factor(pobre, levels = c(0,1), labels = c("no pobre","pobre")))

#Graficamos
ggplot(data, aes(x = pobre, y = n, fill = pobre)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("pobre" = "orange", "no pobre"= "blue")) +
  labs(x = "", y = "count")

#Convertimos la variable ciudad a factor
base_training$ciudad <- as.factor(base_training$ciudad)

#Convertimos a factor la variable pobre
base_training<- base_training %>% 
  mutate(
    pobre = factor(
      pobre,
      levels = c(0, 1),           # orden de los niveles (referencia primero)
      labels = c("No", "Yes")     # cómo se verán
    )
  )

# Limpia niveles vacíos en training
base_training <- droplevels(base_training)


#Creammos el factor 'educ' para categorizar
base_training <- base_training %>%
  mutate(
    educ = case_when(
      superior  == 1 ~ "superior",
      media     == 1 ~ "media",
      secundaria== 1 ~ "secundaria",
      TRUE           ~ "base"       # primaria/ninguna/otro (ajusta nombre si tienes esa info)
    ),
    educ = factor(educ, levels = c("base","secundaria","media","superior"))
  )

base_test <- base_test %>%
  mutate(
    educ = case_when(
      superior   == 1 ~ "superior",
      media      == 1 ~ "media",
      secundaria == 1 ~ "secundaria",
      TRUE            ~ "base"
    ),
    educ = factor(educ, levels = c("base","secundaria","media","superior"))
  )

#Visualizamos las variables de la base
str(base_training)

