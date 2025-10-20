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
