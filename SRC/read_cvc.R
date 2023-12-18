#install.packages("openintro")
#install.packages("tidyverse")

library(openintro)
library(tidyverse)
library(ggplot2)

suicide_data = read.csv("D:/Work/Univer/IIIcurs/ADlabs/personal dataset/Death_rates_for_suicide__by_sex__race__Hispanic_origin__and_age__United_States.csv")

head(suicide_data)
glimpse(suicide_data)

summary(suicide_data)

suicide_data %>%
  distinct(INDICATOR)

subset_data <- suicide_data[, c("ESTIMATE", "STUB_LABEL", "AGE", "YEAR")]

# Проверяем первые строки данных
head(subset_data)

subset_data <- subset_data %>%
  mutate(Sex = case_when(
    grepl("Female", STUB_LABEL) ~ "Female",
    grepl("Male", STUB_LABEL) ~ "Male",
    TRUE ~ NA_character_
  ),
  Race = case_when(
    grepl("Black or African American", STUB_LABEL) ~ "Black or African American",
    grepl("White", STUB_LABEL) ~ "White",
    grepl("American Indian or Alaska Native", STUB_LABEL) ~ "American Indian or Alaska Native",
    TRUE ~ NA_character_
  ))

# Преобразование переменной Раса в фактор
subset_data$Race <- factor(subset_data$Race)
subset_data$Sex <- factor(subset_data$Sex)
subset_data$AGE <- factor(subset_data$AGE)

# Удаление старой переменной STUB_LABEL и ненужных числовых кодов, если они есть
subset_data <- subset(subset_data, select = -c(STUB_LABEL))

# Выводим первые строки обновленного датафрейма
head(subset_data)

# Получаем типы переменных для всей таблицы
variable_types <- sapply(subset_data, class)

# Выводим результат
print(variable_types)


# Создание графика плотности
ggplot(subset_data, aes(x = ESTIMATE, fill = as.factor(YEAR))) +
  geom_density(alpha = 0.7) +
  labs(title = "Плотность распределения оценок смертности от самоубийств по годам",
       x = "Оценка смертности",
       fill = "Год") +
  theme_minimal()

# Создание графика плотности с одной линией
ggplot(subset_data, aes(x = ESTIMATE, fill = "All Years")) +
  geom_density(alpha = 0.7) +
  labs(title = "Плотность распределения оценок смертности от самоубийств по всем годам",
       x = "Оценка смертности",
       fill = "Год") +
  theme_minimal()



# Создание гистограммы
ggplot(subset_data, aes(x = ESTIMATE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Распределение оценок смертности от самоубийств",
       x = "Оценка смертности",
       y = "Частота") +
  theme_minimal()

# Создание точечного графика
ggplot(subset_data, aes(x = YEAR, y = ESTIMATE)) +
  geom_point() +
  labs(title = "Тенденции оценок смертности от самоубийств",
       x = "Год",
       y = "Оценка смертности") +
  theme_minimal()

# Создание ящика с усами
ggplot(subset_data, aes(x = YEAR, y = ESTIMATE)) +
  geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Распределение оценок смертности от самоубийств по годам",
       x = "Год",
       y = "Оценка смертности") +
  theme_minimal()

# Создание столбчатой диаграммы без легенды
ggplot(subset_data, aes(x = AGE, y = ESTIMATE, fill = AGE)) +
  geom_bar(stat = "identity") +
  labs(title = "Оценки смертности от самоубийств по возрастным группам",
       x = "Возрастная группа",
       y = "Оценка смертности") +
  theme_minimal() +
  theme(legend.position="none")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Создание ящика с усами
ggplot(subset_data, aes(x = AGE, y = ESTIMATE, fill = AGE)) +
  geom_boxplot() +
  labs(title = "Распределение оценок смертности от самоубийств по возрастным группам",
       x = "Возрастная группа",
       y = "Оценка смертности") +
  theme_minimal() +
  theme(legend.position="none")  +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Создаем график плотности
ggplot(subset_data, aes(x = ESTIMATE, fill = as.factor(YEAR))) +
  geom_density(alpha = 0.5) +
  labs(title = "График плотности оценок по годам",
       x = "ESTIMATE",
       y = "Density",
       fill = "Year") +
  theme_minimal()


data_cleaned <- subset_data[complete.cases(subset_data$ESTIMATE), ]

mean_data <- data_cleaned %>%
  group_by(YEAR) %>%
  summarize(AVG_ESTIMATE = mean(ESTIMATE))

# Создаем ящик с усами
ggplot(subset_data, aes(x = Race, y = ESTIMATE, fill = Race)) +
  geom_boxplot() +
  labs(title = "Boxplot для анализа целевых групп по Расе",
       y = "ESTIMATE") +
  theme_minimal() +
  theme(legend.position="none")


# Создаем линейный график
ggplot(mean_data, aes(x = YEAR, y = AVG_ESTIMATE)) +
  geom_line(size = 1.3) +
  labs(title = "Сравнительный анализ изменения оценки в разные годы",
       x = "Year",
       y = "ESTIMATE") +
  theme_minimal()


summary(subset_data)
summary(data_cleaned)





# Линейная регрессия
model <- lm(AVG_ESTIMATE ~ YEAR, data = mean_data)

# Предсказание AVG_ESTIMATE на ближайшие 5 лет
next_years <- data.frame(YEAR = seq(max(mean_data$YEAR) + 1, max(mean_data$YEAR) + 5))
predicted_values <- predict(model, newdata = next_years)

# Расчет R-квадрата
r_squared <- summary(model)$r.squared
cat("R-квадрат модели:", r_squared, "\n")

# Построение графика
ggplot(mean_data, aes(x = YEAR, y = AVG_ESTIMATE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(data = next_years, aes(y = predicted_values), color = "red", size = 3) +
  labs(title = "Линейная регрессия и предсказание на ближайшие 5 лет",
       x = "Год",
       y = "AVG_ESTIMATE") +
  theme_minimal()

summary(predicted_values)
summary(linear_model)




# Задайте полный путь к файлу CSV
puti <- file.path("D:/Work/Univer/IIIcurs/ADlabs/personal dataset/datasets", "suicide_data.csv")

# Сохраните датасет в CSV файл
write.csv(suicide_data, file = puti, row.names = FALSE)

# Подтвердите успешное сохранение
print("Датасет успешно сохранен в CSV файл.")



# Задайте полный путь к файлу CSV
puti <- file.path("D:/Work/Univer/IIIcurs/ADlabs/personal dataset/datasets", "subset_data.csv")

# Сохраните датасет в CSV файл
write.csv(subset_data, file = puti, row.names = FALSE)

# Подтвердите успешное сохранение
print("Датасет успешно сохранен в CSV файл.")


# Задайте полный путь к файлу CSV
puti <- file.path("D:/Work/Univer/IIIcurs/ADlabs/personal dataset/datasets", "mean_data.csv")

# Сохраните датасет в CSV файл
write.csv(mean_data, file = puti, row.names = FALSE)

# Подтвердите успешное сохранение
print("Датасет успешно сохранен в CSV файл.")





# Года, которые нужно удалить
years_to_remove <- c(1950, 1960, 1970)

# Создание новой таблицы без указанных годов
new_data <- mean_data[!(mean_data$YEAR %in% years_to_remove), ]

# Вывод исходных данных
cat("Исходные данные:\n")
print(mean_data)

# Вывод новой таблицы данных
cat("\nНовая таблица данных без указанных годов:\n")
print(new_data)


# Линейная регрессия
model <- lm(AVG_ESTIMATE ~ YEAR, data = new_data)

# Предсказание AVG_ESTIMATE на ближайшие 5 лет
next_years <- data.frame(YEAR = seq(max(new_data$YEAR) + 1, max(new_data$YEAR) + 5))
predicted_values <- predict(model, newdata = next_years)

# Расчет R-квадрата
r_squared <- summary(model)$r.squared
cat("R-квадрат модели:", r_squared, "\n")

# Построение графика
ggplot(new_data, aes(x = YEAR, y = AVG_ESTIMATE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(data = next_years, aes(y = predicted_values), color = "red", size = 3) +
  labs(title = "Линейная регрессия и предсказание на ближайшие 5 лет",
       x = "Год",
       y = "AVG_ESTIMATE") +
  theme_minimal()

summary(predicted_values)

summary(suicide_data)


