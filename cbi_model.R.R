# Ustawienie katalogu roboczego
setwd("C:\\Users\\Komputer\\Desktop\\Univeristy\\Semestr 5\\EKONOMETRIA\\Model\\Dane\\Model_robocze")

# Wczytanie danych
data <- read.csv("dane.csv", header = TRUE, sep = ";", dec = ",", encoding = "UTF-8", na.strings = c(" ", "NA"))


# Załadowanie pakietu dplyr
if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Uzupełnianie brakujących danych zamiast ich usuwania
# Imputacja średnią dla zmiennych liczbowych
data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Sprawdzenie, czy brakujące dane zostały uzupełnione
print("Liczba brakujących wartości po imputacji:")
print(colSums(is.na(data)))


# Statystyki opisowe
summary(data) # Używamy poprawnej nazwy obiektu

# Obliczanie statystyk opisowych (psych)
if (!require(psych)) install.packages("psych")
library(psych)
statystyki_opisowe <- describe(data)

# Wyświetlanie statystyk opisowych
print(statystyki_opisowe)

# Przekształcenie danych do numerycznych kolumn
data_numeric <- data[sapply(data, is.numeric)] # Wybieramy tylko kolumny liczbowe

# Tworzenie macierzy korelacji Spearmana
data_numeric <- data[sapply(data, is.numeric)]
macierz_korelacji <- cor(data_numeric, method = "spearman", use = "complete.obs")

# Instalacja i załadowanie pakietu "corrplot"
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Wizualizacja macierzy korelacji Spearmana
corrplot(macierz_korelacji, 
         method = "number",        
         type = "upper",           
         tl.col = "black",         
         tl.cex = 0.8,             
         number.cex = 0.8,         
         number.font = 2,          
         number.color = "darkblue",
         diag = FALSE)

# Zapis wykresu do pliku PNG
png("korelacje_spearman.png", width = 1200, height = 1200, res = 150)
corrplot(macierz_korelacji, 
         method = "number", 
         type = "upper", 
         tl.col = "black", 
         tl.cex = 0.8, 
         number.cex = 1, 
         cl.cex = 1.2, 
         diag = FALSE)
dev.off()

# Histogram zmiennej objaśnianej (CPI)
hist_CPI <- hist(data$CPI,
                 breaks = 10,                     # Liczba przedziałów
                 col = "skyblue",                # Kolor słupków
                 main = "Rozkład zmiennej objaśnianej (CPI)",
                 xlab = "CPI",
                 ylab = "Częstość",
                 freq = TRUE)                    # Histogram na liczebność

# Dodanie poprawionej linii gęstości
density_CPI <- density(data$CPI, na.rm = TRUE)
lines(density_CPI$x, density_CPI$y * max(hist_CPI$counts) / max(density_CPI$y),
      col = "purple", lwd = 2)

# Histogram logarytmu zmiennej objaśnianej
hist_log_CPI <- hist(log(data$CPI),
                     breaks = 10,                 # Liczba przedziałów
                     col = "skyblue",
                     main = "Rozkład logarytmu zmiennej objaśnianej (log(CPI))",
                     xlab = "log(CPI)",
                     ylab = "Częstość",
                     freq = TRUE)

# Dodanie poprawionej linii gęstości
density_log_CPI <- density(log(data$CPI), na.rm = TRUE)
lines(density_log_CPI$x, density_log_CPI$y * max(hist_log_CPI$counts) / max(density_log_CPI$y),
      col = "purple", lwd = 2)


# Utworzenie kolumny log(CPI)
data$log_CPI <- ifelse(data$CPI > 0, log(data$CPI), NA)

# Statystyki opisowe dla zmiennej objaśnianej (CPI i log(CPI))
summary(data$CPI)
summary(data$log_CPI)

# Bardziej szczegółowa analiza za pomocą pakietu psych
if (!require(psych)) install.packages("psych")
library(psych)
describe(data[, c("CPI", "log_CPI")])

# Tabela dla zmiennej objaśnianej
data_summary_dependent <- data.frame(
  "Nazwa zmiennej" = c("CPI", "Logarytm CPI"),
  "Liczba obserwacji" = c(sum(!is.na(data$CPI)), sum(!is.na(data$log_CPI))),
  "Minimum" = c(min(data$CPI, na.rm = TRUE), min(data$log_CPI, na.rm = TRUE)),
  "Maksimum" = c(max(data$CPI, na.rm = TRUE), max(data$log_CPI, na.rm = TRUE)),
  "Średnia" = c(mean(data$CPI, na.rm = TRUE), mean(data$log_CPI, na.rm = TRUE))
)




# Wczytanie potrzebnych bibliotek
if (!require(flextable)) install.packages("flextable")
if (!require(officer)) install.packages("officer")
library(flextable)
library(officer)

# Tabela dla zmiennej objaśnianej
ft_dependent <- flextable(data_summary_dependent)
ft_dependent <- set_caption(ft_dependent, "Tabela 1. Statystyki opisowe zmiennej objaśnianej")

# Tabela dla zmiennych objaśniających
data_summary_independent <- data.frame(
  "Nazwa zmiennej" = c("CBI", "GDP_per_capita", "GDP_per_capita_growth", 
                       "Fiscal_balance", "Openness", "Real_interest_rate", 
                       "Political_stability", "Export_netto"),
  "Liczba obserwacji" = sapply(data[, c("CBI", "GDP_per_capita", "GDP_per_capita_growth", 
                                        "Fiscal_balance", "Openness", "Real_interest_rate", 
                                        "Political_stability", "Export_netto")], function(x) sum(!is.na(x))),
  "Minimum" = sapply(data[, c("CBI", "GDP_per_capita", "GDP_per_capita_growth", 
                              "Fiscal_balance", "Openness", "Real_interest_rate", 
                              "Political_stability", "Export_netto")], function(x) min(x, na.rm = TRUE)),
  "Maksimum" = sapply(data[, c("CBI", "GDP_per_capita", "GDP_per_capita_growth", 
                               "Fiscal_balance", "Openness", "Real_interest_rate", 
                               "Political_stability", "Export_netto")], function(x) max(x, na.rm = TRUE)),
  "Średnia" = sapply(data[, c("CBI", "GDP_per_capita", "GDP_per_capita_growth", 
                              "Fiscal_balance", "Openness", "Real_interest_rate", 
                              "Political_stability", "Export_netto")], function(x) mean(x, na.rm = TRUE))
)

ft_independent <- flextable(data_summary_independent)
ft_independent <- set_caption(ft_independent, "Tabela 2. Statystyki opisowe zmiennych objaśniających")

# Zapis do pliku Word
doc <- read_docx()
doc <- body_add_par(doc, "Statystyki opisowe", style = "heading 1")
doc <- body_add_flextable(doc, ft_dependent)
doc <- body_add_par(doc, "") # Pusta linia
doc <- body_add_flextable(doc, ft_independent)
print(doc, target = "Statystyki_opisowe.docx")

# Eksport tabel do konsoli (opcjonalnie)
ft_dependent
ft_independent


#Budowanie wstępnego modelu
model_1 <- lm(log_CPI ~ CBI + GDP_per_capita + GDP_per_capita_growth + 
                   Fiscal_balance + Openness + Real_interest_rate + 
                   Political_stability + Export_netto, data = data)
summary(model_1)

# Instalacja i załadowanie wymaganych pakietów
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

if (!require(car)) install.packages("car")
library(car)

# Diagnostyka modelu 1

# Test liniowości i wykresy
# Ramsey RESET test
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)
reset_result <- resettest(model_1)
print("Wynik testu RESET:")
print(reset_result)

# Partial residual plots
if (!require(car)) install.packages("car")
library(car)
crPlots(model_1)


# Normalność reszt
plot(model_1, which = 2) # Q-Q plot
shapiro.test(resid(model_base))

# Homoskedastyczność
plot(model_1, which = 1) # Residuals vs Fitted
bptest(model_1)

# Autokorelacja reszt
dwtest(model_1)

# Współliniowość
vif_values <- vif(model_1)
print(vif_values)



# Model_2
# Dodanie logarytmu GDP_per_capita do danych
data$log_GDP_per_capita <- log(data$GDP_per_capita)


model_2 <- lm(log_CPI ~  log_GDP_per_capita + Political_stability +
                          GDP_per_capita_growth + log_GDP_per_capita:Political_stability, data = data)

summary(model_2)

# Diagnostyka Modelu
# Liniowość
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)
resettest(model_2)

# Normalność reszt
shapiro.test(resid(model_2))

# Homoskedastyczność
bptest(model_2)

# Autokorelacja reszt
dwtest(model_2)

# Współliniowość
if (!require(car)) install.packages("car")
library(car)
vif(model_2)

# generowanie wykresu relacji między zmiennymi
library(ggplot2)

ggplot(data, aes(x = log_GDP_per_capita, y = log_CPI, color = factor(Political_stability > 0))) +
  geom_point() +
  geom_smooth(method = "lm", aes(group = factor(Political_stability > 0)), se = FALSE) +
  labs(
    title = "Relacja między log_GDP_per_capita a log_CPI w zależności od Political_stability",
    x = "Log GDP per capita",
    y = "Log CPI",
    color = "Stabilność polityczna"
  ) +
  theme_minimal()


vif_values_2 <- vif(model_2)
print(vif_values_2)


# Ostateczny model
model_3 <- lm(log_CPI ~ log_GDP_per_capita + Political_stability, data = data)
summary(model_3)

# Diagnostyka Model_3

# Liniowość za pomocą RESET test
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Test liniowości funkcji formy
resettest(model_3)

################ Test Chowa

# Zakładamy, że dane są w ramce danych "data" i zmienne są w modelu_3
# Podział danych na dwie grupy (np. na pół według mediany log_GDP_per_capita)
median_value <- median(data$log_GDP_per_capita)
group1 <- data[data$log_GDP_per_capita <= median_value, ]
group2 <- data[data$log_GDP_per_capita > median_value, ]

# Oszacowanie modelu pełnego
model_full <- lm(log_CPI ~ log_GDP_per_capita + Political_stability, data = data)

# Oszacowanie modeli dla każdej grupy
model_group1 <- lm(log_CPI ~ log_GDP_per_capita + Political_stability, data = group1)
model_group2 <- lm(log_CPI ~ log_GDP_per_capita + Political_stability, data = group2)

# Wyliczenie sum kwadratów reszt
RSS_full <- sum(residuals(model_full)^2)
RSS_group1 <- sum(residuals(model_group1)^2)
RSS_group2 <- sum(residuals(model_group2)^2)

# Liczba obserwacji i parametrów
n <- nrow(data)  # Liczba obserwacji w pełnym zbiorze danych
k <- length(coef(model_full))  # Liczba parametrów (łącznie z wyrazem wolnym)
n1 <- nrow(group1)  # Liczba obserwacji w grupie 1
n2 <- nrow(group2)  # Liczba obserwacji w grupie 2

# Statystyka testowa Chowa
F_stat <- ((RSS_full - (RSS_group1 + RSS_group2)) / k) / ((RSS_group1 + RSS_group2) / (n1 + n2 - 2 * k))

# Wartość p-value
p_value <- pf(F_stat, df1 = k, df2 = (n1 + n2 - 2 * k), lower.tail = FALSE)

# Wynik testu
cat("Test Chowa\n")
cat("Statystyka testowa F:", F_stat, "\n")
cat("p-value:", p_value, "\n")


#################################
# Test normalności reszt
shapiro.test(resid(model_3))

# Test homoskedastyczności
bptest(model_3)

# Sprawdzenie współliniowości
vif_values_3 <- vif(model_3)
print(vif_values_3)

# Wykresy diagnostyczne
plot(model_3, which = 1) # Residuals vs Fitted
plot(model_3, which = 2) # Q-Q plot

# Histogram reszt z naniesioną gęstością normalną
residuals <- resid(model_3)
hist(residuals, breaks = 20, freq = FALSE, main = "Histogram reszt modelu 3", 
     xlab = "Reszty", ylab = "Gęstość")
lines(density(residuals), col = "blue", lwd = 2)
curve(dnorm(x, mean = mean(residuals), sd = sd(residuals)), 
      col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Gęstość reszt", "Rozkład normalny"), 
       col = c("blue", "red"), lwd = 2)


# Macierz odporna (HC1)
if (!require(sandwich)) install.packages("sandwich")
if (!require(lmtest)) install.packages("lmtest")
library(sandwich)
library(lmtest)

# Zastosowanie macierzy odpornej
robust_model <- coeftest(model_3, vcov = vcovHC(model_3, type = "HC1"))
print("Wyniki z zastosowaniem macierzy odpornej:")
print(robust_model)

# Porównanie standardowych błędów
standard_errors_classic <- summary(model_3)$coefficients[, 2]
standard_errors_robust <- sqrt(diag(vcovHC(model_3, type = "HC1")))
comparison_se <- data.frame(
  Classic_SE = standard_errors_classic,
  Robust_SE = standard_errors_robust
)
print("Porównanie klasycznych i odpornych błędów standardowych:")
print(comparison_se)

# Wyciągnięcie p-wartości dla porównania
p_values_robust <- robust_model[, 4]
print("P-wartości z macierzy odpornej:")
print(p_values_robust)




# Instalacja i załadowanie pakietu stargazer (jeśli jeszcze nie masz zainstalowanego)
if (!require(stargazer)) install.packages("stargazer")
library(stargazer)

model_1 <- lm(log_CPI ~ CBI + GDP_per_capita + GDP_per_capita_growth + 
                Fiscal_balance + Openness + Real_interest_rate + 
                Political_stability + Export_netto, data = data)

model_2 <- lm(log_CPI ~  log_GDP_per_capita + Political_stability +
                GDP_per_capita_growth + log_GDP_per_capita:Political_stability, data = data)

model_3 <- lm(log_CPI ~ log_GDP_per_capita + Political_stability, data = data)

summary(model_1)
summary(model_2)
summary(model_3)
# Generowanie tabeli w formacie LaTeX
stargazer(
  model_1, model_2, model_3,
  type = "latex",
  title = "Wyniki regresji liniowej dla modeli",
  column.labels = c("Model 1", "Model 2", "Model 3"),
  dep.var.labels = "Log(CPI)",
  covariate.labels = c(
    "CBI", "GDP per capita", "GDP per capita growth",
    "Fiscal balance", "Openness", "Real interest rate",
    "Political stability", "Export netto",
    "Log(GDP per capita)", "Log(GDP per capita):Political stability",
    "Constant"
  ),
  add.lines = list(
    c("Macierz odporna", "", "", "Tak (HC1)")
  ),
  out = "wyniki_regresji.tex"
)

# Analiza obserwacji nietypowych 

plot(model_3, which = 4)  # Cook distance 

# Oblicz próg odległości Cooka
prog <- 4 / 101

# Wyszukaj obserwacje, które przekraczają próg
cooks_distances <- cooks.distance(model_3)
outliers <- which(cooks_distances > prog)

prog <- 4 / 101
cooks_distances <- cooks.distance(model_3)
outliers <- which(cooks_distances > prog)
length(outliers)  # Liczba obserwacji powyżej progu




# Instalacja i załadowanie pakietu car (jeśli jeszcze go nie masz)
if (!require(car)) install.packages("car")
library(car)

# Stworzenie wykresu dźwigni i reszt z progiem Cooka
influencePlot(model_3,
              main = "",
              cex.sub = 0.8)   # Zmniejsz czcionkę podtytułu



# Wyświetlenie podejrzanych obserwacji
suspected_outliers <- c(48, 91, 88)  # Numery obserwacji z wykresu
data[suspected_outliers, ]  # Wyświetlenie szczegółów


############################################################

# Obliczenie współczynników VIF
vif_values <- vif(model_3)

# Stworzenie tabeli z wynikami VIF
vif_table <- data.frame(
  "Nazwa zmiennej" = names(vif_values),
  "Współczynnik VIF" = round(vif_values, 3)
)

# Wyświetlenie tabeli w konsoli
print(vif_table)







