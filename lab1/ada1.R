library(ggplot2)
library(dplyr)
library(carData)
data("CES11")
dane <- CES11 
# 1. Tabela dla edukacji (
dane_edu <- group_by(CES11, education)
tabela_edu <- summarise(dane_edu, 
                        licznosc = n(), 
                        czestosc = n() / nrow(CES11))
tabela_edu <- mutate(tabela_edu, procent = paste0(round(czestosc * 100, 2), "%"))
tabela_edu

# 2. Tabela dla aborcji 
dane_aborcja <- group_by(CES11, abortion)
tabela_aborcja <- summarise(dane_aborcja, 
                            licznosc = n(), 
                            czestosc = n() / nrow(CES11), 
                            procent = paste0(round(czestosc * 100, 2), "%")) 
tabela_aborcja 

View(dane_aborcja)
str(dane_aborcja)
dane_aborcja[rowSums(is.na(dane_aborcja)) > 0, ]

dane_aborcja$id<-NULL

View(dane_aborcja)

# 3. Wykres słupkowy aborcja
wykres_slupkowy_aborcja <- ggplot(tabela_aborcja, aes(x = abortion, y = licznosc, fill = abortion)) + 
  geom_col() + 
  labs(
    title = "Wykres słupkowy dla zmiennej 'abortion'",
    x = "Czy aborcja powinna być zakazana?",
    y = "Liczność odpowiedzi"
  ) +
  theme_minimal()

wykres_slupkowy_aborcja


# 4. Wykres słupkowy edukacja
wykres_slupkowy_edukacja <- ggplot(tabela_edu, aes(x = education, y = licznosc, fill = education)) + 
  geom_col() + 
  labs(
    title = "Wykres słupkowy dla zmiennej 'education'",
    x = "Jaki poziom wykształcenia mają ankietowani",
    y = "Liczność odpowiedzi"
  ) +
  theme_minimal()

wykres_slupkowy_edukacja


#5. wykres mozaikowy 
library(ggmosaic2)
wykres_mozaikowy <- ggplot(data= CES11) +
  geom_mosaic(aes(x = product(abortion, education), fill = abortion)) +
  labs(
    title = "Wykres mozaikowy: Edukacja vs Aborcja",
    x = "Poziom wykształcenia",
    y = "Stosunek do aborcji",
    fill = "Odpowiedź"
  ) +
  theme_minimal() 
  
wykres_mozaikowy


# Liczba sukcesów (x) oraz wielkość próby (n)
x <- sum(dane$abortion == "Yes")
n <- nrow(dane)

# 1. Przedział Wilsona (Score Interval) – BEZ poprawki na ciągłość
przedzial_wilson <- prop.test(x, n, conf.level = 0.95, correct = FALSE)
przedzial_wilson$conf.int

# 2. Alternatywnie: Dokładny przedział Cloppera-Pearsona
przedzial_exact <- binom.test(x, n, conf.level = 0.95)
przedzial_exact$conf.int

# Podsumowanie estymacji punktowej i przedziałowej
p_hat <- x / n
cat("Estymator punktowy p_hat:", round(p_hat, 4), "\n")
cat("Przedział ufności Wilsona 95%:", round(przedzial_wilson$conf.int[1], 4), "-", round(przedzial_wilson$conf.int[2], 4), "\n")


















