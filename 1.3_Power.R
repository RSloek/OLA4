# POWER TILBUD – WEBSCRAPING ===================================================

# 1. PAKKER ===================================================================
library(chromote)
library(rvest)
library(stringr)
library(logr)

# 2. START LOG OG BROWSER =====================================================
# Loggen gemmer beskeder om kørslen. saveRDS() gemmer selve dataframen.
log_open(file_name = "power_log")

options(chromote.headless = FALSE)
browsersession_power <- ChromoteSession$new()

# 3. KLARGØR URL ==============================================================
Power_url <- "https://www.power.dk/kampagne/aktuelt/"

# 4. ÅBN KAMPAGNESIDEN ========================================================
browsersession_power$Page$navigate(Power_url)
browsersession_power$view()

# 5. TEST MED ÉT PRODUKTKORT ================================================
# Kør dette afsnit, når de første produktkort er synlige i Chromote.

html_power <- browsersession_power$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

page_power <- read_html(html_power)

# Find de indlæste kort, og vælg det første som test
testprodukt_power <- html_elements(
  page_power,
  "pwr-product-carousel pwr-product-item-wrapper article.product-item"
)[[1]]

# Produktnavn
navn_element_power <- html_element(
  testprodukt_power,
  ".product-item-title-container a.stretched-link"
)
produkt_navn_power <- html_text(navn_element_power, trim = TRUE)
produkt_navn_power

# Pris
pris_element_power <- html_element(testprodukt_power, "data.main-price")
pris_text_power <- html_attr(pris_element_power, "value")
pris_power <- as.numeric(pris_text_power)
pris_power

# Besparelse
spar_element_power <- html_element(testprodukt_power, "div.savings-label")
spar_text_power <- html_text(spar_element_power, trim = TRUE)
spar_power <- as.numeric(str_remove_all(spar_text_power, "[^0-9]"))
spar_power

# 6. FIND PRODUKTERNES WRAPPERE ===============================================
wrapper_selector <- "pwr-product-carousel pwr-product-item-wrapper"

# Tæl alle wrappere i karrusellerne, også dem uden indlæste produktdata
antal_produkter <- browsersession_power$Runtime$evaluate(
  sprintf("document.querySelectorAll('%s').length", wrapper_selector)
)$result$value

if (antal_produkter == 0) {
  stop("Ingen produktkort fundet. Vent til kampagnesiden er synlig, og kør fra afsnit 5 igen.")
}

# Én plads til resultatet fra hver wrapper
resultater_power <- vector("list", antal_produkter)

# 7. RUL TIL HVERT PRODUKTKORT OG HENT DATA ====================================
for (i in seq_len(antal_produkter)) {
  # JavaScript tæller fra 0, så wrapper nr. 1 i R har indeks 0 i browseren
  wrapper_js <- sprintf(
    "document.querySelectorAll('%s')[%d]",
    wrapper_selector,
    i - 1L
  )
  
  browsersession_power$Runtime$evaluate(
    paste0(wrapper_js, ".scrollIntoView({block: 'center', inline: 'center'})")
  )
  
  # Giv kortet op til ca. 3 sekunder til at blive indlæst
  kort_html <- ""
  for (forsog in seq_len(12)) {
    kort_html <- browsersession_power$Runtime$evaluate(
      paste0(wrapper_js, "?.querySelector('article.product-item')?.outerHTML ?? ''")
    )$result$value
    
    if (nzchar(kort_html)) break
    Sys.sleep(0.25)
  }
  
  if (!nzchar(kort_html)) {
    log_print(paste("Kort", i, "blev ikke indlæst"))
    next
  }
  
  produkt_power <- read_html(kort_html)
  
  navn_element <- html_element(
    produkt_power,
    ".product-item-title-container a.stretched-link"
  )
  pris_element <- html_element(produkt_power, "data.main-price")
  spar_element <- html_element(produkt_power, "div.savings-label")
  spar_text <- html_text(spar_element, trim = TRUE)
  
  resultater_power[[i]] <- data.frame(
    Produktnavn = html_text(navn_element, trim = TRUE),
    Pris = as.numeric(html_attr(pris_element, "value")),
    Besparelse = as.numeric(str_remove_all(spar_text, "[^0-9]"))
  )
}

# 8. SAML OG TJEK RESULTATET ==================================================
Powerdf <- dplyr::bind_rows(resultater_power)

# Antal unikke produktnavne
length(unique(Powerdf$Produktnavn))

# Produktnavne, der optræder mindst to gange
dublet_navn_power <- Powerdf |>
  dplyr::count(Produktnavn, name = "Antal", sort = TRUE) |>
  dplyr::filter(Antal >= 2)

View(dublet_navn_power)

View(dublet_id_de)

# der er to dubletter men det er fordi de samme tilbud faktisk ligger der to gange

cat("Kort fundet:", nrow(Powerdf), "ud af", antal_produkter, "wrappere\n")
View(Powerdf)

log_close()