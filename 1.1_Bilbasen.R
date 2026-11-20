# NYT SØGEFILTER UNDER 100 SIDER

# 1. PAKKER ====================================================================
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(logr)


# 2. START LOG OG BROWSER ======================================================

# Starter en log, der undervejs kan gemme beskeder om, hvad koden laver
log_open(file_name = "carlog") #Hvad er forkskellen på logning og saveRDS?????

# Vælg, at browseren skal være synlig
options(chromote.headless = FALSE)

# Opret en browsersession
browsersession <- ChromoteSession$new()
browsersession$view()

# 3. KLARGØR URL ===============================================================

# Find url vi kan bruge til at scrape data med
# Vi begrænser os til en søgning der giver i alt 2.649 resultater / 89 pages pr. 11/11-26
# 1. Personbil
# 2. Mercedes
# 3. Benzin, Diesel eller el
# 4. Forhandler
Total_url =  "https://www.bilbasen.dk/brugt/bil/mercedes?fuel=3&fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&page=1&sellertypes=dealer"
Baseurl = "https://www.bilbasen.dk/brugt/bil/mercedes?fuel=3&fuel=1&fuel=2&includeengroscvr=true&includeleasing=false&page="
endpoint <- "&sellertypes=dealer"

# 4. FIND HTML MED 1 BIL  ======================================================

# Åbner url i browser
browsersession$Page$navigate(Total_url)
browsersession$view()

# Hent html ind i R
html <- browsersession$Runtime$evaluate("document.documentElement.outerHTML")$result$value #noget javascript halløj det behøver du ikke forstå
page <- read_html(html)

# Find den del af html der indeholder én bilannonce
# Se screenshot Èn_bil_'article'

# Henter data for en bil 
testbil <- html_elements(page, "article[data-e2e='search-result-card']")[[2]] #alternativt testbil <- page |> html_element("article[data-e2e='search-result-card']")

# SE SCREENSHOTS for html_elements herunder

#pris
pris_element <- html_element(testbil, "[class*='Listing_price']")
pris_text <- html_text(pris_element)
pris <- as.numeric(str_remove_all(pris_text, "[^0-9]"))

#model
model_element <- html_element(testbil, "[class*='Listing_makeModel']")
model_text <-html_text2(model_element) #html_text2 fordi html_text ikke kan aflæse mellemrummet mellem tallene
model <- str_replace(model_text, "\n", " ")

#link
link_element <- html_element(testbil, "a[class*='Listing_link']")
link <- html_attr(link_element, "href")

#Bil_id
Bil_id <- str_extract(link, "[0-9]+$") 
Bil_id

#Tags
# Find bilens detaljeliste, og hent teksten fra hvert listepunkt
tags_element <- html_elements(testbil, "ul[class*='ListingDetails_list__'] li")
tags_text <- str_squish(html_text(tags_element))

# Find oplysningerne ud fra deres tekst og format
registrering_text <- str_subset(tags_text, "^[0-9]{1,2}/[0-9]{4}$")[1]
km_text <- str_subset(tags_text, "^[0-9.]+ km$")[1]
rækkevidde_text <- str_subset(tags_text, "rækkevidde")[1]
drivmiddel <- str_subset(tags_text, "^(Benzin|Diesel|El)$")[1]

# Træk året ud, og gør alle tre værdier til tal
registrerings_år <- as.numeric(str_sub(registrering_text, start = -4))
kørte_km <- as.numeric(str_remove_all(km_text, "[^0-9]"))
rækkevidde <- as.numeric(str_remove_all(rækkevidde_text, "[^0-9]"))

#Lokation
lokation_element <- html_element(testbil, "[class*='Listing_location']")
lokation_text <- html_text(lokation_element)
lokation_text

#Forhandler id

# Jeg har manuelt undersøgt at stykket 08ede644-57a6-41ca-abcd-e8cd017ca2f5 i logobilledfilen
# https://billeder.bilbasen.dk/bilinfo/08ede644-57a6-41ca-abcd-e8cd017ca2f5.jpeg?class=S400X400
# er forhandler_id der differentier mellem forskellige lokationer for samme forhandler
# fx er dette for SEMLER Mobility i Brøndby

logo_element <- html_element(testbil, "img[class*='Listing_dealerLogoCard']")
logo_url <- html_attr(logo_element, "src")
forhandler_id <- str_extract(logo_url, "[0-9a-f-]{36}")

# 4. TEST LOOP  ================================================================

biler <- html_elements(page, "article[data-e2e='search-result-card']")
biler[[2]]

#lille test loop
biltest <- data.frame(Link = character(), Pris = numeric(), Forhandler_Id = character(), Drivmiddel = character())

for (i in 1:2) {
  testbil <- biler[[i]]
  
  link_element <- html_element(testbil, "a[class*='Listing_link']")
  link <- html_attr(link_element, "href")
  
  pris_element <- html_element(testbil, "[class*='Listing_price']")
  pris_text <- html_text(pris_element)
  pris <- as.numeric(str_remove_all(pris_text, "[^0-9]"))
  
  logo_element <- html_element(testbil, "img[class*='Listing_dealerLogoCard']")
  logo_url <- html_attr(logo_element, "src")
  forhandler_id <- str_extract(logo_url, "[0-9a-f-]{36}")
  
  
  #Tags
  # Find bilens detaljeliste, og hent teksten fra hvert listepunkt
  tags_element <- html_elements(testbil, "ul[class*='ListingDetails_list__'] li")
  tags_text <- str_squish(html_text(tags_element))
  
  drivmiddel <- str_subset(tags_text, "^(Benzin|Diesel|El)$")[1]
  
  bilraekke <- data.frame(Link = link, Pris = pris, Forhandler_Id = forhandler_id, Drivmiddel = drivmiddel)
  biltest <- rbind(biltest, bilraekke)
  
}

View(biltest)

# 5. HENT BILER FRA FLERE SØGERESULTATSIDER ====================================

# Opret tom dataframe
Bilbasedf <- data.frame(
  Bil_id = character(),
  Bilmodel = character(),
  Pris = numeric(),
  Registrerings_år = numeric(),
  Kørte_km = numeric(),
  Rækkevidde = numeric(),
  Drivmiddel = character(),
  Forhandler_id = character(),
  Lokation = character(),
  Link = character()
)

# Start med to sider. Hæv limit, når du har tjekket resultatet.
log_open(file_name = "carlog")
log_print("Test: Loggen er åben")

limit <- 89

for (i in 1:limit) {
  
  # 5.1 Åbn søgeresultatsiden, og find dens annoncekort ------------------------
  
  side_url <- paste0(Baseurl, i, endpoint)
  log_print(paste("Henter side", i, side_url))
  
  browsersession$Page$navigate(side_url)
  Sys.sleep(4)
  
  html <- browsersession$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  page <- read_html(html)
  biler <- html_elements(page, "article[data-e2e='search-result-card']")
  
  if (length(biler) == 0) {
    log_print(paste("Ingen annoncekort fundet på side", i))
    break
  }
  
  for (j in seq_along(biler)) {
    tryCatch({
      
      # 5.2 Vælg én bil, og hent link, ID, pris, model, lokation -----------------
      
      testbil <- biler[[j]]
      
      logo_element <- html_element(testbil, "img[class*='Listing_dealerLogoCard']")
      logo_url <- html_attr(logo_element, "src")
      forhandler_id <- str_extract(logo_url, "[0-9a-f-]{36}")
      
      link_element <- html_element(testbil, "a[class*='Listing_link']")
      link <- html_attr(link_element, "href")
      Bil_id <- str_extract(link, "[0-9]+$")
      
      pris_element <- html_element(testbil, "[class*='Listing_price']")
      pris_text <- html_text(pris_element)
      pris <- as.numeric(str_remove_all(pris_text, "[^0-9]"))
      
      model_element <- html_element(testbil, "[class*='Listing_makeModel']")
      model_text <- html_text2(model_element)
      model <- str_replace(model_text, "\n", " ")
      
      lokation_element <- html_element(testbil, "[class*='Listing_location']")
      lokation_text <- html_text2(lokation_element)
      
      # 5.3 Hent registreringsår, kilometerstand, rækkevidde og drivmiddel -----
      
      tags_element <- html_elements(testbil, "ul[class*='ListingDetails_list__'] li")
      tags_text <- str_squish(html_text(tags_element))
      
      registrering_text <- str_subset(tags_text, "^[0-9]{1,2}/[0-9]{4}$")[1]
      km_text <- str_subset(tags_text, "^[0-9.]+ km$")[1]
      rækkevidde_text <- str_subset(tags_text, "rækkevidde")[1]
      drivmiddel <- str_subset(tags_text, "^(Benzin|Diesel|El)$")[1]
      
      registrerings_år <- as.numeric(str_sub(registrering_text, start = -4))
      kørte_km <- as.numeric(str_remove_all(km_text, "[^0-9]"))
      rækkevidde <- as.numeric(str_remove_all(rækkevidde_text, "[^0-9]"))
      
      # 5.4 Saml bilens oplysninger, og tilføj én række til Bilbasedf ------------
      
      bilraekke <- data.frame(
        Bil_id = Bil_id,
        Bilmodel = model,
        Pris = pris,
        Registrerings_år = registrerings_år,
        Kørte_km = kørte_km,
        Rækkevidde = rækkevidde,
        Drivmiddel = drivmiddel,
        Forhandler_id = forhandler_id,
        Lokation = lokation_text,
        Link = link
      )
      
      Bilbasedf <- rbind(Bilbasedf, bilraekke)
      
      #chatgpt hjælp    
    }, error = function(e) {
      log_print(paste(
        "Fejl på side", i, "ved bilkort", j, conditionMessage(e)
      ))
    })
  } # Luk billoopet
  
  saveRDS(Bilbasedf, "Bilbasedf_mercedes.rds")
} # Luk sideloopet


# 6. TJEK RESULTATET ===========================================================

nrow(Bilbasedf) #2655
View(Bilbasedf)

length(unique(Bilbasedf$Bil_id)) #2988
colSums(is.na(Bilbasedf[, c("Forhandler_id", "Lokation", "Bil_id", "Pris", "Registrerings_år", "Bilmodel")]))

# Undersøg hvorfor der kun er 2988 unikke bil_id når der burde være 4080 

# Chatgpt hjælper:
dublet_id <- Bilbasedf |>
  count(Bil_id, name = "Antal", sort = TRUE) |>
  filter(Antal >= 2)

View(dublet_id)

# Der er nu to dublikerede annoncer række 809=811 og række 810=812
# Det tyder på det hele har forskubbet sig pga en ny annonce der blev uploadet mens loopet kørte

sum(Bilbasedf$Registrerings_år < 2000, na.rm = TRUE) #Kun 56 biler er fra før år 2000

# 7. RYDDER OP I DATA ==========================================================

Bilbasedf_clean <- Bilbasedf

Bilbasedf_clean <- Bilbasedf[!duplicated(Bilbasedf$Bil_id), ]
nrow(Bilbasedf_clean)
Bilbasedf_clean$Bilmodel_kort <- sub("^(Mercedes [^ +]+).*", "\\1", Bilbasedf_clean$Bilmodel)
View(Bilbasedf_clean)

saveRDS(Bilbasedf_clean, "Bilbasedf_clean.rds")

sum(is.na(Bilbasedf_clean$Forhandler_id))
length(Bilbasedf_clean$Forhandler_id)




