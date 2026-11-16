# WEBSCRAPING AUTOSCOUT24

# 1. PAKKER ====================================================================
library(chromote)
library(rvest)
library(dplyr)
library(stringr)
library(logr)


# 2. START LOG OG BROWSER ======================================================

# Starter en log, der undervejs kan gemme beskeder om, hvad koden laver
log_open(file_name = "carlog_de") #Hvad er forkskellen på logning og saveRDS?????

# Vælg, at browseren skal være synlig # kør dette seperat, så chrome har tid til at åbne vinduet
options(chromote.headless = FALSE)

# Opret en browsersession
browsersession_de <- ChromoteSession$new()
browsersession_de$view()

# 3. KLARGØR URL ===============================================================

# Find url vi kan bruge til at scrape data med
# Vi begrænser os til en søgning der giver i alt 2.649 resultater / 89 pages pr. 11/11-26
# 1. Personbil
# 2. Mercedes
# 3. Benzin, Diesel eller el
# 4. Forhandler
Total_url_de =  "https://www.autoscout24.de/lst/mercedes-benz/ot_jahreswagen?fuel=D%2CE%2CB&fregfrom=2000&custtype=D&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page=2&atype=C&search_id=5a9vxk6sit&source=listpage_pagination"
Baseurl_de = "https://www.autoscout24.de/lst/mercedes-benz/ot_jahreswagen?fuel=D%2CE%2CB&fregfrom=2000&custtype=D&cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&page="
endpoint_de <- "&atype=C&search_id=5a9vxk6sit&source=listpage_pagination"

# 4. FIND HTML MED 1 BIL  ======================================================

# Åbner url i browser
browsersession_de$Page$navigate(Total_url_de)
browsersession_de$view()

# Hent html ind i R
html_de <- browsersession_de$Runtime$evaluate("document.documentElement.outerHTML")$result$value #noget javascript halløj det behøver du ikke forstå
page_de <- read_html(html_de)

# Find den del af html der indeholder én bilannonce
# Se screenshot Èn_bil_'article'

# Henter data for en bil 
testbil_de <- html_elements(page_de, "article[data-testid='list-item']")[[7]] 

# SE SCREENSHOTS for html_elements herunder

#pris
pris_element_de <- html_element(testbil_de, "span[data-testid='regular-price']")
pris_text_de <- html_text(pris_element_de)
pris_de <- as.numeric(str_remove_all(pris_text, "[^0-9]"))

#model
model_element_de <- html_element(testbil_de, "span[class*='ListItemTitle_title']")
model_de <- html_text2(model_element_de) #html_text2 fordi html_text ikke kan aflæse mellemrummet mellem tallene

#Bil_id 
Bil_id_de <- html_attr(testbil_de, "id")
Bil_id_de

#link #chatgpt har hjulpet for det gemmer sig når man åbner chromote!!!!?????
json_de <- html_text(html_element(page_de, "script#__NEXT_DATA__"))
annoncer_de <- jsonlite::fromJSON(json_de)$props$pageProps$listings

link_de <- annoncer_de$url[match(Bil_id_de, annoncer_de$id)]
link_de <- xml2::url_absolute(link_de, "https://www.autoscout24.de")
link_de


#Tags står denne gang direkte i artikle så vi kan hente det der 

# Registrings år
registrering_text_de <- html_attr(testbil_de, "data-first-registration")
registrering_text_de
registrerings_år_de <- as.numeric(str_sub(registrering_text_de, start = -4))

# Drivmiddel
drivmiddel_element_de <- html_element(testbil_de, "div[data-testid='VehicleDetails-gas_pump']")
drivmiddel_de <- html_text(drivmiddel_element_de)

# kørte km
km_text_de <- html_attr(testbil_de, "data-mileage")
kørte_km_de <- as.numeric(km_text_de)

#Lokation
lokation_element_de <- html_element(testbil_de, "span[class*='ListItemSeller_address']")
lokation_text_de <- html_text(lokation_element_de)
lokation_de <-  str_remove(lokation_text_de, "^DE-[0-9]{5} +")

#Forhandler id

forhandler_id_de <- html_attr(biler_de[[1]], "data-customer-id")
forhandler_element_de <- html_element(testbil_de, "span[data-testid='dealer-company-name']")
forhandler_navn <- html_text(forhandler_element_de)


# 4. TEST LOOP  ================================================================

json_de <- html_text(html_element(page_de, "script#__NEXT_DATA__"))
annoncer_de <- jsonlite::fromJSON(json_de)$props$pageProps$listings

biltest_de <- data.frame(
  Link = character(),
  Pris = numeric(),
  Forhandler_Id = character(),
  Drivmiddel = character()
)

for (i in 1:2) {
  testbil_de <- biler_de[[i]]
  
  Bil_id_de <- html_attr(testbil_de, "id")
  link_de <- annoncer_de$url[match(Bil_id_de, annoncer_de$id)]
  link_de <- xml2::url_absolute(link_de, "https://www.autoscout24.de")
  
  pris_element_de <- html_element(testbil_de, "span[data-testid='regular-price']")
  pris_text_de <- html_text(pris_element_de)
  pris_de <- as.numeric(str_remove_all(pris_text_de, "[^0-9]"))
  
  forhandler_id_de <- html_attr(testbil_de, "data-customer-id")
  
  drivmiddel_element_de <- html_element(
    testbil_de, "div[data-testid='VehicleDetails-gas_pump']"
  )
  drivmiddel_de <- html_text(drivmiddel_element_de)
  
  bilraekke_de <- data.frame(
    Link = link_de,
    Pris = pris_de,
    Forhandler_Id = forhandler_id_de,
    Drivmiddel = drivmiddel_de
  )
  
  biltest_de <- rbind(biltest_de, bilraekke_de)
}

View(biltest_de)

# 5. HENT BILER FRA FLERE SØGERESULTATSIDER ====================================

AutoScout24df <- data.frame(
  Bil_id = character(),
  Bilmodel = character(),
  Pris = numeric(),
  Registrerings_år = numeric(),
  Kørte_km = numeric(),
  Drivmiddel = character(),
  Forhandler_id = character(),
  Lokation = character(),
  Link = character()
)

log_open(file_name = "carlog_de")

# Opret en ny Chromote-session
browsersession_de <- ChromoteSession$new()
browsersession_de$view()

# Start med to sider. Loggen er allerede åbnet i sektion 2.
limit <- 200

for (i in seq.int(from = 144, to = limit)) {
  
  # 5.1 Åbn siden, og hent annoncekort og linkdata -------------------------------
  
  side_url_de <- paste0(Baseurl_de, i, endpoint_de)
  log_print(paste("Henter side", i, side_url_de))
  
  browsersession_de$Page$navigate(side_url_de)
  Sys.sleep(4)
  
  html_de <- browsersession_de$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  page_de <- read_html(html_de)
  biler_de <- html_elements(page_de, "article[data-testid='list-item']")
  
  if (length(biler_de) == 0) {
    log_print(paste("Ingen annoncekort fundet på side", i))
    break
  }
  
  # JSON indeholder links til annoncerne på den aktuelle side
  json_de <- html_text(html_element(page_de, "script#__NEXT_DATA__"))
  annoncer_de <- jsonlite::fromJSON(json_de)$props$pageProps$listings
  
  # 5.2 Gennemgå annoncerne én ad gangen ---------------------------------------
  
  for (j in seq_along(biler_de)) {
    tryCatch({
      
      testbil_de <- biler_de[[j]]
      
      # Annonce-ID bruges til at finde den rigtige URL i sidens JSON
      Bil_id_de <- html_attr(testbil_de, "id")
      link_de <- annoncer_de$url[match(Bil_id_de, annoncer_de$id)]
      link_de <- xml2::url_absolute(link_de, "https://www.autoscout24.de")
      
      pris_element_de <- html_element(testbil_de, "span[data-testid='regular-price']")
      pris_text_de <- html_text(pris_element_de)
      pris_de <- as.numeric(str_remove_all(pris_text_de, "[^0-9]"))
      
      model_element_de <- html_element(testbil_de, "span[class*='ListItemTitle_title']")
      model_de <- str_squish(html_text2(model_element_de))
      
      registrering_text_de <- html_attr(testbil_de, "data-first-registration")
      registrerings_år_de <- as.numeric(str_sub(registrering_text_de, start = -4))
      
      km_text_de <- html_attr(testbil_de, "data-mileage")
      kørte_km_de <- as.numeric(km_text_de)
      
      drivmiddel_element_de <- html_element(
        testbil_de, "div[data-testid='VehicleDetails-gas_pump']"
      )
      drivmiddel_de <- str_squish(html_text(drivmiddel_element_de))
      
      forhandler_id_de <- html_attr(testbil_de, "data-customer-id")
      
      lokation_element_de <- html_element(testbil_de, "span[class*='ListItemSeller_address']")
      lokation_text_de <- str_squish(html_text(lokation_element_de))
      lokation_de <- str_remove(lokation_text_de, "^DE-[0-9]{5} +")
      
      # 5.3 Saml oplysningerne i én række ---------------------------------------
      
      bilraekke_de <- data.frame(
        Bil_id = Bil_id_de,
        Bilmodel = model_de,
        Pris = pris_de,
        Registrerings_år = registrerings_år_de,
        Kørte_km = kørte_km_de,
        Drivmiddel = drivmiddel_de,
        Forhandler_id = forhandler_id_de,
        Lokation = lokation_de,
        Link = link_de
      )
      
      AutoScout24df <- rbind(AutoScout24df, bilraekke_de)
      
    }, error = function(e) {
      log_print(paste(
        "Fejl på side", i, "ved bilkort", j, conditionMessage(e)
      ))
    })
  }
  
  saveRDS(AutoScout24df, "AutoScout24df_mercedes.rds")
}

View(AutoScout24df)

# 6. TJEK RESULTATET ===========================================================

nrow(AutoScout24df) #4000

length(unique(AutoScout24df$Bil_id)) #3946

# Chatgpt hjælper:
dublet_id_de <- AutoScout24df |>
  count(Bil_id, name = "Antal", sort = TRUE) |>
  filter(Antal >= 2)

View(dublet_id_de)

# Der er nu 54 dublikerede annonce rækker alle er der 2 gange. 
# AutoScout har sponsorerede annoncer og vi antager at det er dem der popper frem flere gange
# Det er svært at tjekke da vi ikke specifikt kan lede efter annoncer på hjemmesiden

# 7. RYDDER OP I DATA // DATA CLEANSING VENT MED DETTE TIL SENERE ======================================== 

AutoScout24df_clean <- AutoScout24df[!duplicated(AutoScout24df$Bil_id), ]
nrow(AutoScout24df_clean)





