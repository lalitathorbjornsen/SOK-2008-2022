# Innlevering 5
library(readr)
library(tidyverse)
library(ggplot2)

# Lage datasett fra csv
landbakgrunn_for_innvand <- read_delim("landbakgrunn-for-innvand.csv", delim = ";", 
                                       escape_double = FALSE, trim_ws = TRUE, show_col_types = FALSE)
view(landbakgrunn_for_innvand)
 
landbakgrunn_for_innvand <- landbakgrunn_for_innvand %>%
  pivot_longer(!År, names_to = "Verdensdeler", values_to = "value")

view(landbakgrunn_for_innvand)

# Grafen
plot <- landbakgrunn_for_innvand %>%
  filter(År >= 2005) %>%
  ggplot(., aes( x =År,  y = value, color = Verdensdeler)) + 
  geom_line() + geom_point() + theme_bw() + 
  scale_x_continuous(breaks=c(2005:2022)) + 
  ggtitle("Landbakgrunn for innvandrere") +
  xlab("År") + ylab("Antall mennesker")
plot

# Utfordring 5.1.2

library(rjstat)
library(tidyverse)
library(httr)
library(PxWebApiData)
library(ggplot2)
library(dplyr)  
library(ggrepel)
library(zoo)
library(ggthemes)

# options(encoding="UTF-8")

url <- "https://data.ssb.no/api/v0/no/table/13215/"

data <- ' {"query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "015a"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "agg:NACE260InnvGrupp2",
        "values": [
          "SNI-01-03",
          "SNI-05-09",
          "SNI-10-33",
          "SNI-35-39",
          "SNI-41-43",
          "SNI-45-47",
          "SNI-49-53",
          "SNI-49.3",
          "SNI-55",
          "SNI-56",
          "SNI-58-63",
          "SNI-64-66",
          "SNI-68-75",
          "SNI-77-82",
          "SNI-78.2",
          "SNI-81.2",
          "SNI-84",
          "SNI-85",
          "SNI-86-88",
          "SNI-90-99",
          "SNI-00"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'


data <- POST(url , body = data, encode = "json", verbose())
data <- fromJSONstat(content(data, "text"))

names(data)[names(data) == "næring (SN2007)"] <- "Næring"

view(data)

invandring2021 <-
  ggplot(data, aes(x= Næring, y=value, fill = Næring)) + 
  geom_bar(stat = "identity", position = "dodge2" , width = 0.5) + theme_bw() +
  ggtitle("Innvandring fra Øst-Europa i 2021") +
  xlab("Næringssektor") + ylab("Antall mennesker") + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) 

invandring2021


