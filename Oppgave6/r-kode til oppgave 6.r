 Oppgave 6

# pakkene
library(tidyverse)
library(rjstat)
library(tidyverse)
library(httr)
library(PxWebApiData)
library(ggplot2)
library(dplyr)
library(patchwork)


# data for sykefraværet
url1 <- "https://data.ssb.no/api/v0/no/table/12441/"

data1 <- '{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
data1 <- POST(url1 , body = data1, encode = "json", verbose())
data1 <- fromJSONstat(content(data1, "text"))


view(data1)


#A data for arbeidsledighet

url2 <- "https://data.ssb.no/api/v0/no/table/05111/"
data2 <- '{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
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
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
data2 <- POST(url2 , body = data2, encode = "json", verbose())
data2 <- fromJSONstat(content(data2, "text"))

view(data2)

# lage et nytt datasett 
datasett <- merge(data1,data2, by = c("år", "kjønn"))
# for menn
data_menn <- datasett %>% filter(kjønn == "Menn") 
#for kvinner
data_kvinner <- datasett %>% filter(kjønn == "Kvinner")

data_menn$år <- as.numeric(as.character(data_menn$år))
data_kvinner$år <- as.numeric(as.character(data_kvinner$år))


#kvinner

# koeffisient 
coeffk = 2

kvinner <- data_kvinner%>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "blue") +
  geom_line(aes(y = value.x/coeffk), color = "red") +
  scale_y_continuous("Arbeidsledighet i %", sec.axis = sec_axis(~.*coeffk, name = "Sykefravær i %")) + 
  scale_x_continuous("År", breaks = 2005:2019) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  theme(axis.line.y.left = element_line(color = "blue"), 
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue")) +
  ggtitle("Arbeidsledighet og sykefravær \nfor kvinner") + 
  theme(axis.text.x=element_text(angle=90, hjust=1)) 
kvinner

# koeffisient 
coff <- 1.5

# Menn
menn <- data_menn %>% 
  ggplot(aes(x=år, y=value.y)) +
  geom_line(color = "blue") +
  geom_line(aes(y = value.x/coff), color = "red") +
  scale_y_continuous("Arbeidsledighet i %", sec.axis = sec_axis(~.*coff, name = "Sykefravær i %")) + 
  scale_x_continuous("År", breaks = 2005:2019) +
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")) +
  theme(axis.line.y.left = element_line(color = "blue"), 
        axis.ticks.y.left = element_line(color = "blue"),
        axis.text.y.left = element_text(color = "blue"), 
        axis.title.y.left = element_text(color = "blue")) +
  ggtitle("Arbeidsledighet og sykefravær \nfor menn") + 
  theme(axis.text.x=element_text(angle=90, hjust=1))
menn



# Plotte de sammen
kvinner + theme_classic() + menn + theme_classic()

