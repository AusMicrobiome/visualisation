---
title: "R Notebook for plotting some time series data for temperature and nutrients, for example from IMOS NRS time series"
output: html_notebook
---

#### Authors: Martin Ostrowski
#### Date: 20190404
#### email: martin.ostrowski@uts.edu.au

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook that provides the code for presenting boxplots of the monthly ranges in contextual measurements such as Temperature, nutrient concentrations and cell numbers.

![example plot](/nrs_east_temp.png)


The input data is contained in Bacteria.csv and Contextual.csv downloaded from the processed data tab of the Australian Microbiome [data portal](https://data.bioplatforms.com/organization/about/australian-microbiome) 


load the required R libraries
```
library(tidyverse)
library(oce) # color schhemes for oceanography 
library(gridExtra)
library(gtable)
library(grid)
```

Load the required data

```
path="~/Downloads/AustralianMicrobiome-2019-04-10T184531-csv/"
#setwd
contextual<-read_csv("contextual.csv")
#bacteria <- TBD
  
```

Filter to keep only the samples for NRS sites

```{r}

contextual.e <- contextual %>% filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")))

samples<- contextual %>% separate (`Sample ID`, c("num", "code"), sep='/', remove=F)

samples <- samples %>% separate(`Date Sampled`, c("year", "month", "day"), sep='-', remove=F)

NRScols<-read_tsv("~/Dropbox/Rscripts/NRS_cols.txt")
```

Convert NRS station names to factors, import color scheme, convert months into 3 letter abbr.



```{r}
samples$`Nrs Location Code Voyage Code`<-factor(samples$`Nrs Location Code Voyage Code`, levels=c("MAI","KAI","ROT","PHB","NSI","YON","DAR"))

nrsColors <- NRScols$col
names(nrsColors) <- NRScols$site
head(nrsColors)

#add coding to convert Months into 3 letter abbr.

samples$month.abb<-month.abb[as.integer(samples$month)]

samples$month.abb<-as.factor(samples$month.abb)

samples$month.abb<-factor(samples$month.abb, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
```

Plot temperature ranges of surface samples (i.e. less than < 10 m depth)

```{r}

t<-ggplot(samples %>% 
         filter (!is.na(`Temperature Ctd [its-90, deg c]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), #c("MAI","KAI","ROT","PHB","NSI","YON","DAR"))
       aes(x=month.abb, y=`Temperature Ctd [its-90, deg c]`, fill=`Nrs Location Code Voyage Code`)) + 
  #geom_boxplot() +
  #scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Temperature Range (˚C)") +
  geom_jitter(aes(colour = `Temperature Ctd [its-90, deg c]`), 
              alpha=0.7, position=position_jitter(w=0.4,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTemperature(100)) +
   geom_jitter(data=samples %>% 
         filter (!is.na(`Temperature Ctd [its-90, deg c]`)) %>% 
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
         aes(colour = `Depth [m]`), 
              alpha=0.7, position=position_jitter(w=0.4,h=0.2)) + 
  scale_color_gradientn(colors=rev(oceColorsViridis(100))) +theme(axis.text.x=element_text(angle=90, hjust=1))+ theme(legend.position = "right") 

#ggsave(filename = "~/Dropbox/nrs_east_temp.pdf", height=3, width=8)
```
Add a presentation theme

```{r}
theme_mo<-function (base_size = 11, base_family = "") 
{
    blue <- "#2c3e50"
    green <- "#18BC9C"
    white <- "#FFFFFF"
    grey <- "grey80"
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(line = element_line(colour = blue, size = 0.5, 
            linetype = 1, lineend = "butt"), rect = element_rect(fill = white, 
            colour = blue, size = 0.5, linetype = 1), text = element_text(family = base_family, 
            face = "plain", colour = blue, size = base_size, 
            lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, 
            margin = margin(), debug = FALSE), axis.line = element_blank(), 
            axis.text = element_text(size = rel(0.8)), axis.ticks = element_line(color = grey, 
                size = rel(1/3)), axis.title = element_text(size = rel(1)), 
            panel.background = element_rect(fill = white, color = NA), 
            panel.border = element_rect(fill = NA, size = rel(1/2), 
                color = blue), panel.grid.major = element_line(color = grey, 
                size = rel(1/3)), panel.grid.minor = element_line(color = NA, 
                size = rel(1/3)), panel.grid.minor.x = element_blank(), 
            panel.spacing = unit(0.2, "cm"), legend.key = element_rect(fill = white, 
                color = NA), legend.position = "bottom", strip.background = element_rect(fill = blue, 
                color = blue), strip.text = element_text(color = white, 
                size = rel(1.8)), plot.title = element_text(size = rel(1.6), 
                hjust = 0, margin = margin(t = 0, r = 0, b = 4, 
                  l = 0, unit = "pt")), plot.subtitle = element_text(size = rel(1.1), 
                hjust = 0, margin = margin(t = 0, r = 0, b = 3, 
                  l = 0, unit = "pt")), complete = TRUE)
}
```
Plot time series for Nitrate/Nitrite

```{r, fig.height=3.3}

n<-ggplot(samples %>% 
         filter (!is.na(`Nitrate Nitrite [μmol/l]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
       aes(x=month.abb, y=`Nitrate Nitrite [μmol/l]`, fill=`Nrs Location Code Voyage Code`)) + 
  #geom_boxplot() +
  #scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Nitrate/Nitrite Range (µmol/l)") +
  geom_jitter(aes(colour = `Nitrate Nitrite [μmol/l]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTemperature(100)) +
   geom_jitter(data=samples %>% 
         filter (!is.na(`Nitrate Nitrite [μmol/l]`)) %>% 
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
         aes(colour = `Depth [m]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTwo(100)) +theme(axis.text.x=element_text(angle=45, hjust=1))+ theme(legend.position = "none")

```



Plot time series for Phosphate

```{r, fig.height=3.3}

ph<-ggplot(samples %>% 
         filter (!is.na(`Phosphate [μmol/l]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
       aes(x=month.abb, y=`Phosphate [μmol/l]`, fill=`Nrs Location Code Voyage Code`)) + 
  #geom_boxplot() +
  #scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Phosphate Range (µmol/l)") +
  geom_jitter(aes(colour = `Phosphate [μmol/l]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
    scale_color_gradientn(colors=oceColorsTwo(100)) +
   geom_jitter(data=samples %>% 
         filter (!is.na(`Phosphate [μmol/l]`)) %>% 
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
         aes(colour = `Depth [m]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTwo(100)) +theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(legend.position = "none")

```

Arrange T, N and P on a grid for plotting



```{r}
g2 <- ggplotGrob(t)
g3 <- ggplotGrob(n)
g4 <- ggplotGrob(ph)
g <- rbind(g2, g3, g4, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()

grid.draw(g)

```

#####Boxplots only

One strategy is to make 3 depth categories, surface (<= 10 m), middle (between 10,40), lower (<= 50). In these examples we'll plot the surface only.

```{r}
tb<-ggplot(samples %>% 
         filter (!is.na(`Temperature Ctd [its-90, deg c]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), #c("MAI","KAI","ROT","PHB","NSI","YON","DAR"))
       aes(x=month.abb, y=`Temperature Ctd [its-90, deg c]`, fill=`Nrs Location Code Voyage Code`)) + 
  geom_boxplot() +
  scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Temperature Range (˚C)") +
  geom_jitter(aes(colour = `Temperature Ctd [its-90, deg c]`), 
              alpha=0.7, position=position_jitter(w=0.4,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTemperature(100)) +
   theme(axis.text.x=element_text(angle=90, hjust=1))+ theme(legend.position = "none")

tb

ggsave(filename="~/nrs_temp_surf.pdf", height=5, width=12)
```


Boxplots for Nitrate/Nitrite


```{r}
nb<-ggplot(samples %>% 
         filter (!is.na(`Nitrate Nitrite [μmol/l]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
       aes(x=month.abb, y=`Nitrate Nitrite [μmol/l]`, fill=`Nrs Location Code Voyage Code`)) + 
  geom_boxplot() +
  scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Nitrate/Nitrite Range (µmol/l)") +
  geom_jitter(aes(colour = `Nitrate Nitrite [μmol/l]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
  scale_color_gradientn(colors=oceColorsTemperature(100)) +theme(axis.text.x=element_text(angle=45, hjust=1))+ theme(legend.position = "none")

nb

ggsave(filename="~/nrs_no3_surf.pdf", height=5, width=12)
```
Phosphate

```{r}
phb<-ggplot(samples %>% 
         filter (!is.na(`Phosphate [μmol/l]`), `Depth [m]` < 10) %>%
         filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
       aes(x=month.abb, y=`Phosphate [μmol/l]`, fill=`Nrs Location Code Voyage Code`)) + 
  geom_boxplot() +
  scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="Phosphate Range (µmol/l)") +
  geom_jitter(aes(colour = `Phosphate [μmol/l]`), 
              alpha=0.7, position=position_jitter(w=0.2,h=0.2)) + 
    scale_color_gradientn(colors=oceColorsTwo(100)) +
   theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(legend.position = "none") +ylim(0,0.8)

phb +ylim(0,0.8)

ggsave(filename="~/nrs_po4_surf.pdf", height=5, width=12)
```
plot the N:P ratio, (needs work)
```{r}
npr<-ggplot(samples %>% 
         filter (!is.na(`Phosphate [μmol/l]`), !is.na(`Nitrate Nitrite [μmol/l]`), `Depth [m]` < 40, `Nitrate Nitrite [μmol/l]` != 0, `Phosphate [μmol/l]` !=0),
         #filter (`Nrs Location Code Voyage Code` %in% c("MAI","KAI","ROT","PHB","NSI","YON","DAR")), 
       aes(x=month.abb, y=(`Nitrate Nitrite [μmol/l]`/`Phosphate [μmol/l]`), fill=`Nrs Location Code Voyage Code`)) + 
  geom_boxplot() +
  #scale_fill_manual(values = nrsColors) +
  facet_grid (. ~ `Nrs Location Code Voyage Code`) + 
  theme_mo() + 
  labs(x="Month", y="N:P") +
  geom_jitter(aes(colour = `Nitrate Nitrite [μmol/l]`/`Phosphate [μmol/l]`), 
              alpha=0.7, position=position_jitter(w=0.4,h=0.2)) + 
    scale_color_gradientn(colors=oceColorsTwo(100)) +
   theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(legend.position = "none") +ylim(0,0.8)

npr
