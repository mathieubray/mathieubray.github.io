---
title: "Hello World"
author: "Mathieu Bray"
date: "December 23, 2016"
layout: post
tags: [testing, things, out]
---

Hi there, this is my blog. Here is some preamble.



```r
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(animation)

ani.options(interval = .5)
theme_set(theme_bw(16))
```


Now we'll make some data.



```r
player.bios <- read.csv("data/NHLPlayerBios.csv",header=T,stringsAsFactors=F) %>%
  select(Name,Season,Height,Weight,Position) %>%
  rowwise() %>%
  mutate(NewPosition = ifelse(Position == "D", "D", "F")) %>%
  ungroup()

goalie.bios <- read.csv("data/NHLGoalieBios.csv",header=T,stringsAsFactors=F) %>%
  select(Name,Season,Height,Weight,Position) %>%
  mutate(NewPosition="G")

bios <- rbind(player.bios,goalie.bios) %>%
  filter(!is.na(Height),!is.na(Weight)) %>%
  arrange(Season,Name) %>%
  mutate(Year = as.integer(substring(as.character(Season),5,8)))

head(bios)
```

```
## # A tibble: 6 ? 7
##             Name   Season Height Weight Position NewPosition  Year
##            <chr>    <int>  <int>  <int>    <chr>       <chr> <int>
## 1   Aaron Downey 20022003     73    215        R           F  2003
## 2    Aaron Gavey 20022003     74    189        C           F  2003
## 3   Aaron Miller 20022003     75    210        D           D  2003
## 4     Aaron Ward 20022003     74    209        D           D  2003
## 5 Adam Deadmarsh 20022003     72    205        R           F  2003
## 6     Adam Foote 20022003     74    220        D           D  2003
```

And now we plot...


```r
p <- ggplot(bios, aes(x=Height,y=Weight,color=NewPosition,shape=NewPosition,frame=Year)) +
  geom_jitter(size=5,alpha=0.7) +
  scale_color_manual(name="Position",values=c("blue","orange","purple")) +
  scale_shape_manual(name="Position",values=c(16,15,17)) +
  xlim(60,85) +
  xlab("Height (in.)")+
  ylim(120,275) +
  ylab("Weight (lb.)") +
  ggtitle("NHL Players Height and Weight: ")
```


```r
gganimate(p)
```


![No GIF???](/plots/HeightAndWeight.gif) 

Hurray!
