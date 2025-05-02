library(tidyverse)
library(lubridate)
source('./R/00_utils/colors.R')
theme_set(theme_bw())

path <- './data/raw/elke_tables/Provider-Table_mod.csv'
dat <- read.csv(path)

dat <- dat[1:69,]
dat$Start <- mdy(dat$Start)
dat$Ende  <- mdy(dat$Ende)
dat$Woche <- week(dat$Start)
dat$Jahr  <- factor(year(dat$Start))
dat <- dat %>% mutate_if(is.character, as.numeric)


# What do we do with inconsistencies?!
sum(dat$Kontakte_u60 !=  dat$Kontakte_gesamt - dat$Kontakte_a60, na.rm = T)

# Looking at the overall number of calls this year
dat %>% 
  filter(Start >  mdy("01-01-2021")) %>% 
  ggplot(aes(x = Start, y =  Kontakte_gesamt)) +
  geom_bar(stat =  'identity', fill = corporate_purple) +
  geom_smooth(col = corporate_rose, se = FALSE) +
  labs(x = "", y = "Kontakte", title = 'Kontakte in 2021') +
  coord_cartesian(ylim = c(1000, 2200))

# ... and in total. Christmas seems to be a problem
dat %>% 
  filter(Woche <  30) %>% 
  ggplot(aes(x = Start, y =  Kontakte_gesamt)) +
  geom_bar(stat =  'identity', fill = corporate_purple) +
  geom_smooth(col = corporate_rose, se = FALSE) +
  labs(x = "", y = "Kontakte", title = 'Kontakte insgesamt')


# Reorgansing the data
dat_kont <- dat %>% 
  filter(Start >= mdy("01-01-2021")) %>% 
  select(starts_with('Kontakt'), Woche, Start) %>% 
  pivot_longer(cols = starts_with('Kontakt'), names_prefix = 'Kontakte_', names_to = 'type')

# dat_kont %>% 
#   filter(type != 'gesamt') %>% 
#   ggplot() +
#   geom_bar(aes(x = Woche, y = value, fill = type), 
#            stat = 'identity', position = 'dodge') +
#   labs(x = "Kalenderwoche", y = "Kontakte") +
#   #scale_x_continuous(breaks =) +
#   scale_fill_manual("Länge:", 
#                     values = c('a60' = corporate_yellow, 'u60' = corporate_purple),
#                     labels = c(">60 Sek.", "< 60 Sek."))

# Split by length
# First time that there are more >60sec than <60sec calls
dat_kont %>% 
  filter(type != 'gesamt') %>% 
  ggplot() +
  geom_bar(aes(x = Start, y = value, fill = type), 
           stat = 'identity', position = 'dodge', alpha = 0.6) +
  geom_smooth(aes(x = Start, y = value, col = type), se = FALSE) +
  labs(x = "Kalenderwoche", y = "Kontakte", title = 'Kontakte in 2021') +
  #scale_x_continuous(breaks =) +
  scale_fill_manual("Länge:", 
                    values = c('a60' = corporate_yellow, 'u60' = corporate_purple),
                    labels = c(">60 Sek.", "< 60 Sek.")) +
  scale_color_manual("Länge:", 
                     values = c('a60' = corporate_red, 'u60' = corporate_blue),
                     labels = c(">60 Sek.", "< 60 Sek.")) +
  theme(legend.position = 'right') +
  scale_y_continuous(expand = c(0, 0))
#scale_x_continuous(expand = c(0, 0))



dat %>% 
  filter(Start > mdy("01-01-2021")) %>% 
  select(Woche, Anrufer_verschieden, First_call, Start) %>% 
  mutate(Anruf_wiederholer = Anrufer_verschieden - First_call) %>% 
  select(-Anrufer_verschieden) %>% 
  pivot_longer(cols = c(Anruf_wiederholer, First_call), names_to = 'type') %>% 
  ggplot(aes(x = Start ,y = value, fill = type)) +
  geom_bar(stat = 'identity' ) +
  labs(x = "Kalenderwoche", y = "Anrufer*innen", title = 'Anruftyp in 2021') +
  scale_fill_manual("Anrufer*in:", 
                    values = c('Anruf_wiederholer' = corporate_rose, 'First_call' = corporate_purple),
                    labels = c("Wiederholer*in", "1. Anrufer*in")) +
  scale_y_continuous(expand = c(0, 0))

