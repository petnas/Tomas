library(xlsx)
library(reshape2)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

raw <- read.xlsx('duomenys.xlsx', 'Sheet1')
raw <- raw[-1]

col_names=names(raw)

raw_f = as.data.frame(unclass(raw), stringsAsFactors = TRUE)

str(raw_f)########################


#Grouped bar charts
values <- c(36,0,27,1,47,1)


gfg <- data.frame(x = values,  
                  grp =  rep(c('Po paruošiamosios operacijos', 
                               'Po II operacijos',
                               'Po Vienmomentinės'), each=2), 
                  subgroup = LETTERS[1:2]) 
# Modifying data 
gfg <- reshape(gfg,idvar = "subgroup", 
               timevar = "grp", 
               direction = "wide") 

row.names(gfg) <- gfg$subgroup 
gfg <- gfg[ , 2:ncol(gfg)] 
colnames(gfg) <- c('Po paruošiamosios operacijos', 
                   'Po II operacijos',
                   'Po Vienmomentinės')
gfg <- as.matrix(gfg) 

# Create grouped barplot 

barplot(height = gfg,beside = TRUE, xlab = 'Operacijų stadijos',
        ylab = 'Pacientų kiekis', main = 'Atvejai su atrastais navikais ', legend.text = c('Nerasta', 'Rasta'),
        space = c(0, 0.4), ylim = c(0,50))


#####################################

gfg <- data.frame(
  Stage = c('Po paruošiamosios operacijos', 
            'Po II operacijos',
            'Po Vienmomentinės'),
  Nerasta = c(36, 27, 47),
  Rasta = c(0, 1, 1)
)

# Reshape data to long format
gfg_long <- tidyr::gather(gfg, key = "Result", value = "Count", -Stage)

# Create grouped bar chart with ggplot2
ggplot(gfg_long, aes(x = Stage, y = Count, fill = Result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = 'Atvejai su atrastais navikais',
       x = 'Operacijų stadijos',
       y = 'Pacientų kiekis') +
  scale_fill_manual(values = c("Rasta" = "gray70", "Nerasta" = "gray30")) +
  theme_minimal()
        