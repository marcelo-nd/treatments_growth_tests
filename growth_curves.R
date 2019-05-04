library(XLConnect)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)


######################################## Growth curve 2_5_19 ########################################

### Reading data
growth_2_5_19 <- readWorksheetFromFile("C:/Users/marce/OneDrive/curvas_comparacion_tratamientos.xlsx", 
                                     sheet=1
                                    )
growth_2_5_19

str(growth_2_5_19)

### Getting the cummulative sums of each replicate

growth_2_5_19$RplCs1 <- cumsum(growth_2_5_19$Repl1)
growth_2_5_19$RplCs2 <- cumsum(growth_2_5_19$Repl2)
growth_2_5_19$TrsCs1 <- cumsum(growth_2_5_19$Trs1)
growth_2_5_19$TrsCs2 <- cumsum(growth_2_5_19$Trs2)

### Get the means of each treatment.
growth_2_5_19$Rplmean <- rowMeans(growth_2_5_19[c('RplCs1', 'RplCs2')], na.rm=TRUE)

growth_2_5_19$Trsmean <- rowMeans(growth_2_5_19[c('TrsCs1', 'TrsCs2')], na.rm=TRUE)

### Get the standard deviations for each treatment.

growth_2_5_19 <- growth_2_5_19 %>% mutate(RplStDev = apply(.[(7:8)],1,sd))

growth_2_5_19 <- growth_2_5_19 %>% mutate(TrsStDev = apply(.[(9:10)],1,sd))

### Extract only the useful data columns
growth_2_5_19 <- select(growth_2_5_19, "Hours", "Rplmean", "Trsmean", "RplStDev", "TrsStDev")

growth_2_5_19 <- gather(growth_2_5_19, "Rplmean", "Trsmean", key = "Treatment", value = "Biogas")

### Paste the std on a separate column
growth_2_5_19$std <- c(growth_2_5_19$RplStDev[1:19], growth_2_5_19$TrsStDev[20:38])

growth_2_5_19

ggplot(growth_2_5_19, aes(x=Hours, y=Biogas, colour=Treatment)) + 
  geom_errorbar(aes(ymin=Biogas-std, ymax=Biogas+std), width=.1) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  xlab('Horas') +
  ylab('Biogás(mL)') +
  ggtitle("Producción de biogás (mL); 2.5.19") +
  labs(colour = "Tratamiento") +
  scale_color_discrete(labels = c("Reemplazo", "Traspaso")) +
  theme_hc()


######################################## Growth curve 3_5_19 ########################################

### Reading data
growth_3_5_19 <- readWorksheetFromFile("C:/Users/marce/OneDrive/curvas_comparacion_tratamientos.xlsx", 
                                       sheet=2
)

growth_3_5_19 <- readWorksheetFromFile("C:/Users/marce/Desktop/curvas_comparacion_tratamientos.xlsx", 
                                       sheet=2
)
growth_3_5_19

str(growth_3_5_19)

### Getting the cummulative sums of each replicate

growth_3_5_19$RplCs1 <- cumsum(growth_3_5_19$Repl1)
growth_3_5_19$RplCs2 <- cumsum(growth_3_5_19$Repl2)
growth_3_5_19$TrsCs1 <- cumsum(growth_3_5_19$Trs1)
growth_3_5_19$TrsCs2 <- cumsum(growth_3_5_19$Trs2)

### Get the means of each treatment.
growth_3_5_19$Rplmean <- rowMeans(growth_3_5_19[c('RplCs1', 'RplCs2')], na.rm=TRUE)

growth_3_5_19$Trsmean <- rowMeans(growth_3_5_19[c('TrsCs1', 'TrsCs2')], na.rm=TRUE)

### Get the standard deviations for each treatment.

growth_3_5_19 <- growth_3_5_19 %>% mutate(RplStDev = apply(.[(7:8)],1,sd))

growth_3_5_19 <- growth_3_5_19 %>% mutate(TrsStDev = apply(.[(9:10)],1,sd))

### Extract only the useful data columns
growth_3_5_19 <- select(growth_3_5_19, "Hours", "Rplmean", "Trsmean", "RplStDev", "TrsStDev")

growth_3_5_19 <- gather(growth_3_5_19, "Rplmean", "Trsmean", key = "Treatment", value = "Biogas")

### Paste the std on a separate column
growth_3_5_19$std <- c(growth_3_5_19$RplStDev[1:22], growth_3_5_19$TrsStDev[23:44])

growth_3_5_19

ggplot(growth_3_5_19, aes(x=Hours, y=Biogas, colour=Treatment)) + 
  geom_errorbar(aes(ymin=Biogas-std, ymax=Biogas+std), width=.1) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  xlab('Horas') +
  ylab('Biogás(mL)') +
  ggtitle("Producción de biogás (mL); 3.5.19") +
  labs(colour = "Tratamiento") +
  scale_color_discrete(labels = c("Reemplazo", "Traspaso")) +
  theme_hc()

