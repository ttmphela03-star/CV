# Packages:

# EDA:

library(ggplot2)
library(tidyverse)
library(dplyr)
library(praznik)
library(hexbin)
library(patchwork)
library(VGAM)



# Reading in data:

train <- read.csv('train.csv')

test <- read.csv('test.csv')

origi <- read.csv('irrigation_prediction.csv')

# Viewing distribution of target vs features 

names(origi)

# Mi scores:

origi <- origi %>% 
  mutate(
    Irrigation_Need = case_when(
      Irrigation_Need == 'Low' ~ 0,
      Irrigation_Need == 'Medium' ~ 1,
      Irrigation_Need == 'High' ~ 2,
      TRUE ~ as.numeric(Irrigation_Need)
    )
  )

y <- origi$Irrigation_Need

X <- origi %>% 
  dplyr::select(-Irrigation_Need) %>% 
  mutate(across(where(is.character), as.factor))



MI_scores <- praznik::miScores(X,
                               y) %>% 
  sort() %>% 
  as.data.frame() 

colnames(MI_scores) <- 'Mi_Score'

MI_new <- t(MI_scores)

MI_scores %>% 
  ggplot(
   aes(
     x = Mi_Score,
     y = row.names(MI_scores),
     fill = row.names(MI_scores)
   )
  ) + geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = 'MI Scores for Irrigation Need Project',
    x = 'MI Score',
    y = 'Features'
  ) +
  geom_text(
    aes(
      label = round(Mi_Score, 3)
    ), hjust = -0.3
  )



# No distinct relationship found between Irrigation need and soil type

origi %>% 
  ggplot(
    aes(
      x = Soil_pH
    )
  ) +
  geom_histogram(color = 'black',
                 aes(
                   y = after_stat(density)
                 ),
                 fill = 'brown') +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)
  
# Distribution of soil ph by irrigation need: Normal for both low and medium. Negatively skewed for high 
#  Possibly add a column to show deviations from mean since the distribution of soil ph is normal
# Mean - mode for each category 

origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture
    )
  ) +
  geom_histogram(
  fill = 'lightblue',
  color = 'black',
  aes(
    y = after_stat(density)
  )
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need) 

# Main distribution also normally distributed

# High irrigation: A positively skewed distribution 
# Low irrigation: A negatively skewed distribution
# Medium irrigation: positively skewed distribution 
# Adding feature of mean - mode based on subset of data 

origi %>% 
  ggplot(
    aes(
      x = Organic_Carbon
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'violet',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# Main distribution of organic carbon is normally distributed 
# Medium and low irrigation need both normally distributed
# High irrigation skewed to left 
# Very similar output to soil ph

origi %>% 
  ggplot(
    aes(
      x = Electrical_Conductivity
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    color = 'black',
    fill = 'grey'
    ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# Electrical conductivity also normally distributed
# Across the board the electrical conductivity is normally distributed, nice transofrmation feature
# All skewed to the left 

origi %>% 
  ggplot(
    aes(
      x = Temperature_C
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    color = 'black',
    fill = 'seagreen'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)
  

# Main structure is normally distributed of temperature 
# High irrigation is drastically skewed to the left
# Low irrigation has normal
# Medium is slightly skewed left

origi %>% 
  ggplot(
    aes(
      x = Humidity
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    color = 'black',
    fill = 'yellow4'
  ) + 
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# Both low and medium have a normal distribution based on humidity
# high slightly skewed left
# Quite similar to electrical conductivity 

origi %>% 
  ggplot(
    aes(
      x = Rainfall_mm
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'darkgreen',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)
# High irrigation need skewed right in terms of rainfall 


origi %>% 
  ggplot(
    aes(
      x = sin(Sunlight_Hours * 2/pi)
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'darkred',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# Abnormalities found in high irrigation for sunlight hours 


origi %>% 
  ggplot(
    aes(
      x = sin(Wind_Speed_kmh * pi/2)
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'pink',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# High irrigation needed has a left skewed distribution in terms of wind speed 
# Low irrigation needed has a right skewed distribution 
# Medium irrigation needed has a left skewed distribution 

origi %>% 
  ggplot(
    aes(
      x = Field_Area_hectare
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'limegreen',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# high irrigation has a skewed left distribution. Other variables have normal.


origi %>% 
  ggplot(
    aes(
      x = Previous_Irrigation_mm
    )
  ) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    fill = 'lightyellow',
    color = 'black'
  ) +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)

# All normal except high irrigation, a few outliers present

# Investigating rainfall

origi %>% 
  ggplot(
    aes(
      x = Rainfall_mm
    )
  ) +
  geom_boxplot()+
  geom_vline(xintercept = mean(origi$Rainfall_mm), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)


g1 <- origi %>% 
  ggplot(
    aes(
      x = log(Rainfall_mm)
    )
  ) + 
  geom_density(aes(
    y = after_stat(density)
  ),
  fill = 'green',
  color = 'black') +
  facet_wrap(~Irrigation_Need)

g2 <- origi %>% 
  ggplot(
    aes(
      x = Rainfall_mm
    )
  ) + 
  geom_histogram(
  fill = 'red',
  color = 'black') +
  facet_wrap(~Irrigation_Need)

g1 | g2 # Log transformation is not a viable transformation 

origi['Dev_Rainfall'] <- (origi$Rainfall_mm - mean(origi$Rainfall_mm))

g3 <- origi %>% 
  ggplot(
    aes(
      x = Dev_Rainfall
    )
  ) + 
  geom_histogram(
  fill = 'green',
  color = 'black') +
  facet_wrap(~Irrigation_Need)

g2 | g3

aggregate(Irrigation_Need ~ cut(Dev_Rainfall,4),
          data = origi,
          FUN = mean)

g3

# Understanding the outliers of rainfall:

low_rain <- origi %>% 
  filter(Dev_Rainfall < -500 & Irrigation_Need == 0)


table(cut(low_rain$Wind_Speed_kmh,4))

# Notes: 
# - Harvest and sowing are more frequent 
# - More likely to have used mulching 
# - High soil moisture, values greater than 22
# - Low temperature areas
# - More common among low wind speed

par(mfrow=c(3,2))


interaction.plot(
  x.factor = cut(origi$Rainfall_mm, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Humidity, 3, labels = FALSE),
  trace.label =  'Humidity',
  ylim = c(0,2),
  col = c('red', 'orange','green'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Rainfall_mm by Humidity',
  lwd = 3
) # Clear interaction between rainfall and humidity


interaction.plot(
  x.factor = cut(origi$Rainfall_mm, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Soil_Moisture, 3, labels = FALSE),
  trace.label =  'Soil Moisture',
  ylim = c(0,2),
  col = c('red', 'black','green'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Rainfall_mm by Soil Moisture',
  lwd = 3
) # Clear interaction between rainfall mm and soil moisture


interaction.plot(
  x.factor = cut(origi$Rainfall_mm, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Previous_Irrigation_mm, 3, labels = FALSE),
  trace.label =  'Previous Rainfall',
  ylim = c(0,2),
  col = c('red', 'pink','purple'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Rainfall_mm \nby Previous Rainfall',
  lwd = 3
) # Clear interaction between previous rainfall and rainfall


interaction.plot(
  x.factor = cut(origi$Rainfall_mm, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Soil_pH, 3, labels = FALSE),
  trace.label =  'Previous Rainfall',
  ylim = c(0,2),
  col = c('seagreen', 'brown','yellow'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Rainfall_mm \nby Soil pH',
  lwd = 3
) # Interaction between rainfall and soil pH


interaction.plot(
  x.factor = cut(origi$Rainfall_mm, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Field_Area_hectare, 3, labels = FALSE),
  trace.label =  'Field',
  ylim = c(0,2),
  col = c('seagreen', 'brown','yellow'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Rainfall_mm \nby Field Area',
  lwd = 3
)

par(mfrow=c(1,1))

a <- aggregate(Irrigation_Need ~ cut(Rainfall_mm, 3) + cut(Soil_Moisture, 3),
               FUN = var,
               data = origi)

mean(a$Irrigation_Need)

origi %>% 
  ggplot(
    aes(
      x = Rainfall_mm,
      y = Soil_Moisture
    )
  ) +
  geom_smooth(method = 'loess', color = 'seagreen')

cor(origi$Soil_Moisture, origi$Rainfall_mm, method = 'pearson')

# Investigating temperature

origi %>% 
  ggplot(
    aes(
      x = Temperature_C
    )
  ) +
  geom_boxplot()+
  geom_vline(xintercept = mean(origi$Temperature_C), color = 'red', linewidth = 1, linetype = 2)+
  facet_wrap(~Irrigation_Need)

interaction.plot(
  x.factor = cut(origi$Temperature_C, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Sunlight_Hours, 3, labels = FALSE),
  trace.label =  'Sunlight',
  ylim = c(0,2),
  col = c('red', 'orange','darkorange'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Sunlight \nby Temperature',
  lwd = 3
) # Temperature depends on sunlight but not vice versa 


aggregate(Irrigation_Need ~ cut(Temperature_C,8),
          FUN = mean,
          data = origi)

origi %>% 
  ggplot(
    aes(
      x = Temperature_C
    )
  ) +
  geom_boxplot() +
  facet_wrap(~Irrigation_Need) +
  geom_vline(xintercept = mean(origi$Temperature_C),
             color = 'red',
             linetype = 2)

sd(origi$Temperature_C)

origi['Wind_Heat'] <- origi$Wind_Speed_kmh*origi$Temperature_C

sd(origi$Wind_Heat)

origi %>% 
  ggplot(
    aes(
      x = Wind_Heat,
      fill = as.factor(cut(Soil_Moisture,4))
    )
  ) +
  geom_boxplot() +
  facet_wrap(~Irrigation_Need) +
  geom_vline(xintercept = mean(origi$Wind_Heat),
             color = 'red',
             linetype = 2) # Soil moisture lessens the negative impact of heat, feature to explore later

origi %>% 
  ggplot(
    aes(
      x = Temperature_C,
      y = cos(2*pi*Sunlight_Hours/11)
    )
  ) +
  geom_smooth(method = 'loess')

# Understanding outliers within wind heat: 
# Slightly higher soil moisture
# Higher rainfall
# Rice crop types
# Either sowing or harvest growth stage 
# Canal irrigation type

origi['Standard_wind'] <- (origi$Wind_Speed_kmh - mean(origi$Wind_Speed_kmh))/sd(origi$Wind_Speed_kmh)

origi['Standard_temp'] <- (origi$Temperature_C) - mean(origi$Temperature_C)/sd(origi$Temperature_C)

origi %>% 
  ggplot(
    aes(
      x = Standard_temp,
      fill = as.factor(cut(Standard_wind,3))
    )
  ) +
  geom_boxplot()+
  geom_vline(xintercept = mean(origi$Standard_temp), color = 'red', linewidth = 1, linetype = 2)

origi %>% 
  ggplot(
    aes(
      x = Standard_temp,
      y = Standard_wind
    )
  ) +
  geom_smooth(method = 'loess')

cor(origi$Standard_temp, origi$Standard_wind, method = 'spearman') # No monotonic relationship

# Investigating Crop Growth Stage

origi %>% 
  ggplot(
    aes(
      x = Crop_Growth_Stage,
      y = Irrigation_Need
    )
  ) +
  geom_count()

b <- aggregate(Irrigation_Need ~ Crop_Growth_Stage + Soil_Type,
          FUN = var,
          data = origi)

mean(b$Irrigation_Need)



# Investigating soil moisture

aggregate(Irrigation_Need ~ cut(Soil_Moisture,8),
          FUN = mean,
          data = origi)

origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture
    )
  ) +
  geom_boxplot()+
  geom_vline(xintercept = mean(origi$Soil_Moisture), color = 'red', linewidth = 1, linetype = 2)+
  facet_wrap(~Irrigation_Need)

origi %>% 
  ggplot(
   aes(
     x = log(Soil_Moisture)
   )
  ) +
  geom_density(aes(
    y = after_stat(density)
  )) +
  facet_wrap(~Irrigation_Need)

interaction.plot(
  x.factor = cut(origi$Soil_Moisture, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = cut(origi$Soil_pH, 3, labels = FALSE),
  trace.label =  'pH',
  ylim = c(0,2),
  col = c('yellow', 'grey','black'),
  xlab = 'Soil Moisture',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Soil Moisture \nby Soil pH',
  lwd = 3,
  lty = 2
)  #Interaction found between soil moisture and pH

interaction.plot(
  x.factor = cut(origi$Soil_Moisture, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  fun = mean,
  trace.factor = origi$Soil_Type,
  trace.label =  'Soil Type',
  ylim = c(0,2),
  col = c('blue', 'green3','navyblue'),
  xlab = 'Rainfall',
  ylab = 'Irrigation Mean',
  main = 'Interaction Plot of Soil Moisture \nby Soil Type',
  lwd = 3,
  lty = 2
) # Interaction between soil moisture and type

sd(origi$Soil_Moisture)

soil_score <- aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_Moisture, 4),
          data = origi,
          FUN = mean)

# Visualising relationship



origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture,
      fill = as.factor()
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Soil_Moisture), linetype = 2, 
             linewidth = 1, color = 'red') +
  geom_vline(xintercept = mean(origi$Soil_Moisture) - sd(origi$Soil_Moisture), linetype = 2, 
             linewidth = 1, color = 'orange')

# Crop type is a nice feature to map with soil moisture

origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture
    )
  ) +
  geom_density(
    aes(
      y = after_stat(density)
    ),
    fill = 'limegreen'
  ) +
  facet_wrap(~Irrigation_Need)

var(origi$Soil_Moisture)

c <- aggregate(Soil_Moisture ~ Soil_Type,
               FUN = var,
               data = origi)

mean(c$Soil_Moisture)


# Investigating Mulching 

yes_df <- origi[origi$Mulching_Used == 'Yes',]

yes_df %>% 
  ggplot(
    aes(
      x = Mulching_Used,
      fill = Crop_Type
      
    )
  ) + geom_bar(
    aes(
      y = after_stat(count)
    )
  ) +
  facet_wrap(~Crop_Type)

mulch_df <- aggregate(Irrigation_Need ~ Mulching_Used + Crop_Type,
          data = origi,
          FUN = mean)

cut(mulch_df$Irrigation_Need,4)



# The factors that used mulching, required less irrigation 
par(mfrow = c(3,2))
interaction.plot(
  x.factor = origi$Mulching_Used,
  response = origi$Irrigation_Need,
  trace.factor = origi$Crop_Type,
  trace.label = 'Crop Type',
  col = c('red','blue','green','yellow','pink','purple'),
  lty = 2,
  lwd = 3,
  fun = mean
) # Clear interaction between mulching used and crop type

interaction.plot(
  x.factor = origi$Mulching_Used,
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Organic_Carbon, 3, labels = FALSE),
  trace.label = 'Organic carbon',
  col = c('red','blue','green'),
  lty = 2,
  lwd = 3,
  fun = mean
) # Clear interaction between organic carbon and mulching

interaction.plot(
  x.factor = origi$Mulching_Used,
  response = origi$Irrigation_Need,
  trace.factor = origi$Region,
  trace.label = 'Region',
  col = c('red','blue','green','yellow','grey'),
  lty = 2,
  lwd = 3,
  fun = mean
) # Clear interaction between region and mulching used 

interaction.plot(
  x.factor = origi$Mulching_Used,
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Previous_Irrigation_mm, 3, labels = FALSE),
  trace.label = 'Previous Irrigation',
  col = c('black','brown','orange'),
  lty = 2,
  lwd = 3,
  fun = mean
) # Interaction between mulching used and previous irrigation

interaction.plot(
  x.factor = origi$Mulching_Used,
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Electrical_Conductivity, 3, labels = FALSE),
  trace.label = 'Electrical Conductivity',
  col = c('red','blue','green'),
  lty = 2,
  lwd = 3,
  fun = mean 
)# Interaction between electrical conductivity and mulching

par(mfrow=c(1,1))

# Investigating wind speed

origi %>% 
  ggplot(
    aes(
      x = Wind_Speed_kmh
    )
  ) +
  geom_boxplot()+
  geom_vline(xintercept = mean(origi$Wind_Speed_kmh), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)

b <- aggregate(Irrigation_Need ~ cut(Rainfall_mm,6) + cut(Soil_Moisture, 6),
          FUN = var,
          data = origi)

mean(b$Irrigation_Need)

origi %>% 
  ggplot(
    aes(
      x = Wind_Speed_kmh
    )
  ) +
  geom_density(aes(
    y = after_stat(density)
  ), 
  fill = 'seagreen') +
  theme_minimal() +
  facet_wrap(~Irrigation_Need) # Higher wind speeds require more plot irrigation

origi %>% 
  ggplot(
    aes(
      x = Wind_Speed_kmh,
      y = Soil_Moisture
    )
  ) +
  geom_smooth(formula = y~x, color = 'peru', method = 'loess')  +
  facet_wrap(~Irrigation_Need)

origi['Standard_soil'] = (origi$Soil_Moisture - mean(origi$Soil_Moisture))/sd(origi$Soil_Moisture)

cor(origi$Standard_soil, origi$Standard_wind, method = 'kendall')

# Soil Moisture and wind speed do not heave a clear linear relationship


interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = origi$Season,
  trace.label = 'Season',
  col = c('red','blue','green'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # clear interaction between wind speed and season

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = origi$Region,
  trace.label = 'Region',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Clear interaction between region and wind speed

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = origi$Irrigation_Type,
  trace.label = 'Irrigation Type',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Interaction between wind speed and irrigation type

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Temperature_C,3, labels = FALSE),
  trace.label = 'Temperature',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # No interaction between wind speed and temperature

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Soil_Moisture,3, labels = FALSE),
  trace.label = 'Soil Moisture',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Clear interaction between wind speed and soil moisture

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Sunlight_Hours,3, labels = FALSE),
  trace.label = 'Sunlight Hours',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Clear interaction between wind speed and sunlight hours 

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Humidity,3, labels = FALSE),
  trace.label = 'Humidity',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Clear interaction between wind speed and humidity 

interaction.plot(
  x.factor = cut(origi$Wind_Speed_kmh, 3, labels = FALSE),
  response = origi$Irrigation_Need,
  trace.factor = cut(origi$Previous_Irrigation_mm,3, labels = FALSE),
  trace.label = 'Previous Irrigation',
  col = c('red','blue','green','yellow','orange','purple'),
  lty = 2,
  lwd = 3,
  fun = mean 
) # Clear interaction between wind speed and previous irrigation

origi %>% 
  ggplot(
    aes(
      x = Wind_Speed_kmh
    )
  ) +
  geom_freqpoly() +
  theme_minimal() +
  facet_wrap(~Season) 

# No change seen when filling plots by season

# Understanding variation between features and how another feature reduces sed variation

var(origi$Irrigation_Need)

aggregate(Irrigation_Need ~ Region + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var) 

aggregate(Irrigation_Need ~ Irrigation_Type + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var) 

aggregate(Irrigation_Need ~ cut(origi$Soil_Moisture,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var) 

aggregate(Irrigation_Need ~ cut(origi$Sunlight_Hours,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ Season + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Humidity,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Previous_Irrigation_mm,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

c <- aggregate(Irrigation_Need ~ cut(origi$Temperature_C,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var) # Non-linear interaction between wind speed and temperature but a non-linear interaction exists 
# Since the variation of irrigation decreases when the factors are combined 

aggregate(Irrigation_Need ~ cut(origi$Soil_pH,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Field_Area_hectare,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Organic_Carbon,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Electrical_Conductivity,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

aggregate(Irrigation_Need ~ cut(origi$Soil_pH,3) + cut(origi$Wind_Speed_kmh, 3),
          data = origi,
          FUN = var)

# Investigating season

origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture
    )
  ) +
 geom_density(kernel = 'gaussian', fill = 'seagreen', color = 'black') +
  theme_minimal() +
  facet_wrap(~Season) +
  geom_hline(yintercept = 0.017, color = 'red', linetype = 2)+
  geom_vline(xintercept = max(origi$Soil_Moisture), color = 'red', linetype = 2) +
  geom_vline(xintercept = min(origi$Soil_Moisture), color = 'red', linetype = 2)

origi %>% 
  ggplot(
    aes(
      x = Irrigation_Need
    )
  ) +
  geom_histogram(fill = 'green') +
  theme_minimal() + 
  facet_wrap(~Season)

# Irrigation need by season 

df_season <- origi %>% 
  dplyr::select(-Irrigation_Need) %>% 
  mutate(
    Season = case_when(
      Season == 'Rabi' ~ 0,
      Season == 'Kharif' ~ 1,
      Season == 'Zaid' ~ 2,
      TRUE ~ as.numeric(Season)
    )
  ) %>% 
  mutate(across(where(is.character), as.factor))

model <- vglm(
  formula = Season ~ Soil_Type +Region ,
  family = 'multinomial',
  data = df_season
)

summary(model)

# Region, soil_type are  significant predictors of season
# Key components: Silt soil, sandy soil and the west region

var(origi$Irrigation_Need)

aggregate(Irrigation_Need ~ Region + Soil_Type + Season,
          FUN = var,
          data = origi)


mean(origi$Irrigation_Need)
sd(origi$Irrigation_Need)
aggregate(Irrigation_Need ~ Season + Irrigation_Need,
          data = origi,
          FUN = mean)

mean(origi$Rainfall_mm)
sd(origi$Rainfall_mm)
aggregate(Rainfall_mm ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Temperature_C)
sd(origi$Temperature_C)
aggregate(Temperature_C ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Soil_Moisture)
sd(origi$Soil_Moisture)
aggregate(Soil_Moisture ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Wind_Speed_kmh)
sd(origi$Wind_Speed_kmh)
aggregate(Wind_Speed_kmh ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Soil_pH)
sd(origi$Soil_pH)
aggregate(Soil_pH ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Humidity)
sd(origi$Humidity)
aggregate(Humidity ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Sunlight_Hours)
sd(origi$Sunlight_Hours)
aggregate(Sunlight_Hours ~ Season,
          data = origi,
          FUN = mean)

mean(origi$Previous_Irrigation_mm)
sd(origi$Previous_Irrigation_mm)
aggregate(Previous_Irrigation_mm ~ Season,
          data = origi,
          FUN = mean)

origi %>% 
  ggplot(
    aes(
      x = Crop_Growth_Stage
    )
  ) +
  geom_histogram(fill = 'blue', color = 'black', stat = 'count')+
  theme_minimal() +
  facet_wrap(~Season)
unique(origi$Irrigation_Type)

aggregate(Irrigation_Need ~ Irrigation_Type,
          FUN = mean,
          data = origi)

# Investigating Region
mosaicplot(Soil_Type ~ Region,
           data = origi,
           col = c('red','blue','green','yellow','purple','pink'))


aggregate(Irrigation_Need ~ Region, 
          FUN = mean,
          data = origi)

table(origi$Irrigation_Need, origi$Region)

# Investigation soil type

mean(origi$Irrigation_Need)
sd(origi$Irrigation_Need)
 a <- aggregate(Irrigation_Need ~ Soil_Type + cut(origi$Soil_pH,2) + 
                  cut(origi$Organic_Carbon,2) + cut(origi$Field_Area_hectare,2) + 
                  cut(origi$Electrical_Conductivity,2) + Crop_Type,
          data = origi,
          FUN = mean)
 
 par(mfrow=c(2,3))
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = origi$Crop_Type,
   trace.label = 'Region',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 )
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = cut(origi$Field_Area_hectare, 3, labels = FALSE),
   trace.label = 'Field Area',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 )
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = cut(origi$Soil_pH, 3, labels = FALSE),
   trace.label = 'Soil pH',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 )
 
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = cut(origi$Organic_Carbon,3, labels = FALSE),
   trace.label = 'OC',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 )
 
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = cut(origi$Electrical_Conductivity,3, labels = FALSE),
   trace.label = 'EC',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 )
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = cut(origi$Soil_Moisture,3, labels = FALSE),
   trace.label = 'Moisture',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 ) # Clear interaction between soil moisture and soil type
 
 interaction.plot(
   x.factor = origi$Soil_Type,
   response = origi$Irrigation_Need,
   trace.factor = origi$Crop_Growth_Stage,
   trace.label = 'Growth',
   col = c('red','blue','green','yellow','orange','purple'),
   lty = 2,
   lwd = 3,
   fun = mean 
 ) # Clear interaction between crop growth stage and soil type 
 
 origi %>% 
   ggplot(
     aes(
       x = Soil_pH
     )
   ) +
   geom_histogram() +
   facet_wrap(~Irrigation_Need)
 

 
 par(mfrow=c(1,1))
 
 var(origi$Irrigation_Need)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_pH,3),
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 
 aggregate(Irrigation_Need ~ Soil_Type + cut(Electrical_Conductivity,3),
           data = origi,
           FUN = var)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + Crop_Type,
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 aggregate(Irrigation_Need ~ Soil_Type + cut(Humidity,3),
           data = origi,
           FUN = var)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + Irrigation_Type,
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 aggregate(Irrigation_Need ~ Soil_Type + cut(Organic_Carbon,3),
           data = origi,
           FUN = var)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_Moisture,3),
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + Region,
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + Season,
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
 aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_pH,3),
           data = origi,
           FUN = var)
 
 aggregate(Irrigation_Need ~ Soil_Type + cut(Sunlight_Hours,3),
           data = origi,
           FUN = var)
 
 b <- aggregate(Irrigation_Need ~ Soil_Type + Water_Source ,
           data = origi,
           FUN = var)
 
 mean(b$Irrigation_Need)
 
b <- aggregate(Irrigation_Need ~ Soil_Type + Mulching_Used ,
           data = origi,
           FUN = var)
mean(b$Irrigation_Need)

 
aggregate(Irrigation_Need ~ Soil_Type + cut(Wind_Speed_kmh,3) ,
           data = origi,
           FUN = var)

 
aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_Moisture,3) ,
           data = origi,
           FUN = var)

aggregate(Irrigation_Need ~ Soil_Type + cut(Soil_Moisture,3) ,
                data = origi,
                FUN = mean)

 
aggregate(Irrigation_Need ~ Soil_Type + cut(Rainfall_mm,3) ,
           data = origi,
           FUN = var)
 

c <- aggregate(Irrigation_Need ~ Soil_Type + Crop_Growth_Stage ,
          data = origi,
          FUN = var)

d <- aggregate(Irrigation_Need ~ Soil_Type + Crop_Growth_Stage + Mulching_Used,
               data = origi,
               FUN = var)

mean(c$Irrigation_Need) 
mean(d$Irrigation_Need)
 

 
 # Building a general linear model for logistic regression to find best interaction terms 
origi <-  origi %>% 
  mutate(across(where(is.character), as.factor))
 
 
 #lrm <- vglm(Irrigation_Need ~ (.)^2,
           # family = 'multinomial',
           # data = origi)
 
#lrm_summary <- summary(lrm)

# No significant interaction terms found

# New feature: Good conditions

low_irri <- subset(origi, origi$Irrigation_Need == 0)

mean(low_irri$Soil_Moisture)

quantile(origi$Soil_Moisture, 0.75)

origi %>% 
  ggplot(
    aes(
      x = Soil_Moisture,
      fill = as.factor(Mulching_Used)
    )
  ) + geom_boxplot() +
  facet_wrap(~Irrigation_Need) +
  theme(
    legend.key.size = unit(0.4, "cm")
  ) +
guides(
  color = guide_legend(override.aes = list(size = 2))
)
  
 
# Building a new feature: General Weather
# Components:
# - Season, Rainfall, Humidity, Region, Temperature, Wind speed

season_prop <- (table(origi$Irrigation_Need, origi$Season))
print(season_prop/nrow(origi))

row.means <- rowMeans(season_prop)

(season_prop - row.means)

colMeans(season_prop - row.means)

aggregate(Irrigation_Need ~ Season,
          data = origi,
          FUN = mean)

# We've added direction and magnitude to season data

# New feature: 
# Effect of mulch on soil moisture

aggregate(Irrigation_Need ~ cut(Soil_Moisture, 4) + Mulching_Used,
          data = origi,
          FUN = mean)

#Investigating previous irrigation 

mean(origi$Previous_Irrigation_mm)

sd(origi$Previous_Irrigation_mm)

origi %>% 
  ggplot(
    aes(
      x = Previous_Irrigation_mm
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Previous_Irrigation_mm), linewidth = 1, linetype = 2, colour = 'red' ) +
  facet_wrap(~Irrigation_Need)

b <- aggregate(Irrigation_Need ~ cut(Previous_Irrigation_mm, 3, labels = FALSE) + Crop_Growth_Stage,
          FUN = var,
          data = origi)

mean(b$Irrigation_Need)

interaction.plot(
  x.factor = cut(origi$Previous_Irrigation_mm,3),
  response = origi$Irrigation_Need,
  trace.factor = origi$Crop_Growth_Stage,
  trace.label = 'Growth Stage',
  col = c('chocolate','coral','brown','salmon','darkred'),
  lwd = 2,
  lty = 2
) # Interaction between growth stage in previous irrigation 

# New feature: Water score

origi['Water_Score'] <- log(origi$Soil_Moisture * origi$Humidity * origi$Rainfall_mm * origi$Previous_Irrigation_mm)


origi %>% 
  ggplot(
    aes(
      x = Water_Score
    )
  ) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  theme_minimal() +
  facet_wrap(~Irrigation_Need)



 # Investigating humidity

origi %>% 
  ggplot(
    aes(
      x = Humidity
    )
  ) +
  geom_boxplot(color = 'black') +
  facet_wrap(~Irrigation_Need) +
  geom_vline(xintercept = mean(origi$Humidity), color = 'red')

var(origi$Humidity)

c <- aggregate(Humidity ~ cut(Rainfall_mm, 3) + cut(Soil_Moisture, 3) ,
          FUN = var,
          data = origi) # No combination of features reduces the variance within humidity

mean(c$Humidity)



# Investigating field area hectare

origi %>% 
  ggplot(
    aes(
      x = Field_Area_hectare
    )
  ) +
  geom_boxplot(fill = 'azure3')+
  facet_wrap(~Irrigation_Need) +
  geom_vline(xintercept = mean(origi$Field_Area_hectare), color = 'red', linetype = 2, linewidth = 1)



# Investigating sunlight hours

origi %>% 
  ggplot(
    aes(
      x = Sunlight_Hours
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Sunlight_Hours), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)

# Investigating soil pH

origi %>% 
  ggplot(
    aes(
      x = Soil_pH
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Soil_pH), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)

df1 <- origi[which(origi$Irrigation_Need == 2),]
mean(df1$Soil_pH)
mean(origi$Soil_pH)
summary(origi$Soil_pH)

# Investigating organic carbon
origi %>% 
  ggplot(
    aes(
      x = Organic_Carbon
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Organic_Carbon), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)
# Investigating electrical conductivity

origi %>% 
  ggplot(
    aes(
      x = Electrical_Conductivity
    )
  ) +
  geom_boxplot() +
  geom_vline(xintercept = mean(origi$Electrical_Conductivity), color = 'red', linetype = 2, linewidth = 1)+
  facet_wrap(~Irrigation_Need)



