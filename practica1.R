install.packages(c('dplyr','ggplot2','visdat', 'skimr', 'insuranceData'))

require(dplyr)
require(ggplot2)
require(visdat)
require(skimr)
require(insuranceData)

data(dataCar)
str(dataCar) 
summary(dataCar)

#Siniestros de acuerdo al tipo de vehiculo
sin_veh <- dataCar %>%
  group_by(veh_body) %>%
  summarise(sin = sum(clm)) %>%
  arrange(desc(sin))
sin_veh

ggplot(sin_veh, aes(x = "", y = sin, fill = veh_body)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Siniestros por tipo de vehículo",
       fill = "Tipo de vehículo") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif", size = 16, face = "bold"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())


#Monto total de las reclamciones de acuerdo al tipo de vehiculo y el genero 
mayor_claims <- dataCar %>%
  group_by(veh_body, gender) %>%
  summarise(montorecla = sum(claimcst0)) %>%
  arrange(desc(montorecla))
mayor_claims
sum(mayor_claims$montorecla)


ggplot(mayor_claims, aes(x = reorder(veh_body, -montorecla), y = montorecla, fill = gender)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c("F" = "hotpink2", "M" = "dodgerblue3")) +
  labs(title = 'Monto total de reclamaciones por tipo de vehículo',
    x = 'Tipo de vehículo',
    y = 'Monto de reclamaciones') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "serif", size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1))


#Siniestros por area, genero y edad de las personas 
cond_ries <- dataCar %>%
  group_by(agecat, gender, area) %>%
  summarise(ries = sum(clm)) %>%
  arrange(desc(ries))
cond_ries

ggplot(cond_ries, aes(x = area, y = gender, fill = ries)) +
  geom_tile() +
  scale_fill_gradient(low = "lightpink", high = "deeppink3") +
  labs(title = "Siniestros por área y género",
    x = "Área geográfica",
    y = "Género",
    fill = "Número de siniestros"
  ) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, family = "serif", size = 16, face = "bold"))
