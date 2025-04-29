require(pacman)
p_load(haven, dplyr, factoextra, FactoMiner, readr, rgl, fpc, psych, readxl)

datos1 <- read.csv('data_pca.csv', sep = ';', dec = ',')
datos1 <- datos1[-16]

#Normalizar datos
datos1 <- scale(datos1)  #quita la primera columna
#datos normalizados
head(datos1,10)

#Realizar pca
#Diagnostico área el pca 
#Si el determinante tiende a cero se puede aplicar
det(cor(datos1))

pca1<-princomp(datos1)
pca1$loadings

#diagnostico
summary(pca1)
#Hasta el momento se observa que los principales componentes que aportan mayor varianza 
# son 1 y 2.

#Revisar varianza y eigenvalores
fviz_eig(pca1, choice = 'variance')
#Efectivamente la componente uno y dos aportan l amayor varianza
fviz_eig(pca1, choice = 'eigenvalue')
#Solo dos componentes tienen un eigenvalor mayor a la unidad.
# Lo adecuado es extraer unicamente dos factores.


#ANALISIS GRAFICO
# El COSENO CUADRADO se utiliza para medir la calidad de la representacion de las 
# variables originales en el espacio de los componentes principales.
# Especificamente, el coseno cuadrado de una variable en un componente principal
# es el cudrado del coseno del angulo entre la  variable original y el componente 
# principal

#Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca1,
             col.ind = 'cos2',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)

# El coseno cuadrado indica que proporcion de la variaza de la variable original 
# es explicada por el componente principal.
#Valores altos de coseno cudrado (cercanos a 1) significan que la variable esta
# bien representada por el componente principal.
# Mientras que valores bajos (cercanos a 0) indican una mala representacion.

# Las observaciones en color verde fuerte son representadas en mejor medida.
# Las observaciones 28,23,25 y 27 no son tan bien representadas pero son la  minoria.


#Grafico de las cargas.
#¿Cuanto contribuye cada variable a las diferentes componentes principales?
#Los componetes contribuyen en diferente medida a los cuadrantes dentro de 
# la representacion bidimensional.
fviz_pca_var(pca1,
             col.var = 'contrib',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)
# En lo anterior se observa que las variables encias, caries y cavidades se presentan
# en una dimension.
# El aliento fresco, dientes brillantes y dientes atractivos son una dimension diferente.


x11()
psych::cor.plot(datos1)
#analisis como lo proporciona spss
#se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(datos1))

#El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación es muy alto.

#Resultado del pca por factores: 
#la rotación más omún es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
pca11 <- psych::principal(datos1, nfactors = 6, residuals = FALSE, rotate = "varimax", 
                         scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca11$weights[,1]
pca11$weights[,2]
pca11$weights[,3]
pca11$weights[,4]
pca11$weights[,5]
pca11$weights[,6]

#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca11$scores

#La primera puede ser considerada un factor de "salud" y la segunda un factor "estético"


datos2 <- read_excel('PoblacionUSA.xlsx')
datos20 <- datos2[, c(2,3,5,7,9,11,13,15,17,19)]

#Normalizar datos
datos20 <- scale(datos20)  #quita la primera columna
#datos normalizados
head(datos20,10)

#Realizar pca
#Diagnostico área el pca 
#Si el determinante tiende a cero se puede aplicar
det(cor(datos20))

#Calcular factor de adecuacion muestral Kaiser-Meyer-Olkin
# De 0.5-0.6 mala, 0.6-0.7 adecuado y mayor de 0.7 bueno (MSA)
psych::KMO(datos20)
# Todas las variables poseen una msa mayor a 0.5, por lo que es pértinente el pca.
pca20<-princomp(datos20)
pca20$loadings

#diagnostico
summary(pca20)
#Hasta el momento se observa que los principales componentes que aportan mayor varianza 
# son 1 y 2.

#Revisar varianza y eigenvalores
fviz_eig(pca20, choice = 'variance')
#Efectivamente la componente uno y dos aportan l amayor varianza
fviz_eig(pca20, choice = 'eigenvalue')
#Solo dos componentes tienen un eigenvalor mayor a la unidad.
# Lo adecuado es extraer unicamente dos factores.


#ANALISIS GRAFICO
# El COSENO CUADRADO se utiliza para medir la calidad de la representacion de las 
# variables originales en el espacio de los componentes principales.
# Especificamente, el coseno cuadrado de una variable en un componente principal
# es el cudrado del coseno del angulo entre la  variable original y el componente 
# principal

#Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca20,
             col.ind = 'cos2',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)

# El coseno cuadrado indica que proporcion de la variaza de la variable original 
# es explicada por el componente principal.
#Valores altos de coseno cudrado (cercanos a 1) significan que la variable esta
# bien representada por el componente principal.
# Mientras que valores bajos (cercanos a 0) indican una mala representacion.

# Las observaciones en color verde fuerte son representadas en mejor medida.
# Las observaciones 28,23,25 y 27 no son tan bien representadas pero son la  minoria.


#Grafico de las cargas.
#¿Cuanto contribuye cada variable a las diferentes componentes principales?
#Los componetes contribuyen en diferente medida a los cuadrantes dentro de 
# la representacion bidimensional.
fviz_pca_var(pca20,
             col.var = 'contrib',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)
# En lo anterior se observa que las variables encias, caries y cavidades se presentan
# en una dimension.
# El aliento fresco, dientes brillantes y dientes atractivos son una dimension diferente.


x11()
psych::cor.plot(datos20)
#analisis como lo proporciona spss
#se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(datos20))

#El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación es muy alto.

#Resultado del pca por factores: 
#la rotación más omún es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
pca201 <- psych::principal(datos20, nfactors = 2, residuals = FALSE, rotate = "varimax", 
                          scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca201$weights[,1]
pca201$weights[,2]


#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca201$scores

#La primera puede ser considerada un factor de "salud" y la segunda un factor "estético"


datos21 <- datos2[, c(4,6,8,10,12,14,16,18,20)]

#Normalizar datos
datos21 <- scale(datos21)  #quita la primera columna
#datos normalizados
head(datos21,10)

#Realizar pca
#Diagnostico área el pca 
#Si el determinante tiende a cero se puede aplicar
det(cor(datos21))

#Calcular factor de adecuacion muestral Kaiser-Meyer-Olkin
# De 0.5-0.6 mala, 0.6-0.7 adecuado y mayor de 0.7 bueno (MSA)
psych::KMO(datos21)
# Todas las variables poseen una msa mayor a 0.5, por lo que es pértinente el pca.
pca21<-princomp(datos21)
pca21$loadings

#diagnostico
summary(pca21)
#Hasta el momento se observa que los principales componentes que aportan mayor varianza 
# son 1 y 2.

#Revisar varianza y eigenvalores
fviz_eig(pca21, choice = 'variance')
#Efectivamente la componente uno y dos aportan l amayor varianza
fviz_eig(pca21, choice = 'eigenvalue')
#Solo dos componentes tienen un eigenvalor mayor a la unidad.
# Lo adecuado es extraer unicamente dos factores.


#ANALISIS GRAFICO
# El COSENO CUADRADO se utiliza para medir la calidad de la representacion de las 
# variables originales en el espacio de los componentes principales.
# Especificamente, el coseno cuadrado de una variable en un componente principal
# es el cudrado del coseno del angulo entre la  variable original y el componente 
# principal

#Grafico de las puntuaciones factoriales y su representacion
fviz_pca_ind(pca21,
             col.ind = 'cos2',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)

# El coseno cuadrado indica que proporcion de la variaza de la variable original 
# es explicada por el componente principal.
#Valores altos de coseno cudrado (cercanos a 1) significan que la variable esta
# bien representada por el componente principal.
# Mientras que valores bajos (cercanos a 0) indican una mala representacion.



#Grafico de las cargas.
#¿Cuanto contribuye cada variable a las diferentes componentes principales?
#Los componetes contribuyen en diferente medida a los cuadrantes dentro de 
# la representacion bidimensional.
fviz_pca_var(pca21,
             col.var = 'contrib',
             gradient.cols = c('red', 'yellow', 'green'),
             repel = T)


x11()
psych::cor.plot(datos21)
#analisis como lo proporciona spss
#se debe notar que todas las variables en un pca deben estar altamente
#correlacionadas para que tenga sentido realizarlo
det(cor(datos21))

#El determinante de la matriz de correlación es muy bajo, por lo que el grado de asociación es muy alto.

#Resultado del pca por factores: 
#la rotación más omún es VARIMAX
#En un PCA, los componentes principales iniciales pueden ser díficiles de interpretar porque cada variable puede tener cargas
#de manera que cada variable tenga una carga alta en un solo componente, haciendo que la estructura sea más simple y cara. 
pca211 <- psych::principal(datos21, nfactors = 2, residuals = FALSE, rotate = "varimax", 
                           scores =TRUE, oblique.scores=FALSE, method="regression", use= "pairwise", cor="cor", weight=NULL)

#Matriz de coeficientes para las puntuaciones de los componentes. 
pca211$weights[,1]
pca211$weights[,2]


#Nuevas variables obtenidas cuya principal caracteristica es que son ortogonales, es decir, linealmente independientes.
pca211$scores
