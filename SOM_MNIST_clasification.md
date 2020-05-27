---
title: "SOM MNIST Clasification"
author: "desareca"
date: "25-05-2020"
output:
  html_document: 
    code_folding: hide
    df_print: default
    highlight: tango
    keep_md: yes
    theme: united
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
---

<style>
.list-group-item.active, .list-group-item.active:focus, .list-group-item.active:hover {
    background-color: #6633FF;
}
</style>



# Resumen

Los mapas autorganizados de Kohonen son un algoritmo que a partir de un proceso iterativo de comparacion con un conjunto de datos y cambios para aproximarse a los mismos, crea un modelo de esos mismos datos que puede servir para agruparlos por criterios de similitud; adicionalmente, este agrupamiento se produce de forma que la proyeccion de estos datos sobre el mapa distribuya sus caracteristicas de una forma gradual. El Mapa de Kohonen, SOM se usa para diferentes aplicaciones:


- Clustering: se pueden agrupar datos del conjunto de entrada, atendiendo a diferentes criterios.

- Visualizacion: este agrupamiento, como se realiza de una forma ordenada, permite visualizar al conjunto de entrada y descubrir caracteristicas nuevas o relaciones que no se habian previsto de antemano. Tambien permite visualizar la evolucion temporal de un conjunto de datos: proyectando un vector en etapas sucesivas sobre un mapa entrenado se ve como se va moviendo de una zona con unas caracteristicas determinadas a otra.

- Clasificacion: aunque el entrenamiento del mapa no tiene en cuenta la etiqueta de clase o el tipo de cada uno de los vectores de entrada, una vez terminado el entrenamiento se puede asignar algun tipo de etiqueta a cada nodo, y se puede usar para clasificar datos desconocidos.

- Interpolacion de una funcion: asignando valores numericos a cada uno de los nodos de la red de Kohonen, se pueden asignar esos valores numericos a los vectores de entrada: a cada vector (dato) de entrada le correspondera el numero o vector asignados a la salida mas cercana.

- Cuantizacion vectorial: corresponde a la aplicacion de una entrada continua a una salida que esta discretizada, obteniendo a partir de un vector cualquiera el vector mas cercano de un conjunto previamente establecido. 


A continuacion se vera la implementacion de mapas autorganizados para la reduccion dimensional, visualizacion de caracteristicas y clasificacion de imagenes.


# Mapas autorganizados de Kohonen

El algoritmo de SOM (Kohonen, 1982), traducido del ingles mapas auto-organizados, es un  modelo  de  redes  neuronales  de  aprendizaje  no  supervisado  competitivo.  Los modelos de redes neuronales se aplican a problemas de reconocimiento de patrones. Estas redes construyen clases a partir de los datos de entrenamiento no etiquetados $(x1, x2,...xN)$ mediante medidas de disimilitud, y tratan de identificar las particiones optimas (en realidad no se puede asegurar que sean las optimas, pero si seran aceptables) del conjunto de datos de entrada. Las  redes  neuronales  son  competitivas  en  cuanto  a que  las  neuronas  compiten  unas con  otras  por  activarse.  Cuando  se  trabaja  con  redes  neuronales  se  pretende  que cuando  se  presenten  los  datos  de  entrada  al modelo,  este  active  una  (o unas  pocas) neuronas de salida,las cuales se denominaran neuronas vencedoras. El proposito de este  aprendizaje  es  clasificar  los  datos que  se  introducen  en  la  red  y que, cuando  se introduzcan  objetos  que  pertenezcan  a  la  misma  categoria,  estos  activen  la  misma neurona de salida; se debe activar una y solo una. Dichas categorias deben ser creadas por la propia red, puesto que se trata de aprendizaje no supervisado. El objetivo de SOM es representar conjuntos de datos multidimensionales en una red de  menores  dimensiones,  habitualmente  en  un  espacio  bidimensional,  de  forma que dichos datos conserven la topologia inicial; es decir, que aquellos que son proximos en el espacio multidimensional, deben mantenerse proximos en el mapa bidimensional. Las topologias mas frecuentes son la rectangular y la hexagonal; en este trabajo se utiliza la rectangular. 


Un modelo de SOM esta compuesto por dos capas de neuronas. Por un lado, la capa de entrada, formada por $N$ neuronas (una neurona por cada dato de entrada), que se encarga de recibir y transmitir a la capa de salida la informacion procedente del exterior. Por otro lado, la  capa  de  salida,  formada  por $M$  neuronas,  que  es  la  encargada  de procesar   la   informacion,   crear   patrones   e   identificar   las   posibles   categorias. Normalmente, las neuronas de la capa de salida se organizan en un mapa bidimensional como se ha mencionado en el parrafo anterior, tal y como se muestra en la figura.
 
<br> </br>
<center><img src="SOM_MNIST_clasification_files/figure-html/som_example.png"></center>
<br> </br>
<center> Fig 1. Representacion de mapa autorganizado (SOM)</center>
<br> </br>


La transmision entre las dos capas que forman la red es siempre hacia adelante; en otras palabras, la informacion se propaga siempre desde la capa de entrada hacia la capa de salida. Cada neurona de entrada $i$ esta conectada a cada una de las neuronas de salida $j$ mediante un peso $w_{ji}$. De esta forma, las neuronas de  salida  tienen  asociado  un  vector  de  pesos  $w_{j}$,llamado  vector  de  referencia  o *codebook*. Este vector es el vector promedio de la categoria representada por la neurona $j$.

El algoritmo de SOM se divide en cinco etapas:

1. En  la  inicializacion  se  le  asigna  a  cada  uno  de  los  nodos  un  vector  de  pesos aleatorio $w_{j}$.

2. En la segunda etapa, o etapa de competicion, se selecciona, para cada dato de entrada $x_i$,el nodo $j$ al cual es mas proximo en terminos de similitud. Para ello se calcula la distancia euclidea del dato $x_i$ a cada uno de los vectores del *codebook*, y se elige aquella neurona a la cual esta distancia sea minima. A dicha neurona $j$ se le denomina neurona vencedora. 


$$j = argmin \Arrowvert x_i - w_j \Arrowvert^2 \quad\quad (1\leq j\leq M)$$


3. La siguiente etapa es la fase de cooperacion. Una vez terminada la etapa *2*, se vecinos a aquellos nodos $w_k$ cuya distancia a $w_j$ es minima; la funcion que elaciona dicha distancia se llama *tasa de vecindad* ($h=h(l_i-l_k)$). Esta funcion asigna mas o menos peso a los nodos vecinos en funcion de la distancia: cuanto mas proximo, mayor peso y viceversa ($h \in (0, 1)$).


Por otro lado, se define la *tasa de aprendizaje* $\alpha$. Esta depende del numero de iteraciones que se especifican previamente en el argumento de la funcion **SOM** de forma que en cada iteracion, $\alpha$ decrece linealmente desde $1$ hasta $0$ ($\alpha \in (0, 1)$).


4. En  esta etapa  se  actualizan  los  vectores  de  pesos  de  los  nodos  vecinos, conocida como etapa de adaptacion:

$$w_k = w_k + \alpha h(l_i-l_k)(x_i-w_k)$$


5. Se repiten las etapas 2, 3 y 4 hasta que se verifique alguno de los criterios de parada. Dichos criterios de parada pueden ser, bien que se alcance el numero maximo de iteraciones, o que tras varias iteraciones el cambio de vectores de peso no sea significativo.


# Aplicacion I: dataset MNIST
## Carga de datos

MNIST (Instituto Nacional Modificado de Estandares y Tecnologia) es el conjunto de datos de facto de *vision mundial* de la vision de computadora. Desde su lanzamiento en 1999, este clasico conjunto de dato de imagenes manuscritas ha servido como base para los algoritmos de clasificacion de referencia. A medida que surgen nuevas tecnicas de aprendizaje automatico, MNIST sigue siendo un recurso confiable para investigadores y estudiantes por igual.

El conjunto de datos mixto de Instituto Nacional de estandares y tecnologia (MNIST) es una coleccion de 70.000 imagenes de digitos escritos a mano. Los datos fue creados para actuar como un referente para los algoritmos de reconocimiento de imagen. 

Las imagenes son de 28 x 28 pixeles y cuenta con 10 clases posibles, digitos del cero al nueve.



```r
train <- read.csv("train.csv")
train$label <- factor(train$label)
train[,c(2:785)] <- round(train[,c(2:785)], digits = 0)

l <- 1
for (i in 1:10) {
      for (k in 1:25) {
            if(k==1){b <- matrix(unlist(train[l,-1])/255, ncol = 28, nrow = 28)}
            if(k>1){
                  a <- matrix(unlist(train[l,-1])/255, ncol = 28, nrow = 28)
                  b <- rbind(b,a) 
            }
            l <- l+1
      }
      if(i==1){
            c <- b
            remove(b)
      }
      if(i>1){
            c <- cbind(c,b)
            remove(b)
      }
}

colors <- colorRampPalette(c("#6633FF","#FFAA44","#BBFF00"))

plot(colormap(c, palette = colors(256)))
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/load-1.png" alt="Fig 2. Primeros 250 digitos escritos a mano"  />
<p class="caption">Fig 2. Primeros 250 digitos escritos a mano</p>
</div>

```r
# title("Primeros 250 digitos escritos a mano")
```

Al observar el rango de las imagenes tenemos que varia entre 0 y 255, considerando que las redes neuronales normalmente operan bien en rango (0, 1), debemos normalizar los datos. 

Luego, utilizaremos la libreria ***caret*** para dividir el data set en conjunto de entrenamiento y pruebas.


```r
# train[,-1] <- train[,-1]/255

set.seed(100)
Index <- createDataPartition(y = train$label, p = 0.7, list = FALSE)

trainingdata <- list(measurements = as.matrix(train[Index,-1]), 
                     target = as.matrix(train[Index,1]))
testingdata <- list(measurements = as.matrix(train[-Index,-1]), 
                    target = as.matrix(train[-Index,1]))

tb <- data.frame(Muestras = c(nrow(trainingdata$measurements), nrow(testingdata$measurements)))
tb = tb %>% mutate(Porcentaje = round(Muestras*100/sum(Muestras),2))
rownames(tb) <- c("trainset", "testset")
htmlTable(tb,
          caption = "Tabla 1. Conjuntos de entrenamiento y prueba.",
          col.rgroup = c("none","#9999F7"))#6633FF F7F7F7
```

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr><td colspan='3' style='text-align: left;'>
Tabla 1. Conjuntos de entrenamiento y prueba.</td></tr>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey;'> </th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Muestras</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Porcentaje</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: left;'>trainset</td>
<td style='text-align: center;'>29404</td>
<td style='text-align: center;'>70.01</td>
</tr>
<tr style='background-color: #9999f7;'>
<td style='background-color: #9999f7; border-bottom: 2px solid grey; text-align: left;'>testset</td>
<td style='background-color: #9999f7; border-bottom: 2px solid grey; text-align: center;'>12596</td>
<td style='background-color: #9999f7; border-bottom: 2px solid grey; text-align: center;'>29.99</td>
</tr>
</tbody>
</table>


## Entrenamiento del SOM

Con el conjunto de entrenamiento definido vamos a entrenar los SOM, para ello utilizaremos la libreria ***kohonen***, definiendolos siguientes parametros:



- Numero de epocas: 100
- Alpha: entre 0.7 y 0.01
- Radio: 7
- Grilla: 10 x 10
- Topologia: Rectangular

De lo anterior, tenemos que el numero de epocas es la cantidad de veces que pasa el conjunto de entrenamiento por el algoritmo. Alpha es la tasa de aprendizaje que comienza en 0.7 al inicio y va decreciendo hasta finalizar con 0.01. El radio corresponde a la vecindad que se considera al momento de actualizar los pesos, comienza con radio 0 y va aumentando hasta 7 al finalizar.

La grilla es la disposicion de la capa de salida, en este caso de 10x10 en topologia rectangular. Esto hace que reduzcamos en un 87.24% la cantidad de datos de cada imagen.



```r
data.SOM <- supersom(trainingdata$measurements, rlen = rlen, alpha = alpha, mode = "pbatch",
                     normalizeDataLayers = FALSE, radius = radius,
                     grid = somgrid(xdim =  dim, ydim =  dim, topo = "rectangular"))

plot(data.SOM, type = "changes", col = "#6633FF", shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/som-1.png" alt="Fig 3. Entrenamiento SOM durante 100 iteraciones."  />
<p class="caption">Fig 3. Entrenamiento SOM durante 100 iteraciones.</p>
</div>

A continuacion se muestra el codebook del modelo, que refleja como influye cada uno de los 784 pixeles a cada una de las neuronas de salida. Se observa que neuronas cercanas tienden a tener distribuciones similares.



```r
plot(data.SOM, type = "codes", codeRendering = "stars", bgcol = colors(300)[seq.int(1,300,3)], shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/codebook-1.png" alt="Fig 4. Codebook del modelo entrenado."  />
<p class="caption">Fig 4. Codebook del modelo entrenado.</p>
</div>



El siguiente grafico muestra el conteo de observaciones mapeadas a cada neurona, esto influye en la capacidad para distinguir entre distintos tipos de observaciones, si hay sectores con muchas conteos de observaciones quiere decir que un gran numero de obsercaciones presentan las mismas caracteristicas, esto podria ser negativo en su caso extremo, ya que si un sector detecta la mayoria de las observaciones no entregaria informacion util para diferenciar clases. Lo mismo ocurre para neuronas sin observaciones (color gris) que no entregarian informacion, ya que su peso seria 0.

Este no es el caso, aunque hay valores maximos, se observan variaciones de color en la figura, lo que representa que hay cierta variabilidad en los datos.




```r
plot(data.SOM, type = "counts", palette.name = colors, heatkey = TRUE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somCount-1.png" alt="Fig 5. Conteo de observaciones mapeadas por cada neurona."  />
<p class="caption">Fig 5. Conteo de observaciones mapeadas por cada neurona.</p>
</div>


El grafico de distancia entre neuronas vecinas es util para visualizar posibles fronteras entre zonas y tener una idea de donde se agruparian distintos grupos. En la figura se observan por lo menos 3 zonas donde se podian agrupar caracteristica, 2 en los extremos de tono mas oscuro y una separando en la diagonal de tono mas claro.



```r
plot(data.SOM, type = "dist.neighbours", palette.name = colors, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somNeighbours-1.png" alt="Fig 6. Distancia entre neuronas vecinas."  />
<p class="caption">Fig 6. Distancia entre neuronas vecinas.</p>
</div>


La calidad presente en el modelo se puede representar utilizando las distancia de las observaciones al codebook final, mientras menor distancia mejor representacion de las observaciones.



```r
plot(data.SOM, type = "quality", palette.name = colors, heatkey = TRUE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somQuality-1.png" alt="Fig 7. Distancia entre neuronas vecinas."  />
<p class="caption">Fig 7. Distancia entre neuronas vecinas.</p>
</div>


## Representacion de las clases en el modelo

A continuacion revisaremos como quedan distribuidas las clases (cada numero) en el mapa. Para ello consideraremos el porcentaje de ocurrencia de cada clase por neurona.

Lo anterior se realiza, primero definiendo cual es la neurona del mapa con el mayor valor por observacion, por ejemplo, para la primera observacion la neurona con mayor valor es la 92, por lo que a esta neurona se le asigna el valor del target que en este caso es el numero 1, asi para todas las observaciones. El resultad se puede observar en la siguiente figura.




```r
som.prediction <- predict(data.SOM, newdata = trainingdata$measurements)
dist <- table(trainingdata$target, som.prediction$unit.classif)
plot(dist, col = "#6633FF", main = "Numeros")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredTable-1.png" alt="Fig 8. Distribucion de clases por neurona."  />
<p class="caption">Fig 8. Distribucion de clases por neurona.</p>
</div>


Se observa que cada clase activa neuronas especificas, abora vamos a ver si estas distribuciones estan relacionadas o no y por lo tanto si el mapa es util para extraer caracteristicas de cada clase.

Al realizar la correlacion entre los datos tenemos que los datos generados por el modelo para cada clase no estan correlacionados, lo que es muy util para clasificar.




```r
dist <- dist %>% apply(2, function(x){x/sum(x)})
corDist <- cor(dist %>% t())
row.names(corDist) <- paste0("N", row.names(corDist))
colnames(corDist) <- paste0("N", colnames(corDist))
corDist %>% 
   ggcorrplot(lab = TRUE, 
              colors = colorRampPalette(c("#BBFF00","#6633FF","#BBFF00"))(3),
              legend.title = "Correlation")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredCor-1.png" alt="Fig 9. Matriz de correlacion entre representacion de cada numero."  />
<p class="caption">Fig 9. Matriz de correlacion entre representacion de cada numero.</p>
</div>

Ahora observando las zonas del mapa asociada a cada clase tenemos que cada clase se ubica en zonas relativamente diferentes del resto de las clases, esto indica que el modelo separa corectamente las caracteristicas que diferencia a cada clase.


```r
par(mfrow = c(2,5))

for (j in 1:10) {
  plot(data.SOM, type = "property", property = dist[j,], main=paste0("Number ", j-1 ),
       palette.name = colors, labels = TRUE, shape = "straight")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRep-1.png" alt="Fig 10. Zonas del mapa asociadas a cada clase."  />
<p class="caption">Fig 10. Zonas del mapa asociadas a cada clase.</p>
</div>


## Comparacion de modelos de clasificacion

Acontinuacion comenzaremos con realizar una clasificacion utilizando el SOM entrenado anteriormente.



```r
testSOM <- predict(data.SOM, newdata = testingdata$measurements)

testSOMvalue <- testSOM$predictions[[1]] %*% t(testSOM$unit.predictions[[1]])

numberPred <- testSOM$unit.classif %>% sapply(function(x) {which.max(dist[,x])-1})

cm <- confusionMatrix(data = as.factor(numberPred), 
                reference = as.factor(testingdata$target))
cm
```

```
Confusion Matrix and Statistics

          Reference
Prediction    0    1    2    3    4    5    6    7    8    9
         0 1181    0    7    0    1    5   13    1    7    5
         1    0 1379   15    3   10    3    5   21   19    6
         2    1    6 1118   14    2    1    0   10    8    1
         3    4    2   18 1136    0  109    1    2  120   21
         4    1    2    8    1  878    9    0   20    7  193
         5   21    8   46   83   24  966   32   28  106   19
         6   23    3    7    2   15   18 1185    0    4    1
         7    0    2   10   15   22    0    0 1120    6  130
         8    5    1   21   43    2   11    1    3  921    7
         9    3    2    3    8  267   16    4  115   20  873

Overall Statistics
                                          
               Accuracy : 0.854           
                 95% CI : (0.8477, 0.8601)
    No Information Rate : 0.1115          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8377          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity           0.95319   0.9815  0.89226  0.87050  0.71908  0.84886
Specificity           0.99657   0.9927  0.99621  0.97547  0.97881  0.96797
Pos Pred Value        0.96803   0.9439  0.96296  0.80396  0.78463  0.72468
Neg Pred Value        0.99490   0.9977  0.98819  0.98489  0.97011  0.98473
Prevalence            0.09836   0.1115  0.09948  0.10360  0.09694  0.09035
Detection Rate        0.09376   0.1095  0.08876  0.09019  0.06970  0.07669
Detection Prevalence  0.09686   0.1160  0.09217  0.11218  0.08884  0.10583
Balanced Accuracy     0.97488   0.9871  0.94423  0.92298  0.84895  0.90841
                     Class: 6 Class: 7 Class: 8 Class: 9
Sensitivity           0.95488  0.84848  0.75616  0.69506
Specificity           0.99357  0.98359  0.99174  0.96138
Pos Pred Value        0.94197  0.85824  0.90739  0.66590
Neg Pred Value        0.99506  0.98229  0.97435  0.96606
Prevalence            0.09852  0.10480  0.09670  0.09971
Detection Rate        0.09408  0.08892  0.07312  0.06931
Detection Prevalence  0.09987  0.10360  0.08058  0.10408
Balanced Accuracy     0.97422  0.91604  0.87395  0.82822
```

La clasificacion entrega un valor de *accuracy* igual a 0.854 y un valor de *kappa* igual a 0.8377 lo que es bastante bueno para ser un modelo no supervisado.




