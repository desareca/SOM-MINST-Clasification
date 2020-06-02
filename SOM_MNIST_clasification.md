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
    background-color: #6A3D9A;
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

pegaImag <- function(img, nrow=25, ncol=10, colExcl = 1, dimImg = c(28, 28), random = FALSE, seed = 0){
   if(random){img <- img[sample(c(1:nrow(img)), nrow(img)),]}
   l <- 1
   for (i in 1:ncol) {
      for (k in 1:nrow) {
         if(k==1){b <- matrix(unlist(img[l,-colExcl])/255, ncol = dimImg[2], nrow = dimImg[1])}
         if(k>1){
            a <- matrix(unlist(img[l,-colExcl])/255, ncol = dimImg[2], nrow = dimImg[1])
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
   return(c)
}

c <- pegaImag(train, nrow = 25, ncol = 10, dimImg = c(28, 28), random = FALSE)
colors <- colorRampPalette(brewer.pal(n = 10, name = "Paired")[c(10,8,1)])
plot(colormap(c, palette = colors(256)))
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/load-1.png" alt="Fig 2. Primeros 250 digitos escritos a mano"  />
<p class="caption">Fig 2. Primeros 250 digitos escritos a mano</p>
</div>

Al observar el rango de las imagenes tenemos que varia entre 0 y 255, por lo que es necesario normalizar los datos para que la red neuronal opere correctamente. Ademas, como se observa en la imagen de muestra, mucho valores (orillas de los numeros) presentan el mismo valor, lo que no entrega informacion util al modelo y puede provocar problemas de colinealidad, para ello eliminaremos los pixeles con varianza cercana a cero. 

Luego, utilizaremos dividiremos el data set en conjunto de entrenamiento y pruebas. Todo lo anterior utilizando la libreria ***caret***


```r
set.seed(100)
Index <- createDataPartition(y = train$label, p = 0.7, list = FALSE)

preProc <- preProcess(as.matrix(train[Index,-1]), method = c("center", "scale", "nzv"))
trainP <- predict(preProc, as.matrix(train[Index,-1]))
testP <- predict(preProc, as.matrix(train[-Index,-1]))

trainingdata <- list(measurements = trainP, 
                     target = as.matrix(train[Index,1]))
testingdata <- list(measurements = testP, 
                    target = as.matrix(train[-Index,1]))

tb <- data.frame(Muestras = c(nrow(trainingdata$measurements), nrow(testingdata$measurements)))
tb = tb %>% mutate(Porcentaje = round(Muestras*100/sum(Muestras),2))
rownames(tb) <- c("trainset", "testset")
htmlTable(tb,
          caption = "Tabla 1. Conjuntos de entrenamiento y prueba.",
          col.rgroup = c("none",colors(3)[3]))
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
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: left;'>testset</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>12596</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>29.99</td>
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
set.seed(0)
data.SOM <- supersom(trainingdata$measurements, rlen = rlen, alpha = alpha, mode = "pbatch",
                     normalizeDataLayers = FALSE, radius = radius,
                     grid = somgrid(xdim =  dim, ydim =  dim, topo = "rectangular"))

plot(data.SOM, type = "changes", col = "#6A3D9A", shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/som-1.png" alt="Fig 3. Entrenamiento SOM durante 100 iteraciones."  />
<p class="caption">Fig 3. Entrenamiento SOM durante 100 iteraciones.</p>
</div>

A continuacion se muestra el codebook del modelo, que refleja como influye cada uno de los 784 pixeles a cada una de las neuronas de salida. Se observa que neuronas cercanas tienden a tener distribuciones similares.



```r
plot(data.SOM, type = "codes", codeRendering = "stars", bgcol = colors(10)[10], shape = "straight")
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

## Clustering del SOM


Los SOM son redes neuronales no supervisadas, por lo que no es posible realizar una clasificacion directa sin encontrar las zonas asociadas a las distintas clases. Para hacernos una idea de donde estan distribuidos los numeros en el mapa vamos a realizar clustering.

En la primera fila de imagenes tenemos los clusters considerando 4 metodos de aglomeracion comunes (para mayor informacion ver [clustering jerarquico en R](https://rpubs.com/mjimcua/clustering-jerarquico-en-r)).

Los clusters se realizan sobre el *codebook*, ya que en este se encuentra la informacion de los datos.



```r
# metodos disponibles
method <- c("ward.D", "complete", "average", "mcquitty")
som_cluster <- list()
tabCl <- list()
cl <- 10
for (j in 1:4) {
      som_cluster[[j]] <- cutree(hclust(dist(data.SOM$codes[[1]]), method = method[j]), k = cl)
      tabCl[[j]] <- table(target = trainingdata$target, cluster = som_cluster[[j]][data.SOM$unit.classif])
}
# grafico cluster en som
colors2 <- colorRampPalette(brewer.pal(n = 10, name = "Paired"))
par(mfrow = c(4,2))
for (j in 1:4) {
  plot(data.SOM, type="mapping", bgcol = colors2(cl)[som_cluster[[j]]],
     main = paste0("Clusters (", method[j],")"), 
     shape = "straight", border = NULL, 
     labels = ".")
  add.cluster.boundaries(data.SOM, som_cluster[[j]], col = "white", lwd = 3)
  plot(tabCl[[j]], col = colors2(cl), main = "")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somClusterGral-1.png" alt="Fig 8. Cluster con distintos metodos de alglomeracion."  />
<p class="caption">Fig 8. Cluster con distintos metodos de alglomeracion.</p>
</div>

***Falta detalle de los clusters, se realiza una vez finalizado todo.***




## Representacion de las clases en el modelo y clasificacion

A continuacion revisaremos como quedan distribuidas las clases (cada numero) en el mapa. Para ello consideraremos el porcentaje de ocurrencia de cada clase por neurona.

Lo anterior se realiza, primero definiendo cual es la neurona del mapa con el mayor valor por observacion, por ejemplo, para la primera observacion la neurona con mayor valor es la 92, por lo que a esta neurona se le asigna el valor del target que en este caso es el numero 1, asi para todas las observaciones. El resultad se puede observar en la siguiente figura.



```r
som.prediction <- predict(data.SOM, newdata = trainingdata$measurements)
dist <- table(trainingdata$target, som.prediction$unit.classif)
plot(dist, col = colors(3)[2], main = "Numeros")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredTable-1.png" alt="Fig 9. Distribucion de clases por neurona."  />
<p class="caption">Fig 9. Distribucion de clases por neurona.</p>
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
              colors = colorRampPalette(c(colors(3)[3], colors(3)[1], colors(3)[3]))(3),
              legend.title = "Correlation")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredCor-1.png" alt="Fig 10. Matriz de correlacion entre representacion de cada numero."  />
<p class="caption">Fig 10. Matriz de correlacion entre representacion de cada numero.</p>
</div>

Ahora observando las zonas del mapa asociada a cada clase tenemos que cada clase se ubica en zonas relativamente diferentes del resto de las clases, esto indica que el modelo separa correctamente las caracteristicas que diferencia a cada clase.


```r
par(mfrow = c(2,5))

for (j in 1:10) {
  plot(data.SOM, type = "property", property = dist[j,], main=paste0("Numero ", j-1, "\n" ),
       palette.name = colors, heatkey = FALSE, shape = "straight")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRep-1.png" alt="Fig 11. Zonas del mapa asociadas a cada clase."  />
<p class="caption">Fig 11. Zonas del mapa asociadas a cada clase.</p>
</div>

Si observamos (figura de mas abajo) la desviacion estandar de cada neurona por clase tenemos algunas zonas conflictivas.

Para entender la siguiente figura es necesario tener presente que el color morado representa una desviacion maxima y a medida que se acerca al verde (pasando por naranjo) se acerca a 0.
Las neuronas con menor desviacion por clase se muestran en tonos naranjo y verdes, estas representan zonas difusas donde se ubican caracteristicas mas generales.

En este caso particular als zonas con baja desviacion estan asociadas por un lado una zona grande del 9 y el 4, donde hasta un ser humano podria tener problemas e diferenciar; por otro en una zona menor asociada al 3, 5 y 8, numeros que tabien podrian llegar a ser confundidos.



```r
distSd <- dist %>% apply(2, sd)

par(mfrow = c(1,1))
plot(data.SOM, type = "property", property = distSd, main=paste0(""),
     palette.name = colorRampPalette(c(colors(3)[3], colors(3)[2], colors(3)[1])), heatkey = FALSE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRepSd-1.png" alt="Fig 12. Desviacion de representacion de clases por neurona.
"  />
<p class="caption">Fig 12. Desviacion de representacion de clases por neurona.
</p>
</div>



Acontinuacion comenzaremos con realizar una clasificacion utilizando el SOM entrenado anteriormente.

Como se observa en la matriz de confusion, los numeros 4 y 9 presentan problemas



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
         0 1166    0   11    3    0    7   20    3    5    7
         1    0 1345   11    2    8    3    1   14    8    6
         2   21    8 1147   41   29   18   28   24   26   17
         3    9   14   19 1057   12   87    6   18  120   31
         4    0    2    2    3  888   15    0   40    4  256
         5   11    1    1  137    0  976   12    4   97    6
         6   27    4   14    5   19   13 1172    0   10    2
         7    2   12   21   14    9    2    0 1108    9   29
         8    2   16   23   37    7    7    2   10  915   15
         9    1    3    4    6  249   10    0   99   24  887

Overall Statistics
                                        
               Accuracy : 0.8464        
                 95% CI : (0.84, 0.8526)
    No Information Rate : 0.1115        
    P-Value [Acc > NIR] : < 2.2e-16     
                                        
                  Kappa : 0.8293        
                                        
 Mcnemar's Test P-Value : NA            

Statistics by Class:

                     Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity           0.94108   0.9573  0.91540  0.80996  0.72727  0.85764
Specificity           0.99507   0.9953  0.98131  0.97201  0.97169  0.97652
Pos Pred Value        0.95417   0.9621  0.84400  0.76985  0.73388  0.78394
Neg Pred Value        0.99358   0.9946  0.99057  0.97790  0.97075  0.98573
Prevalence            0.09836   0.1115  0.09948  0.10360  0.09694  0.09035
Detection Rate        0.09257   0.1068  0.09106  0.08392  0.07050  0.07748
Detection Prevalence  0.09701   0.1110  0.10789  0.10900  0.09606  0.09884
Balanced Accuracy     0.96808   0.9763  0.94836  0.89099  0.84948  0.91708
                     Class: 6 Class: 7 Class: 8 Class: 9
Sensitivity           0.94440  0.83939  0.75123  0.70621
Specificity           0.99172  0.99131  0.98954  0.96508
Pos Pred Value        0.92575  0.91874  0.88491  0.69135
Neg Pred Value        0.99391  0.98139  0.97379  0.96738
Prevalence            0.09852  0.10480  0.09670  0.09971
Detection Rate        0.09305  0.08796  0.07264  0.07042
Detection Prevalence  0.10051  0.09574  0.08209  0.10186
Balanced Accuracy     0.96806  0.91535  0.87039  0.83564
```

La clasificacion entrega un valor de *accuracy* igual a 0.8464 y un valor de *kappa* igual a 0.8293 lo que es bastante bueno para ser un modelo no supervisado.





## Representacion y clasificacion de numeros pares

Como los mapas autoorganizados mapean las caracteristicas de los datos de manera no supervisada, es posible utilizar el modelo para agrupar de distintas maneras, en este caso utilizaremos el modelo ya entrenado para visualizar y clasificar numeros pares e impares.

Para ello primero debemos redefinir el *target*.



```r
label <- train$label %>% as.character() %>% as.numeric()
label2 <- 1 - (label %% 2)

tb <- data.frame(Target = head(label,10), TargetNew = head(label2,10))
htmlTable(tb,
          caption = "Tabla 2. Equivalencia numero con paridad.",
          col.rgroup = c("none",colors(3)[3]))
```

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr><td colspan='3' style='text-align: left;'>
Tabla 2. Equivalencia numero con paridad.</td></tr>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey;'> </th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Target</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>TargetNew</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: left;'>1</td>
<td style='text-align: center;'>1</td>
<td style='text-align: center;'>0</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; text-align: left;'>2</td>
<td style='background-color: #a6cee3; text-align: center;'>0</td>
<td style='background-color: #a6cee3; text-align: center;'>1</td>
</tr>
<tr>
<td style='text-align: left;'>3</td>
<td style='text-align: center;'>1</td>
<td style='text-align: center;'>0</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; text-align: left;'>4</td>
<td style='background-color: #a6cee3; text-align: center;'>4</td>
<td style='background-color: #a6cee3; text-align: center;'>1</td>
</tr>
<tr>
<td style='text-align: left;'>5</td>
<td style='text-align: center;'>0</td>
<td style='text-align: center;'>1</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; text-align: left;'>6</td>
<td style='background-color: #a6cee3; text-align: center;'>0</td>
<td style='background-color: #a6cee3; text-align: center;'>1</td>
</tr>
<tr>
<td style='text-align: left;'>7</td>
<td style='text-align: center;'>7</td>
<td style='text-align: center;'>0</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; text-align: left;'>8</td>
<td style='background-color: #a6cee3; text-align: center;'>3</td>
<td style='background-color: #a6cee3; text-align: center;'>0</td>
</tr>
<tr>
<td style='text-align: left;'>9</td>
<td style='text-align: center;'>5</td>
<td style='text-align: center;'>0</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: left;'>10</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>3</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>0</td>
</tr>
</tbody>
</table>


Ahora veremos como quedan distribuidas cada clase nueva por neurona.




```r
som.prediction <- predict(data.SOM, newdata = trainingdata$measurements)
dist2 <- table(label2[Index], som.prediction$unit.classif)
dist2 <- dist2 %>% apply(2, function(x){x/sum(x)})

par(mfrow = c(1,2))
tParc = c("Impar", "Par")
for (j in 1:2) {
  plot(data.SOM, type = "property", property = dist2[j,], main=paste0(tParc[j], "\n" ),
       palette.name = colors, heatkey = FALSE, shape = "straight")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRepPar-1.png" alt="Fig 13. Zonas del mapa asociadas a cada clase."  />
<p class="caption">Fig 13. Zonas del mapa asociadas a cada clase.</p>
</div>





```r
dist2Sd <- dist2 %>% apply(2, sd)

par(mfrow = c(1,1))
plot(data.SOM, type = "property", property = dist2Sd, main=paste0(""),
     palette.name = colorRampPalette(c(colors(3)[3], colors(3)[2], colors(3)[1])), 
     heatkey = FALSE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredPSd-1.png" alt="Fig 14. Desviacion de representacion de clases por neurona."  />
<p class="caption">Fig 14. Desviacion de representacion de clases por neurona.</p>
</div>



```r
ParPred <- testSOM$unit.classif %>% sapply(function(x) {which.max(dist2[,x])-1})

cm2 <- confusionMatrix(data = as.factor(ParPred), reference = as.factor(label2[-Index]))
cm2
```

```
Confusion Matrix and Statistics

          Reference
Prediction    0    1
         0 5934  701
         1  490 5471
                                          
               Accuracy : 0.9054          
                 95% CI : (0.9002, 0.9105)
    No Information Rate : 0.51            
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.8107          
                                          
 Mcnemar's Test P-Value : 1.165e-09       
                                          
            Sensitivity : 0.9237          
            Specificity : 0.8864          
         Pos Pred Value : 0.8943          
         Neg Pred Value : 0.9178          
             Prevalence : 0.5100          
         Detection Rate : 0.4711          
   Detection Prevalence : 0.5268          
      Balanced Accuracy : 0.9051          
                                          
       'Positive' Class : 0               
                                          
```



# Aplicacion II: dataset Face Expression
## Carga de datos



```r
dir = "faceExpressionDatabase/"
listImage <- list.files(path = dir, pattern = ".bmp$")

faceExp <- listImage %>% 
      sapply(function(x){cbind(label = x %>% substr(1,1), 
                               rotate(read.bitmap(paste0(dir, x),".bmp"), angle = 90) %>% 
                                     matrix(nrow = 1))}) %>% t()

faceExp <- data.frame(faceExp, stringsAsFactors = FALSE, row.names = NULL)
colnames(faceExp)[1] <- "label"
colnames(faceExp)[2:ncol(faceExp)] <- paste0("pixel", c(2:ncol(faceExp))-2)
faceExp[,2:ncol(faceExp)] <- faceExp[,2:ncol(faceExp)] %>% sapply(as.numeric)

idxFE <- seq.int(1,60,12) %>% sapply(function(x){x+75*c(0:12)}) %>% matrix(ncol = 1)

cFE <- pegaImag(faceExp[idxFE,], nrow = 13, ncol = 5, dimImg = c(64, 64), random = FALSE)
plot(Image(cFE))
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/loadFE-1.png" alt="Fig 15. Muestras aleatorias (50) del dataset face expression"  />
<p class="caption">Fig 15. Muestras aleatorias (50) del dataset face expression</p>
</div>









```r
set.seed(100)
Index <- createDataPartition(y = faceExp$label, p = 0.7, list = FALSE)

preProc <- preProcess(as.matrix(faceExp[Index,-1]), method = c("center", "scale", "nzv"))
trainP <- predict(preProc, as.matrix(faceExp[Index,-1]))
testP <- predict(preProc, as.matrix(faceExp[-Index,-1]))


trainingdata <- list(measurements = trainP, 
                     target = as.matrix(faceExp[Index,1]))
testingdata <- list(measurements = testP, 
                    target = as.matrix(faceExp[-Index,1]))

tb <- data.frame(Muestras = c(nrow(trainingdata$measurements), nrow(testingdata$measurements)))
tb = tb %>% mutate(Porcentaje = round(Muestras*100/sum(Muestras),2))
rownames(tb) <- c("trainset", "testset")
htmlTable(tb,
          caption = "Tabla 3. Conjuntos de entrenamiento y prueba.",
          col.rgroup = c("none",colors(3)[3]))
```

<table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;' >
<thead>
<tr><td colspan='3' style='text-align: left;'>
Tabla 3. Conjuntos de entrenamiento y prueba.</td></tr>
<tr>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey;'> </th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Muestras</th>
<th style='border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;'>Porcentaje</th>
</tr>
</thead>
<tbody>
<tr>
<td style='text-align: left;'>trainset</td>
<td style='text-align: center;'>689</td>
<td style='text-align: center;'>70.67</td>
</tr>
<tr style='background-color: #a6cee3;'>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: left;'>testset</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>286</td>
<td style='background-color: #a6cee3; border-bottom: 2px solid grey; text-align: center;'>29.33</td>
</tr>
</tbody>
</table>












```r
set.seed(0)
data.SOM <- supersom(trainingdata$measurements, rlen = rlen, alpha = alpha, mode = "pbatch",
                     normalizeDataLayers = FALSE, radius = radius,
                     grid = somgrid(xdim =  dim, ydim =  dim, topo = "rectangular"))

plot(data.SOM, type = "changes", col = "#6A3D9A", shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somFE-1.png" alt="Fig 16. Entrenamiento SOM durante 1000 iteraciones."  />
<p class="caption">Fig 16. Entrenamiento SOM durante 1000 iteraciones.</p>
</div>




```r
plot(data.SOM, type = "codes", codeRendering = "stars", bgcol = colors(10)[10], shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/codebookFE-1.png" alt="Fig 17. Codebook del modelo entrenado."  />
<p class="caption">Fig 17. Codebook del modelo entrenado.</p>
</div>




```r
plot(data.SOM, type = "counts", palette.name = colors, heatkey = TRUE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somCountFE-1.png" alt="Fig 18. Conteo de observaciones mapeadas por cada neurona."  />
<p class="caption">Fig 18. Conteo de observaciones mapeadas por cada neurona.</p>
</div>






```r
plot(data.SOM, type = "dist.neighbours", palette.name = colors, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somNeighboursFE-1.png" alt="Fig 19. Distancia entre neuronas vecinas."  />
<p class="caption">Fig 19. Distancia entre neuronas vecinas.</p>
</div>







```r
plot(data.SOM, type = "quality", palette.name = colors, heatkey = TRUE, shape = "straight")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somQualityFE-1.png" alt="Fig 20. Distancia entre neuronas vecinas."  />
<p class="caption">Fig 20. Distancia entre neuronas vecinas.</p>
</div>


## Clustering del SOM



```r
# metodos disponibles
method <- c("ward.D", "complete", "average", "mcquitty")
som_cluster <- list()
tabCl <- list()
clFE <- 13
for (j in 1:4) {
      som_cluster[[j]] <- cutree(hclust(dist(data.SOM$codes[[1]]), method = method[j]), k = clFE)
      tabCl[[j]] <- table(target = trainingdata$target, cluster = som_cluster[[j]][data.SOM$unit.classif])
}
# grafico cluster en som
colors2 <- colorRampPalette(brewer.pal(n = 10, name = "Paired"))
par(mfrow = c(4,2))
for (j in 1:4) {
  plot(data.SOM, type="mapping", bgcol = colors2(clFE)[som_cluster[[j]]],
     main = paste0("Clusters (", method[j],")"), 
     shape = "straight", border = NULL, 
     labels = ".")
  add.cluster.boundaries(data.SOM, som_cluster[[j]], col = "white", lwd = 3)
  plot(tabCl[[j]], col = colors2(clFE), main = "")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somClusterGralFE-1.png" alt="Fig 21. Cluster con distintos metodos de alglomeracion."  />
<p class="caption">Fig 21. Cluster con distintos metodos de alglomeracion.</p>
</div>

***Falta detalle de los clusters, se realiza una vez finalizado todo.***

## Representacion de las clases en el modelo y clasificacion




```r
som.prediction <- predict(data.SOM, newdata = trainingdata$measurements)
dist <- table(trainingdata$target, som.prediction$unit.classif)
plot(dist, col = colors(3)[2], main = "Sujetos")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredTableFE-1.png" alt="Fig 22. Distribucion de clases por neurona."  />
<p class="caption">Fig 22. Distribucion de clases por neurona.</p>
</div>






```r
dist <- dist %>% apply(2, function(x){x/sum(x)})
corDist <- cor(dist %>% t())
row.names(corDist) <- paste0("", row.names(corDist))
colnames(corDist) <- paste0("", colnames(corDist))
corDist %>% 
   ggcorrplot(lab = TRUE, 
              colors = colorRampPalette(c(colors(3)[3], colors(3)[1], colors(3)[3]))(3),
              legend.title = "Correlacion")
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredCorFE-1.png" alt="Fig 23. Matriz de correlacion entre representacion de cada numero."  />
<p class="caption">Fig 23. Matriz de correlacion entre representacion de cada numero.</p>
</div>



```r
par(mfrow = c(3,5))

for (j in 1:13) {
  plot(data.SOM, type = "property", property = dist[j,], main=paste0("Subject ", LETTERS[j]),
       palette.name = colorRampPalette(colors(13)), heatkey = FALSE, shape = "straight")
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRepFE-1.png" alt="Fig 24. Zonas del mapa asociadas a cada clase."  />
<p class="caption">Fig 24. Zonas del mapa asociadas a cada clase.</p>
</div>




```r
col0 <- c(1:(dim*dim))[-unique(som.prediction$unit.classif)]
if(length(col0)){
   dist <- cbind(dist, matrix(0, nrow = nrow(dist), ncol = length(col0)))
   colnames(dist)[(ncol(dist) - length(col0) + 1):ncol(dist)] <- col0
}
dist <- dist[,order(as.numeric(colnames(dist)))]

distSd <- dist %>% apply(2, sd)
border <- c(1:(dim*dim)) %>% sapply(function(x){ifelse(sum(x==col0)>0,1,0)})

par(mfrow = c(1,1))
plot(data.SOM, type = "property", property = distSd, main="",
     palette.name = colorRampPalette(c(colors(3)[3], colors(3)[2], colors(3)[1])),
     heatkey = FALSE, shape = "straight")
add.cluster.boundaries(data.SOM, border, col = colors2(24)[14], lwd = 3)
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRepSdFE-1.png" alt="Fig 25. Desviacion de representacion de clases por neurona.
"  />
<p class="caption">Fig 25. Desviacion de representacion de clases por neurona.
</p>
</div>






```r
testSOM <- predict(data.SOM, newdata = testingdata$measurements)

testPredTest <- testSOM$unit.classif %>% sapply(function(x) {which.max(dist[,x])[[1]]})
testPredTest <- LETTERS[testPredTest]

cm <- confusionMatrix(data = as.factor(testPredTest), reference = as.factor(testingdata$target))
cm
```

```
Confusion Matrix and Statistics

          Reference
Prediction  A  B  C  D  E  F  G  H  I  J  K  L  M
         A 22  0  0  0  0  0  0  0  0  0  0  0  0
         B  0 22  0  0  0  0  0  0  0  0  0  0  0
         C  0  0 22  0  0  0  0  0  0  0  0  0  0
         D  0  0  0 22  0  0  0  0  0  0  0  0  0
         E  0  0  0  0 22  0  0  0  0  0  0  0  0
         F  0  0  0  0  0 22  0  0  0  0  0  0  0
         G  0  0  0  0  0  0 22  0  0  0  0  0  0
         H  0  0  0  0  0  0  0 22  0  0  0  0  0
         I  0  0  0  0  0  0  0  0 22  0  0  0  0
         J  0  0  0  0  0  0  0  0  0 22  0  0  0
         K  0  0  0  0  0  0  0  0  0  0 22  0  0
         L  0  0  0  0  0  0  0  0  0  0  0 22  0
         M  0  0  0  0  0  0  0  0  0  0  0  0 22

Overall Statistics
                                     
               Accuracy : 1          
                 95% CI : (0.9872, 1)
    No Information Rate : 0.0769     
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
                                     
 Mcnemar's Test P-Value : NA         

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E Class: F
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
                     Class: G Class: H Class: I Class: J Class: K Class: L
Sensitivity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Specificity           1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Pos Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Neg Pred Value        1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
Prevalence            0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Rate        0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Detection Prevalence  0.07692  0.07692  0.07692  0.07692  0.07692  0.07692
Balanced Accuracy     1.00000  1.00000  1.00000  1.00000  1.00000  1.00000
                     Class: M
Sensitivity           1.00000
Specificity           1.00000
Pos Pred Value        1.00000
Neg Pred Value        1.00000
Prevalence            0.07692
Detection Rate        0.07692
Detection Prevalence  0.07692
Balanced Accuracy     1.00000
```



## Representacion y clasificacion de rostros felices




```r
num2codFace <- function(y){sapply(y, function(x) ifelse(x<10, paste0("0",x), paste0(x)))}

faceHappy <- list("A"=c(3,16:27) %>% num2codFace(),
                  "B"=c(2,4,50:52) %>% num2codFace(),
                  "C"=c(3,52:62) %>% num2codFace(),
                  "D"=c(1,4,14:32,71:74) %>% num2codFace(),
                  "E"=c(1,19:32) %>% num2codFace(),
                  "F"=c(1,4,23:33,67:74) %>% num2codFace(),
                  "G"=c(1,24:40) %>% num2codFace(),
                  "H"=c(1,21:31) %>% num2codFace(),
                  "I"=c(1,26:35) %>% num2codFace(),
                  "J"=c(1,59:72) %>% num2codFace(),
                  "K"=c(3,64:74) %>% num2codFace(),
                  "L"=c(3,52:63) %>% num2codFace(),
                  "M"=c(3,54:63) %>% num2codFace())

label2 <- listImage %>% 
      sapply(function(x){label = sum(faceHappy[[substr(x,1,1)]] == substr(x,2,3))})

cFEhappy <- pegaImag(faceExp[which(label2==1)[sample.int(100,30)],], 
                     nrow = 6, ncol = 5, dimImg = c(64, 64), random = FALSE)
cFEnohappy <- pegaImag(faceExp[which(label2==0)[sample.int(100,30)],], 
                     nrow = 6, ncol = 5, dimImg = c(64, 64), random = FALSE)

par(mfrow = c(1,2))
plot(Image(cFEhappy))
text(labels = "Rostros felices", x = 192, y = -16)
# abline(v = 384, col = colors(3)[1])
segments(x0 = 385, y0 = 3, x1 = 385, y1 = 318, col = colors(3)[1], lwd = 5)
plot(Image(cFEnohappy))
text(labels = "Rostros no felices", x = 192, y = -16)
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/newTargetFEHappy-1.png" alt="Fig 26. Nueva clasificacion de rostros.
"  />
<p class="caption">Fig 26. Nueva clasificacion de rostros.
</p>
</div>


Ahora veremos como quedan distribuidas cada clase nueva por neurona.




```r
som.prediction <- predict(data.SOM, newdata = trainingdata$measurements)
dist2 <- table(label2[Index], som.prediction$unit.classif)
dist2 <- dist2 %>% apply(2, function(x){x/sum(x)})

tParc = c("No feliz", "Feliz")
par(mfrow = c(1,2))
for (j in 1:2) {
  plot(data.SOM, type = "property", property = dist2[j,], main=paste0(tParc[j], "\n" ),
       palette.name = colors, heatkey = FALSE, shape = "straight")
  add.cluster.boundaries(data.SOM, border, col = colors2(24)[14], lwd = 3)
}
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredRepH-1.png" alt="Fig 27. Zonas del mapa asociadas a cada clase."  />
<p class="caption">Fig 27. Zonas del mapa asociadas a cada clase.</p>
</div>





```r
col0 <- c(1:(dim*dim))[-unique(som.prediction$unit.classif)]
if(length(col0)){
   dist2 <- cbind(dist2, matrix(0, nrow = nrow(dist2), ncol = length(col0)))
   colnames(dist2)[(ncol(dist2) - length(col0) + 1):ncol(dist2)] <- col0
}
dist2 <- dist2[,order(as.numeric(colnames(dist2)))]

dist2Sd <- dist2 %>% apply(2, sd)

par(mfrow = c(1,1))
plot(data.SOM, type = "property", property = dist2Sd, main=paste0(""),
     palette.name = colorRampPalette(c(colors(3)[3], colors(3)[2], colors(3)[1])), 
     heatkey = FALSE, shape = "straight")
add.cluster.boundaries(data.SOM, border, col = colors2(24)[14], lwd = 3)
```

<div class="figure" style="text-align: center">
<img src="SOM_MNIST_clasification_files/figure-html/somPredPSdFEHappy-1.png" alt="Fig 28. Desviacion de representacion de clases por neurona."  />
<p class="caption">Fig 28. Desviacion de representacion de clases por neurona.</p>
</div>



```r
testSOM <- predict(data.SOM, newdata = testingdata$measurements)
ExpHappyPred <- testSOM$unit.classif %>% sapply(function(x) {which.max(dist2[,x])-1})

cm2 <- confusionMatrix(data = as.factor(ExpHappyPred), reference = as.factor(label2[-Index]))
cm2
```

```
Confusion Matrix and Statistics

          Reference
Prediction   0   1
         0 227  12
         1   6  41
                                          
               Accuracy : 0.9371          
                 95% CI : (0.9024, 0.9623)
    No Information Rate : 0.8147          
    P-Value [Acc > NIR] : 1.931e-09       
                                          
                  Kappa : 0.782           
                                          
 Mcnemar's Test P-Value : 0.2386          
                                          
            Sensitivity : 0.9742          
            Specificity : 0.7736          
         Pos Pred Value : 0.9498          
         Neg Pred Value : 0.8723          
             Prevalence : 0.8147          
         Detection Rate : 0.7937          
   Detection Prevalence : 0.8357          
      Balanced Accuracy : 0.8739          
                                          
       'Positive' Class : 0               
                                          
```






