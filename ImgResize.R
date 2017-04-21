# This script is used to resize images from 64x64 to 28x28 pixels

# ########################################################################
# Inicialización (setwd() y rm()):
# ########################################################################
setwd(getwd())
setwd('C:/Users/jtoharia/Dropbox/AFI_JOSE/Proyecto Final')
# setwd('C:/Personal/Dropbox/AFI_JOSE/Proyecto Final')
rm(list = ls()) # Borra todos los elementos del entorno de R.
# plot(1) # Forzamos un dibujo para activar la ventana de Plots
# dev.off() # Elimina los gráficos (Plots) [Solo si la ventana de Plots está activa]

# # Load EBImage library:
# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")
# DEMASIADO LENTO LEYENDO Y GUARDANDO:
library(EBImage)
# example("readImage")

# install.packages("jpeg")
library(jpeg)

nombrefichero = "Foto 5-7-15 19 21 29.jpg"
nombrefichero = "Foto 5-7-15 19 21 49.jpg"
nombrefichero = "Foto 26-6-15 19 35 17.jpg"
mi_path_datos = "Datos"
i_bVerFotos = TRUE
i_bGuardarJPG = TRUE
i_bGuardarCSV = TRUE
# m_nMaxObjSize <- 52428800 # 50 * 1024 * 1024 = 50 Mb (aprox. 1500 * 1200 en color) => CSV de aprox. 15 Mb (1M pixels)
m_nMaxObjSize <- 26214400 # 25 Mb (aprox. 750 * 600 en color) => CSV de aprox. 8 Mb (500K pixels)

rm(list = ls()) # Borra todos los elementos del entorno de R.

mi_imgResize <- function(nombrefichero = "Foto 5-7-15 19 21 29.jpg",
                         mi_path_datos = "Datos", mi_path_out = "Datos",
                         i_bVerFotos = FALSE, i_bGuardarJPG = TRUE, i_bGuardarCSV = TRUE, i_bPrint = TRUE, m_nMaxObjSize = 26214400)
{
  # mi_foto <- readJPEG("Datos/Resized_Foto 5-7-15 19 21 29.jpg", native = TRUE)
  # mi_foto2 <- readJPEG("Datos/Resized_Foto 5-7-15 19 21 49.jpg", native = TRUE)
  # str(mi_foto)
  # str(mi_foto2)
  # par(mfrow = c(1,1), mar = c(4,4,2,1))
  # plot(1:2, type='n')
  # rasterImage(mi_foto, 1, 1, 1.5, 1.5)
  # rasterImage(mi_foto2, 1.5, 1.5, 2, 2)
  # # mi_foto <- readImage("Datos/Foto 5-7-15 19 21 29.jpg", native = FALSE)
  # # mi_foto2 <- readImage("Datos/Foto 5-7-15 19 21 49.jpg", native = FALSE)
  # # str(mi_foto)
  # # str(mi_foto2)
  # # par(mfrow = c(1,1), mar = c(4,4,2,1))
  # # plot(1:2, type='n')
  # # rasterImage(mi_foto, 1, 1, 1.5, 1.5)
  # # rasterImage(mi_foto2, 1.5, 1.5, 2, 2)
  # # display(mi_foto2, method = "browser")
  
  # Variable para guardar los tiempos:
  if(!exists('mi_tiempo_total')) mi_tiempo_total <<- vector()
  
  # #################################
  # Leemos la foto:
  # #################################
  mi_fullpathname <- file.path(mi_path_datos, nombrefichero) # <- paste0(mi_path_datos, nombrefichero)
  mi_size <- 0
  # file.info(mi_fullpathname, extra_cols = TRUE)
  try(mi_size <- file.size(mi_fullpathname))
  if(mi_size == 0)
  {
    print(paste0('ERROR: El fichero \'', mi_fullpathname, '\' no se puede leer.'))
    return(-1)
  }
  if(i_bPrint)  print(paste0('Leyendo fichero \'', mi_fullpathname, '\'...'))
  m_sOp <- 'Leyendo fichero...'
  if(i_bPrint)  print(m_sOp)
  mi_tiempo <- system.time(
    mi_foto <- readJPEG(mi_fullpathname, native = FALSE)
  )
  if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
  mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  # #################################
  # Reducimos el tamaño de la foto:
  # #################################
  reducc <- as.integer(object.size(x = mi_foto)) / m_nMaxObjSize # 50 Mb (aprox. 1500 * 1200 en color)
  # if(colorMode(mi_foto) == Color)
  if(length(dim(mi_foto)) > 2)
    reducc <- reducc / (5/3)
  if(reducc > 1)
  {
    m_sOp <- paste0(' - Dividiendo el tamaño de la foto por ', round(reducc, 1), '...')
    if(i_bPrint)  print(m_sOp)
    m_sOp <- ' - Dividiendo el tamaño de la foto...'
    mi_tiempo <- system.time(
      mi_foto.r <- resize(mi_foto, w = as.integer(dim(mi_foto)[1] / reducc)) # EBImage
    )
    if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
    mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  }
  else
    mi_foto.r <- mi_foto
  # # Algunos estadísticos de cada canal (color) de la foto:
  # apply(FUN = quantile, X = mi_foto, MARGIN = c(3))
  # apply(FUN = mean, X = mi_foto, MARGIN = c(3))
  # # Algunos estadísticos de cada canal (color) de la foto:
  # apply(FUN = quantile, X = mi_foto.r, MARGIN = c(3))
  # apply(FUN = mean, X = mi_foto.r, MARGIN = c(3))
  
  # #################################
  # Quitamos colores:
  # #################################
  # if(colorMode(mi_foto.r) == Color)
  if(length(dim(mi_foto.r)) > 2)
  {
    m_sOp <- ' - Convirtiendo a Grayscale...'
    if(i_bPrint)  print(m_sOp)
    mi_tiempo <- system.time(
      # Quitamos los canales sobrantes:
      # # colorMode(mi_foto.r) = Grayscale
      # mi_foto.r <- channel(mi_foto.r, 'grey')
      # Y, de paso, convertimos el array a una matrix:
      mi_foto.r <- (mi_foto.r[,,1] + mi_foto.r[,,2] + mi_foto.r[,,3]) / 3
    )
    if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
    mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  }
  # # Algunos estadísticos de cada canal (color) de la foto:
  # quantile(mi_foto.r)
  # mean(mi_foto.r)
  
  # #################################
  # Mostramos ambas fotos:
  # #################################
  print(paste0(' - Tamaño de la foto: ', round(object.size(mi_foto.r@.Data) / (1024*1024), 1)
               , ' Mb ('
               , dim(mi_foto.r)[1], ' x ', dim(mi_foto.r)[2], ' pixels)'))
  if(i_bVerFotos)
  {
    m_sOp <- ' - Mostrando fotos...'
    if(i_bPrint)  print(m_sOp)
    mi_tiempo <- system.time(
    {
      # # par(mfrow = c(1,1), mar = c(4,4,2,1))
      # # display(x = mi_foto, method = "raster", all = TRUE, title = nombrefichero)
      # display(x = mi_foto, method = "browser", title = nombrefichero)
      # display(x = mi_foto.r, method = "browser", title = nombrefichero)
      # # str(mi_foto@colormode)
      # # hist(mi_foto.r)
      # # dim(mi_foto.r)
      # # range(mi_foto)
      # # print(mi_foto, short = TRUE)
      # # print(mi_foto.r, short = TRUE)
      # # colorMode(mi_foto) = Grayscale
      # # display(mi_foto, method = "raster", all=TRUE)
      # # img_comb = combine(
      # #   mi_foto,
      # #   mi_foto + 0.3,
      # #   mi_foto * 2,
      # #   mi_foto ^ 0.5
      # # )
      # # display(img_comb, all = TRUE)
      par(mfrow = c(1,1), mar = c(4,4,2,1))
      plot(1:2, type='n', main = nombrefichero)
      rasterImage(mi_foto, 1, 1, 1.5, 1.5)
      rasterImage(mi_foto.r, 1.5, 1.5, 2, 2)
    })
    if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
    mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  }
  # #################################
  # Guardar foto Resized_xxxx:
  # #################################
  if(i_bGuardarJPG)
  {
    s_outPathName <- file.path(mi_path_out, paste0('Resized_', nombrefichero))
    m_sOp <- ' - Guardando foto...'
    if(i_bPrint)  print(m_sOp)
    mi_tiempo <- system.time({
      try(if(file.exists(s_outPathName)) file.remove(s_outPathName), silent = FALSE)
      try(writeJPEG(image = mi_foto.r, target = s_outPathName, quality = 1), silent = FALSE)
      try(mi_size <- file.size(s_outPathName))
      if(mi_size == 0)
      {
        if(i_bPrint)  print(paste0('ERROR al Guardar Foto \'', s_outPathName, '\'.'))
        return(-1)
      }
      else
      {
        if(i_bPrint)  print(paste0('Ok. Foto guardada (', round(mi_size/1024, 3), ' Kb) \'', s_outPathName, '\''))
      }
    })
    if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
    mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  }
  # #################################
  # Guardar CSV Resized_xxxx:
  # #################################
  if(i_bGuardarCSV)
  {
    s_outPathName <- file.path(mi_path_out, paste0('Resized_', nombrefichero, '.csv'))
    m_sOp <- ' - Guardando CSV...'
    if(i_bPrint)  print(m_sOp)
    mi_tiempo <- system.time({
      # mi_x <- as.matrix(mi_foto.r@.Data) # Matriz (2 dimensiones)
      mi_x <- mi_foto.r # Matriz (2 dimensiones)
      # mi_x <- as.vector(t(mi_foto.r@.Data)) # Una única columna
      try(if(file.exists(s_outPathName)) file.remove(s_outPathName), silent = FALSE)
      try(write.csv(x = mi_x, file = s_outPathName, row.names = FALSE), silent = FALSE)
      try(mi_size <- file.size(s_outPathName))
      if(mi_size == 0)
      {
        print(paste0('ERROR al Guardar CSV \'', s_outPathName, '\'.'))
        return(-1)
      }
      else
      {
        if(i_bPrint)  print(paste0('Ok. CSV guardado (', round(mi_size/(1024*1024), 3), ' Mb) \'', s_outPathName, '\''))
      }
    })
    if(i_bPrint)  print(paste0("Tiempo: ", round(mi_tiempo[3],1), " segundos."))
    mi_tiempo_total <<- rbind(mi_tiempo_total, c(Op = m_sOp, mi_tiempo[3]))
  }
  # print('Ok.')  
}

# mi_imgResize(nombrefichero = "Foto 5-7-15 19 21 29.jpg")
# mi_imgResize(nombrefichero = "Foto 5-7-15 19 21 49.jpg")
# mi_imgResize(nombrefichero = "Foto 4-7-15 21 42 45.jpg")
# mi_imgResize(nombrefichero = "Foto 26-6-15 19 35 17.jpg")
# mi_imgResize(nombrefichero = "Foto 4-7-15 21 44 54.jpg")

mi_imgResizePath <- function(i_sPath = "Datos", i_sPathOut = FALSE, i_bProcesarTodo = FALSE, i_bVerFotos = FALSE, i_bGuardarJPG = TRUE, i_bGuardarCSV = TRUE, i_bResetTime = TRUE, i_bPrint = FALSE)
{
  sPath <- file.path(i_sPath)
  if(i_sPathOut == FALSE)
    sPathOut <- sPath
  else
    sPathOut <- file.path(i_sPathOut)
  # Ficheros JPG NOT resized:
  # mis_ficheros <- list.files(path = './Datos/', pattern = '\\.jpg')
  mis_ficheros <- list.files(path = sPath, pattern = '\\.jpg')
  mis_ficheros <- mis_ficheros[substr(mis_ficheros, 1, 8) != 'Resized_']
  # Ficheros JPG NOT resized y NO procesados todavía:
  if(!i_bProcesarTodo)
    mis_ficheros <- mis_ficheros[!(paste0('Resized_', mis_ficheros) %in% list.files(path = sPathOut, pattern = '^Resized_.*\\.jpg$'))]
  nTot <- length(mis_ficheros)
  if(nTot > 0)
  {
    # Variable para guardar los tiempos:
    if(!exists('mi_tiempo_total')) mi_tiempo_total <<- vector()
    if(i_bResetTime) mi_tiempo_total <<- vector()
    systime_tmp <- proc.time()
    n <- 0
    
    for(f in mis_ficheros)
    {
      n <- n + 1
      print(paste0(' - - Procesando fichero \'', f, '\'...'))
      mi_imgResize(nombrefichero = f, mi_path_datos = sPath, mi_path_out = sPathOut,
                   i_bVerFotos=i_bVerFotos, i_bGuardarJPG=i_bGuardarJPG, i_bGuardarCSV=i_bGuardarCSV, i_bPrint = i_bPrint)
      print(paste0(" - - Tiempo Total acumulado: ", round(sum(as.double(mi_tiempo_total[,'elapsed'])) / 60, 3), " minutos. ", n ,"/", nTot, " fotos (", round(100*n/nTot,2) , "%)"))
    }
    # Estadísticas finales:
    # aggregate(elapsed ~ Op, as.data.frame(mi_tiempo_total, stringsAsFactors = FALSE), function(a){ mean(as.double(a)) } )
    print( setNames( aggregate(elapsed ~ Op, as.data.frame(mi_tiempo_total, stringsAsFactors = FALSE)
                               , function(a){ round(mean(as.double(a)), 2) }  )
                     , c('Op', 'Promedio(Segs)') ) )
    print( setNames( aggregate(elapsed ~ Op, as.data.frame(mi_tiempo_total, stringsAsFactors = FALSE)
                               , function(a){ round(sum(as.double(a)) / 60, 3) }  )
                     , c('Op', 'Total(Mins)') ) )
    print(paste0("Tiempo Medio por foto: ", round(mean(as.double(mi_tiempo_total[,'elapsed'])), 1), " segundos."))
    print(paste0("Tiempo Total acumulado: ", round(sum(as.double(mi_tiempo_total[,'elapsed'])) / 60, 1), " minutos. (", nTot, " fotos)"))
    print(paste0("Tiempo Total REAL: ", round(as.double(((proc.time() - systime_tmp)['elapsed'])) / 60, 1), " minutos."))
  }
  else
  {
    print('Ok. No hay ficheros.')
  }
}

mi_imgResizePath(i_sPath = 'Datos', i_sPathOut = 'Datos', i_bProcesarTodo = FALSE, i_bVerFotos = FALSE)
mi_imgResizePath(i_sPath = 'C:/Users/jtoharia/Sync/FotosRubia', i_sPathOut = 'Datos', i_bProcesarTodo = TRUE, i_bVerFotos = FALSE, i_bGuardarCSV = FALSE)

# # Ficheros JPG NOT resized:
# mis_ficheros <- list.files(path = './Datos/', pattern = '\\.jpg')
# mis_ficheros <- mis_ficheros[substr(mis_ficheros, 1, 8) != 'Resized_']
# # Ficheros JPG NOT resized y NO procesados todavía:
# 
# # Ficheros CSV resized:
# mis_ficheros <-list.files(path = './Datos/', pattern = '^Resized_.*\\.csv$')
# 
# # Ficheros JPG resized:
# mis_ficheros <-list.files(path = './Datos/', pattern = '^Resized_.*\\.jpg$')


# X <- read.csv("olivetti_X.csv", header = F)
# labels <- read.csv("olivetti_y.csv", header = F)
# 
# # Dataframe of resized images
# rs_df <- data.frame()
# 
# # Main loop: for each image, resize and set it to greyscale
# for(i in 1:nrow(X))
# {
#     # Try-catch
#     result <- tryCatch({
#     # Image (as 1d vector)
#     img <- as.numeric(X[i,])
#     # Reshape as a 64x64 image (EBImage object)
#     img <- Image(img, dim=c(64, 64), colormode = "Grayscale")
#     # Resize image to 28x28 pixels
#     img_resized <- resize(img, w = 28, h = 28)
#     # Get image matrix (there should be another function to do this faster and more neatly!)
#     img_matrix <- img_resized@.Data
#     # Coerce to a vector
#     img_vector <- as.vector(t(img_matrix))
#     # Add label
#     label <- labels[i,]
#     vec <- c(label, img_vector)
#     # Stack in rs_df using rbind
#     rs_df <- rbind(rs_df, vec)
#     # Print status
#     print(paste("Done",i,sep = " "))},
#     # Error function (just prints the error). Btw you should get no errors!
#     error = function(e){print(e)})
# }
# 
# 
# # Set names. The first columns are the labels, the other columns are the pixels.
# names(rs_df) <- c("label", paste("pixel", c(1:784)))
# 
# # Train-test split
# #-------------------------------------------------------------------------------
# # Simple train-test split. No crossvalidation is done in this tutorial.
# 
# # Set seed for reproducibility purposes
# set.seed(100)
# 
# # Shuffled df
# shuffled <- rs_df[sample(1:400),]
# 
# # Train-test split
# train_28 <- shuffled[1:360, ]
# test_28 <- shuffled[361:400, ]
# 
# # Save train-test datasets
# write.csv(train_28, "C://train_28.csv", row.names = FALSE)
# write.csv(test_28, "C://test_28.csv", row.names = FALSE)

# Done!
print("Done!")
