# ===============================================================================
# Modelamiento hidrologico utilizando el modelo GR2M
# ===============================================================================
# Profesor: Dr. Carlos A. Fernandez Palomino
# Email: cafpxl@gmail.com
#
# Contenido del script:
#
# 1. Configuracion del entorno de trabajo y carga de paquetes
#
# - Directorio de trabajo: Establece el directorio donde se encuentran todos
#   los archivos y datos necesarios.
# - Carga de paquetes: Define y carga todos los paquetes necesarios para el
#   analisis, incluyendo sp, raster, dplyr, airGR, entre otros. Si algun
#   paquete no esta instalado, se instala automaticamente.
#
# 2. Procesamiento de datos meteorologicos
#
# - Estimacion de promedios areales de precipitacion y temperatura:
#   Calcula valores promedio de las variablesen toda la cuenca, generando
#   series temporales que se utilizaran en analisis
#   hidrologicos posteriores.
#
# 3. Estimacion de la Evapotranspiracion Potencial (ETP)
#
# - Utiliza el metodo de Hargreaves para calcular la ETP a partir de las
#   variables Temperatura_maxima y Temperatura_minima.
# - La latitud de la cuenca se calcula en funcion de su centroide.
#
# 4. Estimacion de precipitacion promedio areal
#
# - Producto RAIN4PE: Importa los datos del archivo NetCDF y calcula el
#   promedio areal de precipitacion en la cuenca basado en el producto RAIN4PE
#
# 5. Calculo de caudales diarios y mensuales
#
# - Importacion de datos de caudales: Se importan los datos de caudales en m³/s
#   y se agregan a caudales mensuales.
# - Conversion a caudales en mm: Calcula el caudal en mm utilizando el area de
#   la cuenca para convertir las unidades.
# - Exportacion de caudales: Guarda los caudales mensuales en m³/s y en mm en
#   archivos CSV para su uso posterior.
#
# 6. Modelado hidrologico con el modelo GR2M
#
# - Parametros iniciales del modelo GR2M: Se define una configuracion inicial
#   de parametros (x1, x2) y periodos para la simulacion, calentamiento y
#   calibracion del modelo hidrologico.
# - Calibracion y validacion del modelo: Se realiza la calibracion y validacion
#   del modelo GR2M utilizando los datos disponibles y se analizan los resultados.

# ===============================================================================
# Configuracion del proyecto, carga de paquetes y funciones auxiliares
# ===============================================================================

# Seleccionar el directorio de trabajo (especificar ruta)
WF <- "C:/CURSO_SWAT"  # Ruta especifica del proyecto
setwd(WF)

# Lista de paquetes requeridos
paquetes <- c("sp", "sf", "raster", "terra", "tidyterra", "ncdf4", "tibble",
              "dplyr", "xts", "lubridate", "gstat", "corrplot", "airGR", "hydroGOF",
              "readr", "lattice", "latticeExtra", "ggplot2", "RColorBrewer", "reshape2", "tmap")

# Instalar paquetes que no esten instalados
paquetes_instalados <- paquetes %in% installed.packages()[, "Package"]
if (any(!paquetes_instalados)) {
  paquetes_faltantes <- paquetes[!paquetes_instalados]
  message("Instalando paquetes faltantes: ", paste(paquetes_faltantes, collapse = ", "))
  install.packages(paquetes_faltantes)
}

# Cargar todos los paquetes, con manejo de errores
invisible(lapply(paquetes, library, character.only = TRUE))

# Cargar funciones auxiliares
source("1_programas/Script_auxiliares/utilidades.R")

# Activar para mapas interactivos
tmap_mode("view")

# ===============================================================================
# Definir variables de entrada
# ===============================================================================
# Definir variables de entrada --------------------------------------------
inicio <- "1981-01-01"# inicio del periodo de analisis
fin <- "2015-12-31"# fin del periodo de analisis
ruta_shapefile_cuenca <- "C:/CURSO_SWAT/3_SWAT/1_datos/4_shapefiles/subs1.shp"

# Definir sistema de coordenadas
# http://spatialreference.org/ref/epsg/32718/
utm <- "+init=epsg:32719"
wgs <- "+proj=longlat +datum=WGS84"

# Gestion de datos espaciales ---------------------------------------------
# Importar shapefile de la cuenca y definir sistema de coordenadas
sub_cuenca_shp <- raster::shapefile(ruta_shapefile_cuenca)
plot(sub_cuenca_shp)
#proj4string(cuenca_shp) <- CRS(utm)
cuenca_shp_sf <- sf::st_as_sf(sub_cuenca_shp) %>%
  st_union() %>% sf::st_as_sf()

cuenca_shp <- as(cuenca_shp_sf, "Spatial")

plot(cuenca_shp_sf)


tm_shape(cuenca_shp_sf) + 
  tm_borders()

# ===============================================================================
# Estimacion de precipitacion promedio areal en base al producto RAIN4PE
# ===============================================================================
# Referencia: Fernandez-Palomino et al. (2022).
# Ver: https://doi.org/10.1175/JHM-D-20-0285.1

# Cargar el archivo NetCDF de RAIN4PE y extraer el promedio areal sobre la cuenca
prec_nc <- rast("3_SWAT/1_datos/2_meteorologicos/RAIN4PE/RAIN4PE_daily_0.1d_1981_2015_v1.0.nc")
prec <- extract(prec_nc, cuenca_shp_sf, fun = mean)
prec <- tibble(
  fecha = seq.Date(as.Date("1981-01-01"), as.Date("2015-12-31"), by = "day"),
  value = round(t(prec)[-1], 2)
) %>%
  dplyr::filter(fecha >= as.Date(inicio) & fecha <= as.Date(fin))


# Convertir a paso mensual y calcular el promedio de 'value' por mes
prec <- prec %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(value = sum(value, na.rm = TRUE))

prec <- as.xts(prec)

# Graficar
xyplot(prec, main = "Precipitación mensual")


# ===============================================================================
# Estimacion de temperatura promedio areal en base al producto PISCO
# ===============================================================================
# Tmax --------------------------------------------------------------------
# Cargar el archivo NetCDF y extraer el promedio areal sobre la cuenca
tmax_nc <- rast("3_SWAT/1_datos/2_meteorologicos/PISCO/PISCOdtx_v1.1.nc")
tmax <- extract(tmax_nc, cuenca_shp_sf, fun = mean)# promedio_areal
tmax <- tibble(
  fecha = seq.Date(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"),
  value = round(t(tmax)[-1], 2)
) %>%
  dplyr::filter(fecha >= as.Date(inicio) & fecha <= as.Date(fin))  # Filtrar para el periodo de analisis


# Agregar a paso mensual
tmax <- tmax %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(value = mean(value, na.rm = TRUE))


# Tmin --------------------------------------------------------------------
tmin_nc <- rast("3_SWAT/1_datos/2_meteorologicos/PISCO/PISCOdtn_v1.1.nc")
tmin <- extract(tmin_nc, cuenca_shp_sf, fun = mean)
tmin <- tibble(
  fecha = seq.Date(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"),
  value = round(t(tmin)[-1], 2)
) %>%
  dplyr::filter(fecha >= as.Date(inicio) & fecha <= as.Date(fin))  # Filtrar para el periodo de analisis

# Agregar a paso mensual
tmin <- tmin %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(mes) %>%
  summarise(value = mean(value, na.rm = TRUE))

tmax <- tmax %>% as.xts()
tmin <- tmin %>% as.xts()

# Graficar temperaturas
xyplot(tmax, ylim = c(-10, 30), col = "red", main = "Temperatura Maxima (rojo) y Minima (azul)") +
  xyplot(tmin, ylim = c(-10, 30), col = "blue")


# ===============================================================================
# Estimacion de evapotranspiracion potencial (ETP) usando Hargreaves
# ===============================================================================
# Calcular latitud del centroide de la cuenca
latitud <- coordinates(spTransform(cuenca_shp, CRS(wgs)))[1, 2]

# Calcular y graficar evapotranspiracion potencial (ETP)
etp <- et.hargreaves(Tmin = tmin, Tmax = tmax, lat = latitud)
colnames(etp) <- "value"
xyplot(etp, main = "Evapotranspiración Potencial")

# ===============================================================================
# Importar datos de caudales y convertir a mm
# ===============================================================================
# Leer datos de caudales diarios
Q_diario <- as.xts(read.zoo("3_SWAT/1_datos/1_caudales/Qobserved.csv", header = TRUE, sep = ","))
Q_diario <- window(Q_diario, start = "1981-01-01",end = "2015-12-31")

xyplot(Q_diario, main = "Caudales diarios en m3/s")

q_mcs <- diario2mensual_xts_df(Q_diario, FUN=MEAN)
xyplot(q_mcs, main = "Caudales mensuales en m3/s")

q_mm <- q_mcs * day(q_mcs) * 24 * 3600 * 1000 / area(cuenca_shp)
names(q_mm) <- "value"
xyplot(q_mm, main = "Caudales mensuales en mm")


# ===============================================================================
# Modelamiento hidrologico con GR2M
# ===============================================================================
# Importar y visualizar datos preprocesados
area_cuenca <- area(cuenca_shp) / 1e6  # km²

# Graficar evapotranspiracion y precipitacion
xyplot(as.xts(etp), col="black", ylim=c(0, 200), ylab="mm", xlab="Mes", main="Evapotranspiración")

xyplot(prec, col="blue", ylab="mm", xlab="Mes", main="Precipitación")


# Parametros del modelo GR2M
Param <- c(300, 1)# Valores iniciales de parámetros (x1 = 300, x2 = 1)
periodo_sim <- list(inicio = "1985-01-01", fin = "2015-12-31")
periodo_calentamiento <- list(inicio = "1985-01-01", fin = "1985-12-31")
periodo_calibracion <- list(inicio = "1986-01-01", fin = "2005-12-31")
periodo_validacion <- list(inicio = "2006-01-01", fin = "2015-12-31")


# ===============================================================================
# Simulacion inicial para el periodo de calibracion
# ===============================================================================
# Preparacion de datos
prec <- window(prec, start = as.Date(periodo_sim$inicio), end = as.Date(periodo_sim$fin))
etp <- window(etp, start = as.Date(periodo_sim$inicio), end = as.Date(periodo_sim$fin))


BasinObs <- data.frame(
  DatesR = as.POSIXlt(seq.Date(
    from = as.Date(periodo_sim$inicio),
    to = as.Date(periodo_sim$fin),
    by = "month"
  )),
  P = as.vector(coredata(prec$value)),
  E = as.vector(coredata(etp$value)),
  Qmm = as.vector(coredata(q_mm$value))
)

dir.create("2_R_hydroSWAT/Salidas.R", recursive = T)
write.csv(BasinObs, file = "2_R_hydroSWAT/Salidas.R/datos_para_gr2m.csv")


# Configuracion del modelo y opciones de ejecucion del modelo
InputsModel <- CreateInputsModel(
  FUN_MOD = RunModel_GR2M,
  DatesR = BasinObs$DatesR,
  Precip = BasinObs$P,
  PotEvap = BasinObs$E
)

IndPeriod_WarmUp <- which(
  BasinObs$DatesR >= as.Date(periodo_calentamiento$inicio) &
    BasinObs$DatesR <= as.Date(periodo_calentamiento$fin)
)

Ind_Run <- which(
  BasinObs$DatesR >= as.Date(periodo_calibracion$inicio) &
    BasinObs$DatesR <= as.Date(periodo_calibracion$fin)
)

RunOptions <- CreateRunOptions(
  FUN_MOD = RunModel_GR2M,
  InputsModel = InputsModel,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = Ind_Run)


# Simulacion y visualizacion de resultados
OutputsModel <- RunModel_GR2M(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Param = Param)

plot.OutputsModel(
  x = OutputsModel,
  Qobs = BasinObs$Qmm[Ind_Run],
  BasinArea=area_cuenca)


# Calibracion manual: ajustar 'Param' para mejorar los resultados



# ===============================================================================
# Calibracion automatica del modelo
# ===============================================================================

# Preparar datos para calibracion
InputsCrit <- CreateInputsCrit(
  FUN_CRIT = ErrorCrit_NSE,
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Obs = BasinObs$Qmm[Ind_Run])

CalibOptions <- CreateCalibOptions(
  FUN_MOD = RunModel_GR2M,
  FUN_CALIB = Calibration_Michel)

# Calibracion
OutputsCalib <- Calibration(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  InputsCrit = InputsCrit,
  CalibOptions = CalibOptions,
  FUN_MOD = RunModel_GR2M,
  # FUN_CRIT = ErrorCrit_NSE,
  FUN_CALIB = Calibration_Michel)

# Simulacion utilizando los parametros calibrados
(Param <- OutputsCalib$ParamFinalR) #parametros calibrados
OutputsModel <- RunModel(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Param = Param,
  FUN = RunModel_GR2M)

# Resultados
plot.OutputsModel(
  x = OutputsModel,
  Qobs = BasinObs$Qmm[Ind_Run],
  BasinArea=area_cuenca)



# Calcular y graficar caudales simulados y observados en m³/s
sim_cal <- OutputsModel$Qsim*area_cuenca*1000/(day(index(q_mm)[Ind_Run])*24*3600)
obs_cal <- BasinObs$Qmm[Ind_Run]*area_cuenca*1000/(day(index(q_mm)[Ind_Run])*24*3600)

ggof(
  sim=sim_cal,
  obs=obs_cal,
  dates=as.Date(BasinObs$DatesR[Ind_Run]),
  ftype = "o", FUN=mean,col = c("red", "blue"),
  gofs=c("NSE","RMSE","PBIAS"),
  main="Periodo de calibracion"
)

# Guardar resultados de calibracion
write.csv(
  tibble(fecha=as.Date(BasinObs$DatesR[Ind_Run]), sim_cal,obs_cal),
  file = "2_R_hydroSWAT/Salidas.R/Q_periodo_calibracion.csv")


# ===============================================================================
# Validacion del modelo
# ===============================================================================

# indices del periodo de validacion
Ind_Run <- which(
  BasinObs$DatesR >= as.Date(periodo_validacion$inicio) &
    BasinObs$DatesR <= as.Date(periodo_validacion$fin)
)
# Preparar datos para validacion
RunOptions <- CreateRunOptions(
  FUN_MOD = RunModel_GR2M,
  InputsModel = InputsModel,
  IndPeriod_Run = Ind_Run)


# Simulacion utilizando los parametros calibrados
Param <- OutputsCalib$ParamFinalR
OutputsModel <- RunModel_GR2M(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Param = Param)


# Resultados
plot.OutputsModel(x = OutputsModel,
                  Qobs = BasinObs$Qmm[Ind_Run],
                  BasinArea=area_cuenca)

# Calcular y graficar caudales simulados y observados en m³/s
sim_val <- OutputsModel$Qsim*area_cuenca*1000/(day(index(q_mm)[Ind_Run])*24*3600)
obs_val <- BasinObs$Qmm[Ind_Run]*area_cuenca*1000/(day(index(q_mm)[Ind_Run])*24*3600)

ggof(
  sim=sim_val,
  obs=obs_val,
  dates=as.Date(BasinObs$DatesR[Ind_Run]),
  ftype = "o", FUN=mean,col = c("red", "blue"),
  gofs=c("NSE","RMSE","PBIAS"),
  main="Periodo de validacion"
)

# Guardar resultados de validacion

write.csv(
  tibble(fecha=as.Date(BasinObs$DatesR[Ind_Run]), sim_val,obs_val),
  file = "2_R_hydroSWAT/Salidas.R/Q_periodo_validacion.csv")

# Mostrar parámetros calibrados
Param


