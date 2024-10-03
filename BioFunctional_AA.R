# Configura el repositorio de CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("here")
library(here)

# Cambia al directorio raíz del proyecto
setwd(here::here())

# Fuente de scripts si hay otros archivos necesarios
source(here::here("code", "funciones_analisis_datos.R"))
source(here::here("code", "aplicacion_biofunctional.R"))

# Inicia la aplicación Shiny
shinyApp(ui, server)

