################################################################################
################################################################################
################################################################################
################################################################################
# NCBI extraction information
# https://docs.ropensci.org/rentrez/articles/rentrez_tutorial.html#getting-summary-data-entrez_summary 
################################################################################
################################################################################
################################################################################
################################################################################
# Package Rentrez
library(rentrez)
library(xml2)
################################################################################
################################################################################
################################################################################
################################################################################
# Working Directory
getwd()
setwd("C:YOUR/WORKING/DIRECTORY")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# Extract the information in a metadata table

# Lista de Run IDs
#run_ids <- c("ERR2860778", "ERR1524111", "ERR2205480", "ERR139681", "ERR579243", "SRR1021873", "SRR5950850")
run_ids <- readLines("ListRun_EMP.txt")

# Crear un data frame vacío para almacenar los resultados
metadata <- data.frame(
  Run = character(),
  LIBRARY_STRATEGY = character(),
  SPOT_LENGTH = numeric(),
  total_bases = numeric(),
  BioProject = character(),
  BioSample = character(),
  size = numeric(),
  center_name = character(),
  Collection_Date = character(),
  DATASTORE_filetype = character(),
  DATASTORE_provider = character(),
  DATASTORE_region = character(),
  Depth = numeric(),
  Experiment = character(),
  geo_loc_name_country = character(),
  host_common_name = character(),
  host_latin_name = character(),
  INSTRUMENT_MODEL = character(),
  lat_lon = character(),
  LIBRARY_NAME = character(),
  LibraryLayout = character(),
  LibrarySelection = character(),
  LibrarySource = character(),
  SCIENTIFIC_NAME = character(),
  Platform = character(),
  published = character(),
  alias = character(),
  STUDY_REF_accession = character(),
  env_broad_scale = character(),
  env_local_scale = character(),
  env_medium = character(),
  stringsAsFactors = FALSE
)

# Función para extraer datos con manejo de errores
extraer_valor <- function(xml, xpath) {
  nodo <- xml_find_first(xml, xpath)
  if (!is.null(nodo) && !is.na(nodo)) {
    return(xml_text(nodo))
  } else {
    return(NA)
  }
}

# Procesar cada Run ID
for (run in run_ids) {
  cat("Procesando:", run, "\n")
  
  # Buscar el Run ID en la base de datos SRA
  search_result <- tryCatch(
    entrez_search(db = "sra", term = run),
    error = function(e) {
      cat("Error en la búsqueda de", run, ":", e$message, "\n")
      return(NULL)
    }
  )
  
  if (!is.null(search_result) && length(search_result$ids) > 0) {
    # Obtener el XML con detalles del Run ID
    record <- tryCatch(
      entrez_fetch(db = "sra", id = search_result$ids, rettype = "xml"),
      error = function(e) {
        cat("Error al obtener detalles para", run, ":", e$message, "\n")
        return(NULL)
      }
    )
    
    if (!is.null(record)) {
      # Parsear el texto XML con xml2
      xml <- read_xml(record)
      
      # Extraer datos específicos usando XPath
      metadata_row <- data.frame(
        Run = run,
        LIBRARY_STRATEGY = extraer_valor(xml, "//LIBRARY_STRATEGY"),
        SPOT_LENGTH = as.numeric(extraer_valor(xml, "//SPOT_LENGTH")),
        total_bases = as.numeric(extraer_valor(xml, "//RUN/@total_bases")),
        BioProject = extraer_valor(xml, "//EXTERNAL_ID[@namespace='BioProject']"),
        BioSample = extraer_valor(xml, "//EXTERNAL_ID[@namespace='BioSample']"),
        size = as.numeric(extraer_valor(xml, "//RUN/@size")),
        center_name = extraer_valor(xml, "//CENTER_NAME"),
        Collection_Date = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='collection date']/VALUE"),
        DATASTORE_filetype = paste(sapply(xml_find_all(xml, "//CloudFile/@filetype"), xml_text), collapse = ","),
        DATASTORE_provider = paste(sapply(xml_find_all(xml, "//CloudFile/@provider"), xml_text), collapse = ","),
        DATASTORE_region = paste(sapply(xml_find_all(xml, "//CloudFile/@location"), xml_text), collapse = ","),
        Depth = as.numeric(extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='geographic location (depth)']/VALUE")),
        Experiment = extraer_valor(xml, "//EXPERIMENT/@accession"),
        geo_loc_name_country = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='geographic location (country and/or sea)']/VALUE"),
        host_common_name = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='common name']/VALUE"),
        host_latin_name = extraer_valor(xml, "//SAMPLE_NAME/SCIENTIFIC_NAME"),
        INSTRUMENT_MODEL = extraer_valor(xml, "//INSTRUMENT_MODEL"),
        lat_lon = paste(extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='geographic location (latitude)']/VALUE"),
                        extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='geographic location (longitude)']/VALUE"), sep = " "),
        LIBRARY_NAME = extraer_valor(xml, "//LIBRARY_NAME"),
        LibraryLayout = extraer_valor(xml, "//LIBRARY_LAYOUT/*[1]/name()"),
        LibrarySelection = extraer_valor(xml, "//LIBRARY_SELECTION"),
        LibrarySource = extraer_valor(xml, "//LIBRARY_SOURCE"),
        SCIENTIFIC_NAME = extraer_valor(xml, "//SCIENTIFIC_NAME"),
        Platform = extraer_valor(xml, "//PLATFORM/*[1]/name()"),
        published = extraer_valor(xml, "//RUN/@published"),
        alias = extraer_valor(xml, "//SAMPLE/@alias"),
        STUDY_REF_accession = extraer_valor(xml, "//STUDY/@accession"),
        env_broad_scale = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='environment (biome)']/VALUE"),
        env_local_scale = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='environment (feature)']/VALUE"),
        env_medium = extraer_valor(xml, "//SAMPLE_ATTRIBUTE[TAG='environment (material)']/VALUE"),
        stringsAsFactors = FALSE
      )
      
      # Agregar la información al data frame principal
      metadata <- rbind(metadata, metadata_row)
    } else {
      cat("No se pudo procesar el XML para el Run ID:", run, "\n")
    }
    
  } else {
    cat("No se encontró información para el Run ID:", run, "\n")
  }
}

# Check Metadata
#View(metadata)
print("Metadata Done")

# Guardar el resultado en un archivo CSV
output_file <- "metadata_sra_EMP.csv"
write.csv(metadata, output_file, row.names = FALSE)
print("Save Metadata")


# Mostrar un resumen del resultado final
cat("\nProceso completado. Resultados guardados en:", output_file, "\n")
print("(^_^)")

# END (^_^)
################################################################################
################################################################################
################################################################################
