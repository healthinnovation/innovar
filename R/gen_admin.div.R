#' Create categories for adiministrative divisions (Perú)
#'
#'Function 'gen_admin.div' returns the natural, geographical, or administrative division of Peru that includes the region, province or district provided to the function.
#'
#' @param x x is a list/vector with one of the 25 regions/departments names of Peru (first-level administrative subdivisions)
#' @param method defines the type of natural, geographical, or administrative division that should be returned
#' Traditional: Coastal, Andes Mountains, Amazon Jungle, or Lima & Callao
#' TC: (Lima no incluye Huarua/Cañete)\href{https://andina.pe/agencia/noticia-tc-crea-cinco-macro-regiones-para-facilitar-acceso-a-justicia-constitucional-604658.aspx}{Administratives Macro Regions of the Constitutional Tribunal}
#' Cardinal: North, South, Amazon Jungle, or Lima & Callao
#' Traditional Cardinal:  Coastal and Andes Mountains are divided into North and South (e.g. South Andes Mountains)
#'
#' @return Tabla con las divisiones resultantes del Peru
#'
#' @examples
#' \dontrun{
#' df <- data.frame(reg=c("LIMA","CALLAO","CAJAMARCA","AMAZONAS","SAN MARTIN","HUANUCO",
#' "PASCO","JUNIN","CUSCO","PUNO","APURIMAC","AYACUCHO","HUANCAVELICA","TUMBES","PIURA",
#' "LAMBAYEQUE","LA LIBERTAD","ANCASH","ICA","AREQUIPA","TACNA","MOQUEGUA","LORETO",
#' "UCAYALI", "MADRE DE DIOS"), stringsAsFactors = FALSE)
#' }
#'
#' @export gen_admin.div
gen_admin.div <- function(x,
                          method="Traditional") {
  reg.mountain.north <- c("CAJAMARCA","AMAZONAS","SAN MARTIN","HUANUCO","PASCO")
  reg.mountain.south <- c("JUNIN","CUSCO","PUNO","APURIMAC","AYACUCHO","HUANCAVELICA")
  reg.coastal.north <- c("TUMBES","PIURA","LAMBAYEQUE","LA LIBERTAD","ANCASH")
  reg.coastal.south <- c("ICA","AREQUIPA","TACNA","MOQUEGUA")

  reg.south <- c("JUNIN","HUANCAVELICA","ICA","AYACUCHO","APURIMAC",
                 "AREQUIPA","MOQUEGUA","TACNA","PUNO","CUSCO")
  reg.north <- c("TUMBES","PIURA","LAMBAYEQUE","LA LIBERTAD","ANCASH",
                 "CAJAMARCA","AMAZONAS","SAN MARTIN","HUANUCO","PASCO")
  reg.jungle <- c("LORETO","UCAYALI","MADRE DE DIOS")

  reg.tc.north <- c("ANCASH","CAJAMARCA","LA LIBERTAD",
                    "LAMBAYEQUE","PIURA","TUMBES")
  reg.tc.center <- c("APURIMAC","AYACUCHO","HUANCAVELICA",
                     "HUANUCO","JUNIN","PASCO","ICA")
  reg.tc.south <- c("AREQUIPA","CUSCO","MADRE DE DIOS",
                    "MOQUEGUA","PUNO","TACNA")
  reg.tc.east <- c("AMAZONAS","LORETO","SAN MARTIN","UCAYALI")

  if(method=="Traditional") {

    x = replace(x, x %in% reg.jungle,"AMAZON JUNGLE")
    x = replace(x, x %in% reg.mountain.north | x %in% reg.mountain.south,"ANDES MOUNTAINS")
    x = replace(x, x %in% reg.coastal.north | x %in% reg.coastal.south,"COASTAL")
    x = replace(x, x=="CALLAO" | x=="LIMA","LIMA & CALLAO")

  } else if(method=="TC") {

    x = replace(x, x %in% reg.tc.north,"NORTH")
    x = replace(x, x %in% reg.tc.south,"SOUTH")
    x = replace(x, x %in% reg.tc.center,"CENTER")
    x = replace(x, x %in% reg.tc.east,"EAST")
    x = replace(x, x=="CALLAO" | x=="LIMA","LIMA & CALLAO")
    # Deberian ser solo los distritos de Lima, Callao, más Huarua/Cañete. Por ahora todo LIMA will do just fine
  } else if(method=="Cardinal") {

    x = replace(x, x %in% reg.north,"NORTH")
    x = replace(x, x %in% reg.south ,"SOUTH")
    x = replace(x, x %in% reg.jungle,"AMAZON JUNGLE")
    x = replace(x, x=="CALLAO" | x=="LIMA","LIMA & CALLAO")

  } else if(method=="Traditional Cardinal") {

    x = replace(x, x %in% reg.jungle,"AMAZON JUNGLE")
    x = replace(x, x %in% reg.mountain.south,"SOUTH ANDES MOUNTAINS")
    x = replace(x, x %in% reg.mountain.north,"NORTH ANDES MOUNTAINS")
    x = replace(x, x %in% reg.coastal.north,"NORTH COAST")
    x = replace(x, x %in% reg.coastal.south,"SOUTH COAST")
    x = replace(x, x=="CALLAO" | x=="LIMA","LIMA & CALLAO")
  }
  return(x)
}
