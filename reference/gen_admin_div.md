# Create categories for adiministrative divisions (Peru)

Function 'gen_admin_div' returns the natural, geographical, or
administrative division of Peru that includes the region, province or
district provided to the function.

## Usage

``` r
gen_admin_div(x, method = "Traditional")
```

## Arguments

- x:

  x is a list/vector with one of the 25 regions/departments names of
  Peru (first-level administrative subdivisions).

- method:

  defines the type of natural, geographical, or administrative division
  that should be returned. Possible values are:

  \- Traditional: Coastal, Andes Mountains, Amazon Jungle, or Lima &
  Callao - TC: (Lima no incluye Huarua/Cañete)[Administratives Macro
  Regions of the Constitutional
  Tribunal](https://andina.pe/agencia/noticia-tc-crea-cinco-macro-regiones-para-facilitar-acceso-a-justicia-constitucional-604658.aspx) -
  Cardinal: North, South, Amazon Jungle, or Lima & Callao -
  Trad_Cardinal: Coastal and Andes Mountains are divided into North and
  South (e.g. South Andes Mountains)

## Value

Converts characters where it detects regions of Peru with the resulting
divisions specified.

## Examples

``` r
library(innovar)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df_dep <- data.frame(
Region = c("LIMA", "CALLAO", "CAJAMARCA", "AMAZONAS",
           "SAN MARTIN", "HUANUCO", "PASCO", "JUNIN", "CUSCO", "PUNO", "APURIMAC",
            "AYACUCHO", "HUANCAVELICA", "TUMBES", "PIURA", "LAMBAYEQUE",
            "LA LIBERTAD", "ANCASH", "ICA", "AREQUIPA", "TACNA", "MOQUEGUA",
            "LORETO", "UCAYALI", "MADRE DE DIOS"), stringsAsFactors = FALSE
          )

df_dep %>%
  mutate(
    Reg_traditional = gen_admin_div(Region, method = "Traditional"),
    Reg_TC = gen_admin_div(Region, method = "TC"),
    Reg_Cardinal = gen_admin_div(Region, method = "Cardinal"),
    Reg_Trad_Cardinal = gen_admin_div(Region, method = "Trad_Cardinal")
  )
#>           Region Reg_traditional        Reg_TC  Reg_Cardinal
#> 1           LIMA   LIMA & CALLAO LIMA & CALLAO LIMA & CALLAO
#> 2         CALLAO   LIMA & CALLAO LIMA & CALLAO LIMA & CALLAO
#> 3      CAJAMARCA ANDES MOUNTAINS         NORTH         NORTH
#> 4       AMAZONAS ANDES MOUNTAINS          EAST         NORTH
#> 5     SAN MARTIN ANDES MOUNTAINS          EAST         NORTH
#> 6        HUANUCO ANDES MOUNTAINS        CENTER         NORTH
#> 7          PASCO ANDES MOUNTAINS        CENTER         NORTH
#> 8          JUNIN ANDES MOUNTAINS        CENTER         SOUTH
#> 9          CUSCO ANDES MOUNTAINS         SOUTH         SOUTH
#> 10          PUNO ANDES MOUNTAINS         SOUTH         SOUTH
#> 11      APURIMAC ANDES MOUNTAINS        CENTER         SOUTH
#> 12      AYACUCHO ANDES MOUNTAINS        CENTER         SOUTH
#> 13  HUANCAVELICA ANDES MOUNTAINS        CENTER         SOUTH
#> 14        TUMBES         COASTAL         NORTH         NORTH
#> 15         PIURA         COASTAL         NORTH         NORTH
#> 16    LAMBAYEQUE         COASTAL         NORTH         NORTH
#> 17   LA LIBERTAD         COASTAL         NORTH         NORTH
#> 18        ANCASH         COASTAL         NORTH         NORTH
#> 19           ICA         COASTAL        CENTER         SOUTH
#> 20      AREQUIPA         COASTAL         SOUTH         SOUTH
#> 21         TACNA         COASTAL         SOUTH         SOUTH
#> 22      MOQUEGUA         COASTAL         SOUTH         SOUTH
#> 23        LORETO   AMAZON JUNGLE          EAST AMAZON JUNGLE
#> 24       UCAYALI   AMAZON JUNGLE          EAST AMAZON JUNGLE
#> 25 MADRE DE DIOS   AMAZON JUNGLE         SOUTH AMAZON JUNGLE
#>        Reg_Trad_Cardinal
#> 1          LIMA & CALLAO
#> 2          LIMA & CALLAO
#> 3  NORTH ANDES MOUNTAINS
#> 4  NORTH ANDES MOUNTAINS
#> 5  NORTH ANDES MOUNTAINS
#> 6  NORTH ANDES MOUNTAINS
#> 7  NORTH ANDES MOUNTAINS
#> 8  SOUTH ANDES MOUNTAINS
#> 9  SOUTH ANDES MOUNTAINS
#> 10 SOUTH ANDES MOUNTAINS
#> 11 SOUTH ANDES MOUNTAINS
#> 12 SOUTH ANDES MOUNTAINS
#> 13 SOUTH ANDES MOUNTAINS
#> 14           NORTH COAST
#> 15           NORTH COAST
#> 16           NORTH COAST
#> 17           NORTH COAST
#> 18           NORTH COAST
#> 19           SOUTH COAST
#> 20           SOUTH COAST
#> 21           SOUTH COAST
#> 22           SOUTH COAST
#> 23         AMAZON JUNGLE
#> 24         AMAZON JUNGLE
#> 25         AMAZON JUNGLE
```
