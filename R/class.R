# Class definitions

setClass("indexFit",
         slots = c(
           Fits = "list",
           Explained = "tbl_df",
           Loadings = "tbl_df"
         )
)


setClass("indexcalc",
         slots = c(
           Options = "list",
           Data = "tbl_df",
           Fit = "indexFit"
         )
)
