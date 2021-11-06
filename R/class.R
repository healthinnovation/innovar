# Class definitions

setClass("indexFit",
         slots = c(
           Fits = "list",
           Explained = "tbl_df",
           Loadings = "tbl_df"
         ),
         prototype = list(
           Fits = list(),
           Explained = NA_real_,
           Loadings = NA_real_
         )
)

setClass("indexFits",
         slots = c(
           Specific = "list",
           Fits = "list",
           Explained = "tbl_df",
           Loadings = "tbl_df"
         ),
         prototype = list(
           Specific = list(),
           Fits = list(),
           Explained = NA_real_,
           Loadings = NA_real_
         )
)


setClass("indexcalc",
         slots = c(
           Options = "list",
           Data = "tbl_df",
           Fit = "indexFits"
         )
)
