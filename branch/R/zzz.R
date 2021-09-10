globalVariables(c(".", "i"))

.onAttach <- function(lib, pkg) {
    packageStartupMessage(paste0("* Please cite the 'meteoRIT' package as:\n", 
        "Desjeux Y. (2019). Research Integrated Tools to Process Weather Data Provided by M\u00e9t\u00e9o France. R package version 1.0.0.\n\n", 
        "See also: citation(\"meteoRIT\")\n"))
}
