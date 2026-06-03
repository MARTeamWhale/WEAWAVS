# Script to batch rename files 

# WEAWAVS: Photo file naming for photos sent externally

# Naming convention: 
# Organization_Region_Program_Survey_Species_YYYY-MM-DD_photofilenumber.jpg

# Example:
# DFO_MAR_TeamWhale_WEAWAVS_Orcas_2026-05-05_C0001.jpg


# Choosing file directory interactively
setwd(choose.dir(caption = "Select folder containing files to rename"))

# Create a list of files to rename
(files2rename <- list.files(pattern=".JPG"))

file.rename(from=files2rename, 
            to=sub(pattern="KWSR-2025", 
                   replacement="KWSR_2025", files2rename))      

(files <- list.files(pattern=".JPG"))

write.csv(files, "files.csv", row.names = F)
