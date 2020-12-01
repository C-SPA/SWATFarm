# SWATFarm
SWAT Model * Farm Routines

SWATFarm is an adaptation of SWAT (Soil and Water Assessment Tool) that uses dynamic nutrient fractions and daily manure production calculated by process-based animal subroutine. 

To compile SWATFarm in windows user can use “Rtools” which is a R toolset and combines files from Cygwin, MinGW, and MSYS. Here is the link to download “Rtools” : https://cran.r-project.org/bin/windows/Rtools/

After installing development environment open command window or terminal, clone with git, gh, or GitHub Desktop.\
git clone https://github.com/C-SPA/SWATFarm.git \
cd SWATFarm/src \
make debug32 # on MSWindows or:\
make debug64 # on Linux/Mac OSX\
