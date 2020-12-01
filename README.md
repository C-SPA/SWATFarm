# SWATFarm
__Current author list:__
_Moges B. Wagena, Amy S. Collick, Douglas Liebe, Roja Kaveh Garna, Darrell Bosch, Robin White, Zachary M. Easton, Daniel R. Fuka_
##
SWAT Model * Farm Routines

SWATFarm is an adaptation of SWAT (Soil and Water Assessment Tool) that uses dynamic nutrient fractions and daily manure production calculated by process-based animal subroutine. 
SWATFarm enhances the understanding of the impacs of livestock management on food production and watershed nutrient dynamics at different spatial and temporal resolutions. SWATFarm allows animal nutrition to be altered based on economic and environmental benefits. As a result, the new model as an integrated agroecosystem model can be used to optimize crop and livestock management practices to mitigate the effect of climate change, maximize food output, and minimize environmental impacts.

To compile SWATFarm in windows, the user can use __Rtools__ which is a R toolset and combines files from Cygwin, MinGW, and MSYS. Here is the link to download “Rtools” : https://cran.r-project.org/bin/windows/Rtools/

After installing development environment open command window or terminal, clone with git, gh, or GitHub Desktop.\
```bash
git clone https://github.com/C-SPA/SWATFarm.git 
cd SWATFarm/src 
make debug32 # on MSWindows or: 
make debug64 # on Linux/Mac OSX 
```
