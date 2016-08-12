# DeltaModel
A model on fisheries dynamics on the Wax Lake Delta
This is the code of a individual-based, population dynamics model on fisheries dynamics in the [Wax Lake Delta](http://en.wikipedia.org/wiki/Wax_Lake). This cohort model considers four species ([Paleomonetes sp.](http://en.wikipedia.org/wiki/Palaemonetes), [Menidia beryllina](http://en.wikipedia.org/wiki/Inland_silverside), [Fundulus grandis](http://en.wikipedia.org/wiki/Gulf_killifish) and [Anchoa mitchilli](http://en.wikipedia.org/wiki/Anchoa_mitchilli)) that compete for food resources and whose performance depends on bathymetrical conditions and tide dynamics. To run the software, compile it and type:
> delta *TOPOFILE* *WATERROUTE* *OUTPUTROUTE* *SIMYEARS*

where:

* *TOPOFILE* is the file containing the bathymetric description of the simulated habitat.
* *WATERROUTE* is the route of the directory where the files describing the tide dynamics are located.
* *OUTPUTROUTE* is the route of the directory where the software will store the output files.
* *SIMYEARS* is the number of years to simulate.

This software is available under the GNU General Public License (GPL) version 3.0.
