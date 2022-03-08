
# dyreson_etal_2021_earthsfuture
__The Role of Regional Connections in Planning for Future Power System Operations under Climate Extremes__  
*Ana Dyreson<sup>3</sup>, Naresh Devineni<sup>5</sup>,Sean Turner<sup>1</sup>,  Thushara DeSilva<sup>3</sup>, Ariel Miara<sup>3</sup>, Nathalie Voisin<sup>1,2</sup>,Stuart Cohen<sup>3</sup>, Jordan Macknick<sup>3</sup>  
1 Pacific Northwest National Laboratory, Seattle, WA  
2 University of Washington, Seattle, WA  
3 National Renewable Energy Laboratory, Golden, CO  
4 Pacific Northwest National Laboratory, Richland, WA  
5 City College of New York, New York, New York
\* corresponding author: adyreson@mtu.edu

## Abstract
Extreme climate events can negatively impact power system operations. Identifying the sensitivity of a power system to climate extremes must consider future infrastructure changes. We investigate the sensitivity of a historic Western U.S. power system (5% variable renewable penetration of energy) and a future system (31%) to compound drought and heat wave events.  We use an electricity operational model combined with a model of historically extreme drought (for hydropower and freshwater-reliant thermoelectric generators) over the Western U.S. and a synthetic, regionally extreme heat event focused on Southern California (for thermoelectric generators and electricity load). We find that drought has the highest impact on summertime production cost (+10 to +12%), while temperature-based deratings have minimal effect (at most +1%).  The heat wave scenario impacting load increases the summertime regional net imports to that region by 10 to 14%, while the drought decreases them by 5 to 12%. Combined heat and drought conditions have a moderate effect on imports to Southern California (-2%) in the historic system and a stronger effect (+8%) in the future system. Southern California dependence on other regions decreases in the summertime with the moderate increase in variable renewable energy (-34% imports), but hourly peak regional imports are maintained under those infrastructure changes. Using a novel method that combines synthetic and historically-driven conditions to test multiple infrastructures, we consolidate the importance of considering compounded heat wave and drought in planning studies and show that region-to-region energy transfers during peak periods are key to optimal operations under climate extremes.

## Code reference
The R code in this repository can be used to generate the figures and results as discussed in the paper.

## Journal reference
TBD

## Data reference

### Input data
1) water-constrainted generation capacity and monthly energy targets for production cost modeling (PNNL)
2) temperature and precipitation results from 100 simulations
3) derating inputs: dry bulb and wet bulb temperature based on four selected simulations and county design point temperature for thermoelectric generation
4) load inputs: hourly county electricity demand temperature sensitivites (slopes), hourly county electricity demand for ten scenarios (baseline year + four simulated years for low VG and moderate VG infrastructures; moderate VG infrastrcutures also incorporate load growth)

### Output data
1) Selected production cost model results aggregated to county or reserve sharing group.

## Contributing models
| Model | Version | Repository Link | DOI |
|-------|---------|-----------------|-----|
| WM | 1.0.0 | https://github.com/IMMM-SFA/wm | http://doi.org/10.5281/zenodo.1225344 |
| PLEXOS | 8.0 | | |

## Reproduce my experiment
The inputs provided in this repository are inputs (or used to create inputs) for production cost modeling software to simulate electricity operations. See code reference above for analysis of those results.
