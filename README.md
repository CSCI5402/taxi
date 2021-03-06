# taxi
New York City taxi data

![Alt Text](https://github.com/CSCI5402/taxi/blob/main/maps/rides.gif?raw=true)
## Team Members
(C) Joseph Froelicher, Tommy Guess, Michael Huffman  
CSPB 5402 - University of Colorado - Computer Science

## Project Description

Data available from:
https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page

The repository is organized as follows:
* python
* maps
* shapefiles

## Questions, Answers, and Applications

* How are rides distributed, geographically?
    Yellow taxi rides are heavily concentrated in lower Manhattan, where as Green taxi rides are distributed throughout the 5 boroughs. 
* How can we figure out the relationship or “association” between pickup regions and drop-off regions?
    Zones and boroughs are largely self-associated: Rides are more likely to end where they started. 
* What factors influence tipping? 
     Busy regions tip higher when all other variables are controlled for.
* How did COVID impact taxi rides in NYC?
     COVID had a devestating impact on taxi rides in NYC

The knowledge obtained in these domains would be useful for understanding the geographic and financial nature of the taxi cab industry in NYC. 

### Ride Association
To investigate the associations between pickup zones and dropoff zones, we choose to analyze data over a 1 yearperiod. A 1 year study period was chosen to maximize data collected, but limit temporal drift that these values nodoubt experience. The change of these values over time would be an interesting follow up study. To avoid the effects ofCOVID prejudicing our results, we picked 2019

### COVID-19
We were interested in visualizing the impact of COVID-19 on yellow-cab travel in New York City. This was done best by using maps, and additionally a plot of ride trends (pictured above). The lockdown clearly had significant impact on travel in the city, and this of course affects yellow cab data. The code for visualization for the yellow-cab data is housed in https://github.com/CSCI5402/taxi/tree/main/maps.

### Financial
The most interesting questions from a financial perspective had to do with tipping. In a nutshell, the research could be described with the following question: which factors were connected with higher tipping? Our analysis yielded several interesting results. First, airports tended to have high tips relative to the rest of the locations. While this was partially due to a higher ratecode, we also found that these locations had a higher tip amount as a percentage of the total fare. From here, we discovered that the most popular locations also tipped the most - a result that may be tied to better service or more well-known tip etiquette. More details are found in the project paper.

The applications of this are quite rich, especially if tips do in fact serve as a proxy for rider satisfaction. A taxicab company could potentially look at this data to see what areas/factor cause customers to be happier with their rides. Another way this could be applied would be to look for trends over time. This is an obvious extension which would be interesting to do in the future. 


## Link to Video
https://github.com/CSCI5402/taxi/blob/main/Group5_NewYorkCityTaxis_Part6_video.mp4

## Link to Final Paper
https://github.com/CSCI5402/taxi/blob/main/Group5_NewYorkCityTaxis_Part4.pdf




