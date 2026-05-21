# Proposal for Semester Project


<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github. 
Please do not Rename this file (Readme.md has a special meaning on GitHub).

quarto render Readme.md --to pdf
-->

**Patterns & Trends in Environmental Data / Computational Movement Analysis / Geo 880**

| Semester:      | FS26                                                                      |
|:---------------|:------------------------------------------------------------------------- |
| **Data:**      | Self recorded movement data of Vitaparcours tracks                        |
| **Title:**     | Stop detection in Vitaparcours movement data using segmentation algorithms|
| **Student 1:** | Judith Winet                                                              |
| **Student 2:** | Rodrigo Nogueira Silva                                                    |

## Abstract 
<!-- (50-60 words) -->
This project investigates the effectiveness of the Laube and Purves (2011) segmentation algorithm at detecting stops in Vitaparcours movement data. The algorithm’s precision is evaluated against the “real” GPS locations by comparing the trajectories recorded by three mobile apps (Posmos, Strava and SwissTopo). The aim is to assess the impact of provider specific sampling and potential environmental influences.

## Research Questions
<!-- (50-60 words) -->

- Can the Vitaparcours exercise stops be efficiently detected in movement data from different providers using the segmentation algorithm in Laube and Purves (2011)?
- How well do modelled exercise stops match the real location of these stops for movement trajectories of different providers?
- How do trees influence the data accuracy of movement data?

## Results / products
<!-- (50-100 words) -->
<!-- What do you expect, anticipate? -->
We expect to be able to detect stops using movement data from all the different providers. However, due to noise and measurement uncertainties in connection with the environment, there may be some discrepancies. The stops will probably not be modelled in the exact same locations for the two providers per trail, but we expect only minor differences.  

## Data
<!-- (100-150 words) -->
<!-- What data will you use? Will you require additional context data? Where do you get this data from? Do you already have all the data? -->
For this semester project, we will record our movements through a Vitaparcours. Each of us will complete one course, recording the movements with two different apps and mark the stops on a map. One person will use the PosmosProject and SwissTopo apps and another will use the Strava and SwissTopo apps.

We will not need any additional data as we only record our own movements through the course. But it might be helpful to also look up the stop locations online so that we can compare them with the exact location data. This data can be found on the website of Zurich Vitaparcours.

We do not have all the movement data yet but we will record this in the week between the 18.05.2026 and the 24.05.2026.

## Analytical concepts
<!-- (100-200 words) -->
<!-- Which analytical concepts will you use? What conceptual movement spaces and respective modelling approaches of trajectories will you be using? What additional spatial analysis methods will you be using? -->
For this project, we will do a stop detection analysis using a constrained Euclidean conceptual movement space, defined more or less by the Vitaparcours trail. For trajectory modelling, we will implement the segmentation algorithm approach described in Laube and Purves (2011), focusing on identifying static events in the movement data. To compare the static movements with the real Vitaparcours stops, we will record separately a GPS signal with the coordinates on every stop. Additionally, we will compare the number of stop detections and accuracies of the static events of the tracks to the actual GPS data of the stops. To compare the GPS signal at a stop with an actual point instead of just slow/static walking around, we may also have to use some additional spatial analysis methods like special clustering (e.g. calculating the weighted mean). 

## R concepts
<!-- (50-100 words) -->
<!-- Which R concepts, functions, packages will you mainly use. What additional spatial analysis methods will you be using? -->
We will mainly use the packages readr, sf and dplyr as well as tmap and ggplot2 for visualisation. For clustering our static event to a single point we may use dbscan and for the weighted mean the spatstat.geom package, but as we have no experience on working with these spatial analysis methods on RStudio, the packages can still change. 

## Risk analysis
<!-- (100-150 words) -->
<!-- What could be the biggest challenges/problems you might face? What is your plan B? -->
The biggest challenges are mainly GPS related issues, as Vitaparcours tracks are often located in forests, where the accuracy of both the movement data and the GPS signal for tracking the stops may be affected by uncertainties. Tiredness could also lead to extended pause times or slower movement throughout the track, which could be falsely detected as a static event. These challenges can be resolved by adjusting thresholds and conducting a exact analysis of the raw data. Therefore, plan B will probably be to accept lower level of accuracy.

Another potential challenge could be ensuring the data comparability between the three apps. We do not know yet whether the headers, units, etc. are the same and comparable. This can be solved with further data preparation, but it will require additional effort.

## Questions? 
<!-- (100-150 words) -->
<!-- Which questions would you like to discuss at the coaching session? -->
-	How should we best handle the varying sampling frequencies between the different apps?
-	Do we need to implement some pre-processing filters like e.g. smoothing?
-	Do you have any clustering tips for RStudio? 
-	Is it a good idea to calculate the weighted mean to have a single point on the static event or is there a better option? 
-	What is the recommended spatial buffer distance when determining if a modelled stop matches a “ground truth” stop in a forest environment?



hello hello