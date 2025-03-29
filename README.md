# Movement Explorer

Updated: March 28, 2025

The Movement Explorer website consists of a set of R workflows and shiny apps to enable users to analyse movement and habitat selection using telemetry data. The methods and tools are intended to be simple to use and additional resources are provided under each step for those interested in pursuing additional (and sometimes) more complex analyses.


## Step 1 - Cleaning movement data

Data preparation and cleaning is an essential first step prior to doing and data exploration and modelling. There are several resources to help with this step, including:

- Movebank
- MoveApps
- amt package workflow
- ExMove
- can't remember, but see tools paper


## Step 2 - Defining seasons and migration periods

Movement and habitat selection often vary by season and migration periods. Consequently, it is important to identify seasons and migration periods based on local data or regional expert knowledge, or a combination of both. To assist with this task, we developed a simple tool which enables users to view their movement data and identify start and end periods for each season and migration period of interest.

**Workflow**


**Shiny app**

>shiny::runGitHub(repo="beaconsproject/movement_explorer", subdir="seasonal_explorer")

Other methods and tools exist to perform similar analyses, including:

- Migration Mapper tool
- TuktuTools and TuktuMigration


## Step 3 - Exploring range and occurrence distributions

>shiny::runGitHub(repo="beaconsproject/movement_explorer", subdir="home_range_explorer")


## Step 4 - Developing species habitat models (HSF, iSSF)


