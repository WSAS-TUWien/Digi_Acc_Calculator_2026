#  Activity-Based Cost & GHG Calculator (W.S.A. Schwaiger, IMW/TU Wien)

Providing business logic for activity-based cost & GHG accounting for multi-level manufactuing systems and 
applying it to demonstration use case.

## Files

| File                           | What it does 
|--------------------------------|-------------------------------------------------------------------------------------
| **01_item_master_builder.R**   | generates item-IDs for raw materials, intermediate and finished products  
| **02_erp_mapper.R**            | maps itemIDs to raw material, intermediate and finished products
| **03_mis_graph_builder.R**     | builds the multi-level manufacturing information System (MIS) from BOM and routing
| **04_ais_recurison_engine.R**  | calculates the cost and GHG emssions for all elements in the MIS graph
| **05_analyzer.R**              | analyses direct and overhead costs and scope 1, 2, 3 emissions of finshed products
| **AB_Cost_GHG_Calculator.pdf** | pdf file generated from executing **AB_Cost_GHG_Calculator.Rmd** 
| **AB_Cost_GHG_Calculator.Rmd** | integrating all R, RData and png files, executing calculations and building pdf file
| **SlotCar.RData**              | data used in demonstration use case
| **SlotCar_BOM.png**            | Bill-Of-Material (BOM) figure of demonstration use case
| **SlotCar_MIS.png**            | MIS figure of demonstration use case
| **SlotCar_Routing.png**        | Routing figure of demonstration use case

> R files **`.R`** incorporate the business logic
> RData file **`.RData`** includes demonstration use case
> Rmd file **AB_Cost_GHG_Calculator.Rmd** applies business logic to demonstration use case and delivers results.

## Execution

> install R and R-Studio
> create new project in R-Studio
> copy all files into project directory
> run (knit) the **AB_Cost_GHG_Calculator.Rmd** file and install packages as automatically recommended
> view resulting pdf-file that contains all product cost and product carbon footprint of finished products
> investigate flexibility of calculator by using alternative MIS constellations via changing slot-cars' BOM and routing data

