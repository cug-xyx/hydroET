# hydroET

An R package related to evapotranspiration research

## Installation

You can install the hydroET from github:

```R
remotes::install_github('cug-xyx/hydroET')
```

## (Potential) evapotranspiration model

| Function name   | Model name                                                   | Type |
| --------------- | ------------------------------------------------------------ | ---- |
| `ET_CR_Ma`      | **Calibration-free complementary relationship (CR) model**   | ET   |
| `ET_Penman1948` | Penman 1948 model                                            | PET  |
| `ET_PT1972`     | Priestley-Taylor model                                       | PET  |
| `ET_summary`    | A **tibble** containing various types of evapotranspiration results |      |

## Others functions

| Function name           | Description                                                  |
| ----------------------- | ------------------------------------------------------------ |
| `TM4TIF`                | Add **time** attribute to **raster** object (personal function) |
| `ggRunoff`              | Drawing **rainfall runoff hydrograph** based on **ggplot2**  |
| `exact_separate`        | Enhanced `tidyr::separate` function                          |
| `bind_array_layer`      | Merge layers of the same shape **array**                     |
| `previous_value_interp` | Interpolate with the **previous value**                      |
| `separate2`             | Enhanced `tidyr::separate` function by `data.table`          |
| `facet_subgraphs`       | add subgraphs to `ggplot2` **facet object**                  |

## crawlerTools

| Function name          | Description                                                  | Url                                 |
| ---------------------- | ------------------------------------------------------------ | ----------------------------------- |
| `GETZQData`            | get **hydrological data** (water level and runoff) from Hubei by station code | http://113.57.190.228:8001/         |
| `get_NCBI_info`        | get **NCBI** sample information                              | https://www.ncbi.nlm.nih.gov/       |
| `get_protein_homologs` | get protein homologs from **JGI Phytozome**                  | https://phytozome-next.jgi.doe.gov/ |
| `get_plantintron_data` | get data from **Plant Intron Splicing Efficiency Database**  | https://plantintron.cn/arabidopsis/ |

## TO DO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [x] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)
- [x] Link to Rstudio server
- [ ] Maximum Evaporation Theory (Yang)
- [x] `facet_wrap`+`geom_son_bar`

## References

[Ning Ma, 2021, Calibration-Free Complementary Relationship Estimates Terrestrial Evapotranspiration Globally](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021WR029691)
