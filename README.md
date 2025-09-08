# hydroET

An R package related to evapotranspiration research

## Installation

You can install the hydroET from github:

```R
remotes::install_github('cug-xyx/hydroET')
```

## (Potential) evapotranspiration model

| Function name   | Model name                                                            | Type |
| --------------- | --------------------------------------------------------------------- | ---- |
| `ET_CR_Ma`      | Calibration-free complementary relationship (CR) model                | ET   |
| `ET_Penman1948` | Penman 1948 model                                                     | PET  |
| `ET_PT1972`     | Priestley-Taylor model                                                | PET  |
| `ET_summary`    | A `data.frame` containing various types of evapotranspiration results |      |


## TODO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [x] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)
- [ ] Check `ET_CR_Ma` and `ET_CR_Ma_ref`, which one is the correct algorithm
- [ ] Develop convenient functions to directly download ET/PET forcing data on `GEE`
- [ ] Maximum Evaporation Theory (Yuting Yang, Zhuoyi Tu)
- [ ] PET algorithm developed by Sha Zhou
- [ ] Using `use method` to process `SpatRaster` object