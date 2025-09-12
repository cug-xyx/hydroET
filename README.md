# hydroET

An R package related to evapotranspiration research

## Installation

You can install the hydroET from github:

```R
remotes::install_github('cug-xyx/hydroET')
```

## (Potential) evapotranspiration models

| Function name    | Model name                                                            | Status |
| ---------------- | --------------------------------------------------------------------- | ------ |
| `ET_CR_Ma`       | Calibration-free complementary relationship (CR) model                | ❌     |
| `PET_Penman1948` | Penman 1948 model                                                     | ✅     |
| `PET_PT1972`     | Priestley-Taylor model                                                | ✅     |
| `PET_Yang2019`   | Maximum evaporation model                                             | ✅     |
| `PET_Zhou2024`   | *Energy-based PET* (PETe)                                             | ✅     |
| `ET_summary`     | A `data.frame` containing various types of evapotranspiration results | ✅     |

## TODO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [x] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)
- [ ] Check `ET_CR_Ma` and `ET_CR_Ma_ref`, which one is the correct algorithm
- [ ] Develop convenient functions to directly download ET/PET forcing data on `GEE`
  - [x] site scale
  - [ ] grid scale
- [x] Maximum Evaporation Theory (Yuting Yang, ML Roderick, Zhuoyi Tu)
- [x] PET algorithm developed by Zhou & Yu (2024)
- [ ] Develop `cpp` version functions
- [ ] Using `use method` to process `SpatRaster` object
- [ ] Add references in `doc`
- [ ] Deploy `gh-pages`
- [ ] Revisiting `rgee`
