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


## TODO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [ ] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)
- [ ] Maximum Evaporation Theory (Yuting Yang, Zhuoyi Tu)
- [ ] PET algorithm developed by Sha Zhou