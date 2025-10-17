# hydroET

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![GitHub last
commit](https://img.shields.io/github/last-commit/cug-xyx/hydroET)

<!-- badges: end -->

An R package related to evapotranspiration research.

## Installation

You can install the hydroET from github:

```R
remotes::install_github('cug-xyx/hydroET')
```

## PET / ET models

| Function name    | Model name                                                            | Status | Language |
| ---------------- | --------------------------------------------------------------------- | ------ | -------- |
| `ET_CR_Ma`       | Calibration-free complementary relationship (CR) model                | ❌     | `R`      |
| `PET_Penman1948` | Penman 1948 model                                                     | ✅     | `cpp`    |
| `PET_PT1972`     | Priestley-Taylor model                                                | ✅     | `cpp`    |
| `PET_Yang2019`   | Maximum evaporation model                                             | ✅     | `cpp`    |
| `PET_Zhou2024`   | *Energy-based PET* (PETe)                                             | ✅     | `cpp`    |
| `ET_summary`     | A `data.frame` containing various types of evapotranspiration results | ❌     |          |

## Attribution Framework

- Elasticity Coefficient Method (ECM)

## TODO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [x] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)
- [ ] Check `ET_CR_Ma` and `ET_CR_Ma_ref`, which one is the correct algorithm
- [x] Develop convenient functions to directly download ET/PET forcing data on `GEE`
  - [x] site scale
  - [x] grid scale
- [x] Maximum Evaporation Theory (Yuting Yang, Zhuoyi Tu)
- [x] PET algorithm developed by Zhou & Yu (2024)
- [x] Develop `cpp` version functions
- [ ] Update `doc`
- [ ] Using `use method` to process `SpatRaster` object
- [ ] Add references in `doc`
- [ ] Deploy `gh-pages`

## References

- Ma, Ning, Jozsef Szilagyi, and Yongqiang Zhang. “Calibration‐Free Complementary Relationship Estimates Terrestrial Evapotranspiration Globally.” *Water Resources Research* 57, no. 9 (2021). https://doi.org/10.1029/2021WR029691.
- Tu, Zhuoyi, and Yuting Yang. “On the Estimation of Potential Evaporation Under Wet and Dry Conditions.” *Water Resources Research* 58, no. 4 (2022). https://doi.org/10.1029/2021WR031486.
- Zhou, Sha, and Bofu Yu. “Physical Basis of the Potential Evapotranspiration and Its Estimation over Land.” *Journal of Hydrology* 641 (September 2024): 131825. https://doi.org/10.1016/j.jhydrol.2024.131825.
- Zhou, Sha, and Bofu Yu. “Neglecting Land–Atmosphere Feedbacks Overestimates Climate-Driven Increases in Evapotranspiration.” *Nature Climate Change*, 1–8 (September 11, 2025). https://doi.org/10.1038/s41558-025-02428-5.

