# hydroET

---

An R package related to evapotranspiration research

## Installation

You can install the hydroET from github:

```R
remotes::install_github('cug-xyx/hydroET')
```

## (Potential) evapotranspiration model

| Function name   | Model name                                                 | Type |
| --------------- | ---------------------------------------------------------- | ---- |
| `ET_CR_Ma`      | **Calibration-free complementary relationship (CR) model** | ET   |
| `ET_Penman1948` | Penman 1948 model                                          | PET  |
| `ET_PT1972`     | Priestley-Taylor model                                     | PET  |

## Others functions

| Function name | Description                                                  |
| ------------- | ------------------------------------------------------------ |
| `TM4TIF`      | Add **time** attribute to **raster** object (personal function) |
| `ggRunoff`    | Drawing **rainfall runoff hydrograph** based on **ggplot2**  |



## TO DO

- [x] Accuracy of inverse function to derive `Tws`and`Twb`
- [ ] Calibration of `alpha` (Appendix B in Ma and Szilagyi, 2019)

## References

[Ning Ma, 2021, Calibration-Free Complementary Relationship Estimates Terrestrial Evapotranspiration Globally](https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021WR029691)
