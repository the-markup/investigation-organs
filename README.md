# How We Investigated UNOS’s Liver Allocation Policy and Inequities in the Liver Transplant System 

Two investigations from The Markup/The Washington Post

---

This repository contains code to reproduce the findings featured in [our investigation on liver transplants](https://themarkup.org/organ-failure/2023/03/21/poorer-states-suffer-under-new-organ-donation-rules-as-livers-go-to-waste), which is described in detail in [our methodology](https://themarkup.org/show-your-work/2023/03/21/how-we-investigated-unoss-liver-allocation-policy). It also contains code to reproduce the findings of [a subsequent investigation on racial inequities in the liver transplant system](https://themarkup.org/organ-failure/2024/02/08/a-death-sentence-native-americans-shut-out-of-the-nations-liver-transplant-system), described in detail in [a separate methodology](https://themarkup.org/show-your-work/2024/02/08/how-we-investigated-racial-disparities-in-liver-transplants).

For the sake of convenience, we've already commited the outputs of our analyses, which you can view in the `02_analysis/output` folder. If you want to rerun the analysis and verify the steps yourself, see the **Run analysis** section.

### Data

In this repository the data used as the input for analysis is stored in the `01_data` folder, along with the code used to clean the raw data. We've included the raw data as well as our cleaned data, found in `01_data/raw` and `01_data/clean`, respectively. Some files will need to be uncompressed first. To do that you can run:

```
find . -name '*.gz' -print0 | xargs -0 gunzip --keep
```

Within the `01_data/raw` folder, you can find the following raw data sources:

| Data source | Description |
|-------------|-------------|
| `00_misc/cb_2021_us_state_500k` | A folder of geographic datasets from the U.S. Census Bureau defining the boundaries of states and territories. |
| `00_misc/state_code_mapping_20221022.csv` | A crosswalk we created outlining each state's name, FIPS, and Census Bureau region. |
| `01_hospitals/22F138_Institutions.csv` | A data table with information on name, location (city, state, zip), provider ID, and type of each center (hospital, lab, OPO) involved with transplants. Referred to as "institution data" in the methodology. |
| `01_hospitals/DONORHOSPITAL_22F168Records(1of3)-redacted.csv` | A data table identifying which donors were associated with which hospital, lab, and/or OPO. Referred to as “donor-hospital crosswalk” in the methodology. |
| `01_hospitals/DONORHOSPITAL_22F168Records(2of3)-redacted.csv` | A data table containing the latitude and longitude of each facility. Referred to as “facility coordinate data” in the methodology. | 
| `02_candidate/Updated_CAN_LIIN_22F218Records(1of2)-redacted.csv` | A dataset of information on candidates on the waitlists for liver and intestine transplant. Referred to as the "candidate data" in the methodology.  |
| `03_donor/22F123_Records_Redacted.csv` | A dataset of information on liver donors. Referred to as the "donor data" in the methodology. |
| `04_transplant/TX_LI_22F218Records(2of2)-redacted.csv` | A dataset on liver transplants. Referred to as the "transplant data" in the methodology. |
| `05_cdc_wonder/causes_of_death` | A folder of data files from the CDC Wonder database on major causes of death. |
| `05_cdc_wonder/liver_disease_rates` | A folder of data files from the CDC Wonder database on the prevelance of chronic liver disease and cirrhosis. |
| `06_don_disp/DONDISPOSITION_22F167Records-redacted.csv` | A dataset of information on organ donor disposition. Referred to as "donor disposition data" in the methodology. |
| `07_optn_tx_counts/OPTN-Transplants_in_the_U.S._by_State-20230216.csv` | A data table from OPTN on the number of liver transplants performed. Downloaded on 02/16/2023. |

Within the `01_data/clean` folder, you can find the following clean data sources (produced with `01_master_organs_data_clean.R`):

| Data source | Description |
|-------------|-------------|
| `all_candidate_clean.rds` | A cleaned version of the candidate data, including candidates under 18 years old. |
| `candidate_clean.rds` | A cleaned version of the candidate data, excluding children. The primary "candidate data" used for analysis. |
| `cdc_wonder_clean.rds` | A cleaned version of the data from `raw/05_cdc_wonder/liver_disease_rates`. |
| `don_disp_clean.rds` | A cleaned version of the donor disposition data. |
| `donor_clean.rds` | A cleaned version of the donor data. |
| `optn_tx_counts_clean.rds` | A cleaned version of the data table from `raw/07_optn_tx_counts`. |
| `transplant_clean.rds` | A cleaned version of the transplant data. |

### Setup

To run the analysis

- Install the following packages in RStudio using `install.packages`:
  - `tidyverse`
  - `tidylog`
  - `janitor`
  - `lubridate`
  - `here`
  - `patchwork`
  - `gridExtra`
- To run the Jupyter notebooks with R Kernel, use the `IRkernel` package with `IRkernel::installspec()` (see [here](https://towardsdatascience.com/how-to-run-r-scripts-in-jupyter-15527148d2a) for more)

### Run analysis

You can find our analysis within the `02_analysis` folder. Each analysis is contained with in its own Jupyter notebook, which you can review and run to replicate our analysis. 

These notebooks include:

| Notebook | Description |
|----------|-------------|
| `01_transplants_by_state.ipynb` | An analysis of the state-level trends in transplants performed per year in each state. |
| `02_per_capita_rates.ipynb` | An analysis of the per-capita rates in each state of chronic liver disease deaths, liver transplants, liver donations, and additions to the liver waitlist. |
| `03_liver_import_export_flow.ipynb` | An analysis of the flow of livers imported and exported between states. |
| `04_donor_candidate_distance.ipynb` | An analysis of the distance traveled by livers from donor to candidate. |
| `05_discards.ipynb` | An analysis of discarded livers. |
| `06_donor_cause_of_death.ipynb` | An analysis of the major causes of death among liver donors and in the United States broadly. |
| `07_comp_tx_chg_across_periods.ipynb` | An analysis comparing the number of transplants performed based on variable time period selection. |
| `08_covid.ipynb` | An analysis of the impacts of COVID-19 on the number of transplants performed nationally. |
| `09_racial_discrepancies.ipynb` | An analysis, by racial group and state, of how deaths from liver disease compare to transplant and transplant waitlisting (added with second investigation). |
| `10_ihs_unmet_need_reports.ipynb` | An analysis of deferrals for liver-related care by the Indian Health Service (added with second investigation). |

Each notebook will generate analysis outputs (figures and tables), which can be found within `02_analysis/output`.

### Update log

- 2024-02-08: For our second investigation, into racial inequities in the transplant system, we produced two additional analysis notebooks, `09_racial_discrepancies` and `10_ihs_unmet_need_reports.ipynb`, described in the table above. These were added to this repo in the existing `02_analysis` folder. We also added data obtained from IHS regarding unmet service requests, which are analyzed in the `10_ihs_unmet_need_reports.ipynb` notebook - see the `01_data/raw/08_ihs_unmet_need` folder. For more, see [this pull request](https://github.com/the-markup/investigation-organs/pull/3).