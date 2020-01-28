# QDiabetes

## General info

This project is an R package for calculating the 10-year risk of type 2 diabetes using the [QDiabetes^&#174;^-2018 algorithm](https://qdiabetes.org). The QDiabetes^&#174;^-2018 algorithm is actually six separate risk predictions models (three sub-models, subdivided by gender).

### Model A

The basic (core) model, 'model A', uses the following 16 variables to calculate risk:

* **Gender**
* **Age** (years)
* **Ethnicity**, nine categories:
    - White or not stated
    - Indian
    - Pakistani
    - Bangladeshi
    - Black Caribbean
    - Black African
    - Chinese
    - Other Asian
    - Other ethnic group
* **Deprivation** (as measured by Townsend scores, where higher values indicate higher levels of deprivation)
* **Body mass index** (kg/m^2^)
* **Smoking status**, five levels:
    - Non-smoker
    - Ex-smoker
    - Light smoker (1--9/day)
    - Moderate smoker (10--19/day)
    - Heavy smoker (&ge;20/day)
* **Family history of diabetes in first degree relative**
* **History of learning disabilities**
* **History of schizophrenia or bipolar affective disorder**
* _**History of gestational diabetes** (women only)_
* _**History of polycystic ovary syndrome** (women only)_
* **History of cardiovascular disease** (ischaemic heart disease, stroke, or transient ischaemic attack)
* **History of treated hypertension** (diagnosis of hypertension and current treatment with at least one hypertensive drug)
* **History of use of corticosteroids** (British National Formulary chapter 6.3.2, including oral or injections of systemic: [prednisolone](https://en.wikipedia.org/wiki/Prednisolone), [betamethasone](https://en.wikipedia.org/wiki/Betamethasone), [cortisone](https://en.wikipedia.org/wiki/Cortisone), depo-medrone, [dexamethasone](https://en.wikipedia.org/wiki/Dexamethasone), [deflazacort](https://en.wikipedia.org/wiki/Deflazacort), efcortesol, [hydrocortisone](https://en.wikipedia.org/wiki/Hydrocortisone), [methylprednisolone](https://en.wikipedia.org/wiki/Methylprednisolone), or [triamcinolone](https://en.wikipedia.org/wiki/Triamcinolone))
* **History of use of second generation "atypical" antipsychotics**, including: [amisulpride](https://en.wikipedia.org/wiki/Amisulpride), [aripiprazole](https://en.wikipedia.org/wiki/Aripiprazole), [clozapine](https://en.wikipedia.org/wiki/Clozapine), [lurasidone](https://en.wikipedia.org/wiki/Lurasidone), [olanzapine](https://en.wikipedia.org/wiki/Olanzapine), [paliperidone](https://en.wikipedia.org/wiki/Paliperidone), [quetiapine](https://en.wikipedia.org/wiki/Quetiapine), [risperidone](https://en.wikipedia.org/wiki/Risperidone), [sertindole](https://en.wikipedia.org/wiki/Sertindole), and [zotepine](https://en.wikipedia.org/wiki/Zotepine)
* **History of use of statins**

### Model B

'Model B', uses the same variables as model A, with the addition of:

* **Fasting plasma glucose level** (mmol/L)

### Model C

'Model C', uses the same variables of model A, with the addition of:

* **Glycated haemoglobin A~1c~ value** (mmol/mol)


## Installation

You can install the development version from [GitHub](https://github.com/) with:

```R
if (!{"devtools" %in% installed.packages()[, "Package"]}) install.packages("devtools")
devtools::install_github("Feakster/qdiabetes")
```

## Example

Here are some very simple examples of using the _QDiabetes_ package:

```R
### Load Package Namespace ###
library(QDiabetes)

### Simple Usage ###
QDRA(gender = "Male", age = 45, bmi = 35)
# [1] 9.88593
QDRB(gender = "Female", age = 65, bmi = 30, fpg = 6)
# [1] 18.43691
QDRC(gender = "Male", age = 25, bmi = 40, hba1c = 42)
# [1] 8.226301

### Making Use of Vectorisation ###
QDRA(gender = "Female", age = seq(25, 75, 10), bmi = 35)
# [1]  1.085179  2.921454  5.893499  9.082108 10.713717  9.567516
QDRB(gender = "Male", age = 65, bmi = 35, fpg = 2:6)
# [1]  0.9123063  0.5911511  1.8416081  7.8554831 30.8096968
QDRC(gender = "Female", age = 80, bmi = 28, hba1c = seq(15, 45, 5))
# [1]  0.008084487  0.033019655  0.121238952  0.412396004  1.320727239  4.005759509 11.409509026
```

## Known issues

See [Issues](https://github.com/Feakster/qdiabetes/issues) on the QDiabetes GitHub repository.
