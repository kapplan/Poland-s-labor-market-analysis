# Poland's Labour Force Analysis

This project aims to analyze the labor market in Poland over the span of 10 years, from 2012 to 2022, focusing on employment patterns, educational attainment levels, and monthly earnings. The data was manually collected from ILOSTAT, filtering for Poland, and includes the latest available data up to 2022. The goal is to gain insights into gender inequality, education patterns, and changes in the labor market over the specified period.

## Table of Contents

- [Introduction](#introduction)
- [Datasets](#datasets)
- [Analyses](#analyses)
  - [Employment Trends](#employment-trends)
  - [Educational Attainment](#educational-attainment)
  - [Educational Mismatch](#educational-mismatch)
  - [Occupation Analysis](#occupation-analysis)
  - [Monthly Earnings](#monthly-earnings)
  - [Weekly Hours Worked](#weekly-hours-worked)
- [Key Findings](#key-findings)
- [Installation](#installation)
- [Usage](#usage)
- [License](#license)

## Introduction

This analysis explores the evolving dynamics of the Polish labor force within the context of the economic and political landscape. By examining factors such as employment patterns, educational attainment, and earnings, this project aims to shed light on gender inequality, education trends, and labor market changes in Poland from 2012 to 2022.

## Datasets

The following datasets were used in this analysis:

- **data_combined.csv**: Contains data on the working-age population and employment ratio by sex and age for Poland.
- **meanhoursbysexoccupation.csv**: Contains data on mean weekly hours worked by sex and occupation for Poland.

## Analyses

### Employment Trends

Analyzes employment trends by year, sex, and population. Merges population and employment ratio data to calculate total employment and average employment ratio.

### Educational Attainment

Examines the distribution of education levels by age bands and sex. Groups education levels and visualizes the distribution across different age groups.

### Educational Mismatch

Investigates educational mismatch by gender over time. Analyzes the proportion of the population that is overeducated, undereducated, or has matched education levels.

### Occupation Analysis

Explores what occupations males and females work in and their respective employment counts. Focuses on different ISCO-08 occupations.

### Monthly Earnings

Analyzes average monthly earnings by sex and education level. Focuses on various job titles and visualizes the changes in earnings over time.

### Weekly Hours Worked

Examines trends in mean weekly hours worked in various occupations over time, with a breakdown by gender.

## Key Findings

- **Employment Trends**: A slight but steady increase in the working-age population that is employed, with men maintaining a higher employment count. A visible decline in total employment in 2019 due to the COVID-19 pandemic.
- **Educational Attainment**: A significant proportion of the population has attained upper secondary education. Younger females show higher attainment of higher education levels.
- **Educational Mismatch**: Overeducation is more prevalent among females, while males show a higher percentage of matched education levels.
- **Occupation Analysis**: Different occupations show varying trends in employment counts and gender distribution.
- **Monthly Earnings**: Earnings have generally increased over time, with a persistent gender pay gap in most occupations.
- **Weekly Hours Worked**: Weekly hours worked vary across occupations, with some fluctuations due to external economic factors.

## Installation

1. Clone the repository:
   ```sh
   git clone https://github.com/your-username/polands-labour-force-analysis.git
   ```

2. Install requirements:
   ```sh
   pip install -r requirements.txt
   ```

## Usage
1. Navigate to the project directory:
```sh
cd polands-labour-force-analysis
```

2. Open the Jupyter Notebook:
```sh
jupyter notebook poland-s-labour-force-analysis-in-r.ipynb
```

## License
This project is licensed under the MIT License. 
