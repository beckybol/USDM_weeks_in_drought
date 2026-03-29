# U.S. Drought Monitor Analysis

## 📌 Overview
This repository is dedicated to accessing, processing, and analyzing data from the U.S. Drought Monitor (USDM). The analysis is entirely written in R, leveraging the `tidyverse` and spatial data packages to explore drought trends and impacts.

## ☁️ Environment Setup (GitHub Codespaces)
This project is configured to run in an isolated, cloud-based R environment using GitHub Codespaces. You do not need to install R or any dependencies on your local machine to run this code.

**To launch the environment:**
1. Click the green **"<> Code"** button at the top of this repository.
2. Select the **Codespaces** tab.
3. Click **"Create codespace on main"**.

*Note: The first time the codespace builds, it will automatically download R and install core packages like `tidyverse`, `sf`, and `ncdf4` via the `.devcontainer` configuration. Subsequent launches will take only a few seconds.*

## 📂 Project Structure
* `data/` - Raw and processed USDM datasets (ignored in version control)
* `scripts/` - R scripts for data extraction and analysis
* `.devcontainer/` - Configuration files for the cloud R environment

## 👤 Author
Becky Bolinger