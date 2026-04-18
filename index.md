# OmopStudyBuilder ![OmopStudyBuilder logo](reference/figures/image.jfif)

The **OmopStudyBuilder** package helps a study prepare for network
studies using the OMOP Common Data Model (CDM). The package sets up an R
project for your study with a default folder structure and template code
typically required for network studies. This allows you to focus on the
parts of the analysis that are specific to your study design.

In addition to project setup, the package reviews code and dependencies,
supports renv locking for consistent package versions, and can build and
run a Docker image for reproducible execution aligned with best
practices.

The package is highly opinionated and designed to align with the OxInfer
study code checklist. For further details, please refer to the
[documentation](https://oxford-pharmacoepi.github.io/Oxinfer/onboarding/code_review.html).

## Installation

You can install the development version of the package from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/OmopStudyBuilder")
```

## Quick Start

The main entry point is
[`initStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/initStudy.md),
which creates the study folder structure and template files.

``` r
library(OmopStudyBuilder)

initStudy(here::here("SampleStudy"))
```

Once the study has been created, you can review the generated code and
dependencies:

``` r
reviewStudyCode(here::here("SampleStudy", "studyCode"))
reviewStudyDependencies(here::here("SampleStudy", "studyCode"))
```

If you want reproducible package versions, initialise `renv` in the
study folder and snapshot the environment:

``` r
renv::init(here::here("SampleStudy", "studyCode"))
install.packages(c("dplyr", "CDMConnector", "IncidencePrevalence"))
renv::snapshot(here::here("SampleStudy", "studyCode"))
```

## Optional: Docker for reproducible execution

This package supports building and running study code in Docker for
reproducible execution. Docker is optional and only needed if you want
to build and run the study in a containerised workflow. Install Docker
and confirm it is running before using the Docker-based functions.

**General checks (all operating systems)**

- Install Docker.
- Start Docker (e.g., Docker Desktop).
- Verify Docker is available:
  - `docker --version`
  - `docker info`

**macOS**

- Install Docker Desktop for Mac:
  <https://docs.docker.com/desktop/setup/install/mac-install/>
- Open Docker Desktop and wait until it reports “Docker Desktop is
  running”.

**Windows**

- Install Docker Desktop for Windows (WSL 2 recommended):
  <https://docs.docker.com/desktop/setup/install/windows-install/>
- Ensure WSL 2 is enabled (Docker Desktop installer will guide you).
- Open Docker Desktop and wait until it is running.

**Linux**

- Install Docker Engine using your distribution instructions:
  <https://docs.docker.com/engine/install/>
- Start/enable the Docker service (varies by distro).
- If you want to run Docker without `sudo`, follow:
  <https://docs.docker.com/engine/install/linux-postinstall/>

### Example Usage

After the study has been created and configured, you can optionally
build a Docker image from the study folder:

``` r
dockeriseStudy(path = here::here("SampleStudy", "studyCode"))
```

Run the study interactively in RStudio Server or as an automated script.
If you use a `.env` file for credentials, pass `env_file = ".env"`.

``` r
runRStudio()
runStudy()
```

Use optional inputs only if you need them:

``` r
runStudy(
  image_name = "omop-study-study-code",
  data_path = "path/to/data",
  results_path = "./results"
)
```

To distribute the study, share the **study folder** created by
[`initStudy()`](https://oxford-pharmacoepi.github.io/OmopStudyBuilder/reference/initStudy.md)
(including `studyCode/` and `renv.lock`). Partners can build and run
using the same commands.

``` r
install.packages("OmopStudyBuilder")
library(OmopStudyBuilder)

dockeriseStudy(path = here::here("SampleStudy", "studyCode"))
runStudy()
runRStudio()
stopStudy()
```

To push a built image to Docker Hub, use the helper and enter your
credentials when prompted (the tag defaults to `latest`, and the image
name defaults to the current folder name):

``` r
pushDockerImage(
  repo = "yourname/omop-study-study-code"
)
```
