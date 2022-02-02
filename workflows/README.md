These files should be placed in the `.github/workflows` directory of all lab R packages to configure GitHub Actions.

## R Package Repositories

In lab R package repositories, you should only use the following workflow configurations:

1. `check-deploy.yaml`
2. `pkgdown.yaml`
3. `test-coverage.yaml`

## Analysis repositories

In coding/data sprint analysis repositories, only the `analysis-workflowr.yaml` file is necessary. You shouldn't normally need this, however, since the GitHub template comes with it already included!
