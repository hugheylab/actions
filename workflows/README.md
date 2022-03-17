These files should be placed in the `.github/workflows` directory of all lab R packages to configure GitHub Actions.

## R Package Repositories

In lab R package repositories, you should only use the following workflow configurations:

1. `check-deploy.yaml`
2. `pkgdown.yaml`
3. `test-coverage.yaml`
4. `lint-package.yaml` (`custom-lint-script.R` must also be in the workflows directory)
  - This workflow runs `lintr` on the package using only the linters that align with lab style policies.
  - To modify the linters to be run, edit the `custom-lint-script.R` to disable, enable, modify, or add different linters.
  - See [lintr](https://github.com/r-lib/lintr) for more details.

## Analysis repositories

In coding/data sprint analysis repositories, only the `analysis-workflowr.yaml` file is necessary. You shouldn't normally need this, however, since the GitHub template comes with it already included!
