# Isabelle Linter
Linter for Isabelle, with jEdit integration.

## Setup
Requires Isabelle >= `2021-1-RC0`.
The linter can be used as a stand-alone tool or as a jEdit component.

Install with: `isabelle components -u <DIR>`.

For stand-alone (cli) tool only, add the component `<REPO_DIR>/linter_base` **instead**.

## Usage
Automatically starts with jEdit.
Configuration can be done via the `Linter` panel and/or Isabelle [options](etc/options).

CLI usage: `isabelle lint -?`.

## Lints
TODO: Generate list of lints.