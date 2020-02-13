# UC-SEC Method Study

This is the repository for the UC-SEC method paper. This repository contains the cleaned data, analysis scripts, exploratory reports, shiny apps, and final figures for manuscript.

## The structure of the repository

There are three directories at the to-level folder, `data`, `data-raw`, and `src`. The `data-raw` folder has the raw data. The entire folder is ignored in the `.gitignore` unless specified. The data cleaning and the cleaned data is put in the `data` folder. All scripts and reports are put in the `src` folder.

## Common usage

Clone the repository:

```bash
git clone https://github.com/zhuchcn/sec-method-study.git
```

To creat a new branch of your own:

```bash
git branch branch-name
```

The branch name must not have any space. And the branch name should be descriptive to what is being done in this branch.

To add a commit:

```bash
# make sure your are at the top-level directory of the repository.
git add .
git commit -m "your commit message (please be descriptive)"
```

To push a commit to the master branch (however, please avoid do this):

```bash
git push origin branch-name
```

Pull from the master branch. This is done when the remote origin is ahead of your local node.

```bash
git pull origin master
```

To switch between branches:

```bash
git checkout branch-name
```

## Tips

+ In order to keep the master branch clean, please do not push directly to it. Please push to your own branch and submit a pull request on github.

+ Be careful of the files you add to the commit. `git add .` command above could be dangerous because it adds whatever files to the stage. Please don't commit files that are too large, and also avoid committing files that are not in text format, such as zipped file and word document unless you have too. Pictures are fine.

+ Please keep the structure clean by putting files in where they should be.
