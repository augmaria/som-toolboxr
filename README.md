som-toolboxr
============

R scripts for exploratory data analysis using SOM. Frequently, I start working on a dataset which I know little about (in terms of noise, feature relevance, natural groupings) and need something to give me a 'picture' of how the data behaves.

That is why I created these scripts that can take a data file, normalize it, check for data inconsistencies and train an SOM. They also plot component plane visualizations, u-matrix and hit histograms. Hopefully I will be able to get it to a state where I can do as much as I can do with Matlab som_toolbox and even more.

Running
--------
Start R and change to the current directory. Run the script

      test1.R

Which will take the scripts through both labeled and unlabeled data scenarios using iris dataset.

Platforms
---------

Should run on platforms that has R installed. kohonen and colorspace packages must be installed. 
