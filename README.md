![](terminal.gif)

# Warm-up DBDD

This repository contains the R script (`tech-radar.R`) used to create the technology radar pilot as a SVG file. You can find versions of the technology radar in English, German, French and Italian in the corresponding folders, best by opening the corresponding `README.md` files.

The master is the table `data-English.csv`. It can be found in the subdirectory `English`. Any number of rows may be updated, but the header names must stay the same. Once the table is changed, it can be translated to the other languages by running the corresponding R script.

```bash
Rscript translator.R
```

The program runs a tripple for-loop to individually translate the content of cells to the different languages.

> [!NOTE]  
> In order to run the script `translator.R`, you need to [set up an OpenAI account](https://openai.com/product) and create an [API access key](https://platform.openai.com/). You then have to save the key in a file called `my-secret-API-key`.

Once the data is translated to all desired languages, you may run the main script.

```bash
Rscript tech-radar.R
```

This program will create a visualization of the technology radar content for every language, and a corresponding `README.md` file containing the legend of the different technologies in the visualization.

## Making the visualization with the data sets

The data sets in each sub-directory contain observations with name, description etc. as well as the coordinates of said observation in the plot. Because it's a circular plot, the polar coordinates are provided. They can quickly be converted to cartesian coordinates:

```math
x = R \; \sin(\varphi) \qquad \qquad y = R \; \cos(\varphi)
```

Where $R$ is the radius ($R \in [1,2]$), and $\varphi$ is the angle ($\varphi \in [-\pi,\pi]$).
