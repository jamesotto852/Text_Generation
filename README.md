# Text Generation

This repository corresponds to a [series of blog posts](https://jamesotto852.github.io/text-generation-1) on generating text using recurrent neural networks using data from [Project Gutenberg](https://www.gutenberg.org/). 
The `/R/` directory has the code necessary for downloading and transforming data, as well as building and making predictions with models via the [R interface to Keras](https://keras.rstudio.com/).
In order to build the RNN models, it is necessary that you be using a Python environment with at least tensorflow v2.7.0 installed.

Note that the data and models have been excluded from this repo for their size. 
Fortunately, the provided functions simplify the process of downloading data and training the models.
