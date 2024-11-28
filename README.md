# Neer Match <a href="https://r-neer-match.pikappa.eu"><img src="man/figures/logo.png" align="right" height="139" alt="neermatch website" /></a>

<!-- badges: start -->

![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)
![Downloads](https://cranlogs.r-pkg.org/badges/last-month/neermatch)
![Version](https://www.r-pkg.org/badges/version/neermatch)
![Release](https://www.r-pkg.org/badges/last-release/neermatch)
![Codecov test
coverage](https://codecov.io/gh/pi-kappa-devel/r-neer-match/graph/badge.svg)
<!-- badges: end -->

The package `neermatch` provides a set of tools for neural-symbolic
entity reasoning and matching. It is designed to support easy set-up,
training, and inference of entity matching models using deep learning,
symbolic learning, and a hybrid approach combining both deep and
symbolic learning. Moreover, the package provides automated fuzzy logic
reasoning (by refutation) functionality that can be used to examine the
significance of particular associations between fields in an entity
matching task.

The project is financially supported by the [Deutsche
Forschungsgemeinschaft](https://www.dfg.de/de) (DFG) under Grant
539465691 as part of the Infrastructure Priority Programme “[New Data
Spaces for the Social Sciences](https://www.new-data-spaces.de/en-us/)”
(SPP 2431).

The package has also an `Python` implementation available at
[py-neer-match](https://github.com/pi-kappa-devel/py-neer-match).

## Features

The package is built on the concept of similarity maps. Similarity maps
are concise representations of potential associations between fields in
two datasets. Entities from two datasets can be matched using one or
more pairs of fields (one from each dataset). Each field pair can have
one or more ways to compute the similarity between the values of the
fields.

Similarity maps are used to automate the construction of entity matching
models and to facilitate the reasoning capabilities of the package. More
details on the concept of similarity maps and an early implementation of
the package’s functionality (without neural-symbolic components) are
given by (Karapanagiotis and Liebald 2023).

The training loops for both deep and symbolic learning models are
implemented in [tensorflow](https://www.tensorflow.org) (Abadi et al.
2015). The pure deep learning model inherits from the
[keras](https://keras.io) model class (Chollet et al. 2015). The
neural-symbolic model is implemented using the logic tensor network
([LTN](https://pypi.org/project/ltn/)) framework (Badreddine et al.
2022). Pure neural-symbolic and hybrid models do not inherit directly
from the [keras](https://keras.io) model class, but they emulate the
behavior by providing custom `compile`, `fit`, `evaluate`, and
`predict`methods, so that all model classes in `neermatch` have a
uniform calling interface.

# Basic Usage

Implementing matching models using `neermatch` is a three-step process:

1.  Instantiate a model with a similarity map.
2.  Compile the model.
3.  Train the model.

To train the model you need to provide three datasets. Two datasets
should contain records representing the entities to be matched. By
convention, the first dataset is called Left and the second dataset is
called Right dataset in the package’s documentation. The third dataset
should contain the ground truth labels for the matching entities. The
ground truth dataset should have two columns, one for the index of the
entity in the Left dataset and one for the index of the entity in the
Right dataset.

``` r
# see also vignette("fuzzy-games") for a more detailed example
library(neermatch)

# 0) replace this with your own data preprocessing function
matching_data <- fuzzy_games_example_data()

# 1) customize according to the fields in your data
similarity_map <- SimilarityMap(
  list(
    title = list("jaro", "levenshtein"),
    platform = list("levenshtein", "discrete"),
    year = list("euclidean", "discrete"),
    `developer~dev` = list("jaro")
  )
)
model <- NSMatchingModel(similarity_map)

# 2) compile
compile(model)

# 3) train
fit(
  model,
  matching_data$left, matching_data$right, matching_data$matches,
  epochs = 71L, batch_size = 16L, log_mod_n = 10L
)
#> | Epoch      | BCE        | Recall     | Precision  | F1         | Sat        |
#> | 0          | 6.8655     | 0.0000     | nan        | nan        | 0.7393     |
#> | 10         | 5.2539     | 0.0000     | nan        | nan        | 0.8062     |
#> | 20         | 6.4435     | 0.0000     | nan        | nan        | 0.8388     |
#> | 30         | 7.0955     | 0.0000     | nan        | nan        | 0.8450     |
#> | 40         | 6.0574     | 0.0000     | nan        | nan        | 0.8524     |
#> | 50         | 2.4904     | 0.0000     | nan        | nan        | 0.8715     |
#> | 60         | 2.0246     | 0.0000     | nan        | nan        | 0.8793     |
#> | 70         | 1.6947     | 1.0000     | 0.9750     | 0.9873     | 0.8882     |
#> Training finished at Epoch 70 with DL loss 1.6947 and Sat 0.8882
```

# Installation

## From Source

You can obtain the sources for the development version of `neermatch`
from its github
[repository](https://github.com/pi-kappa-devel/r-neer-match).

``` bash
git clone https://github.com/pi-kappa-devel/r-neer-match
```

The installation package can be built using:

``` bash
R CMD build r-neer-match
```

The last command produces a file named `neermatch_<version>.tar.gz`,
where `<version>` is the latest version number of the package. This can
be used to install the package in R as follows:

``` bash
R CMD INSTALL neermatch_<version>.tar.gz
```

# Documentation

Online documentation is available for the
[release](https://r-neer-match.pikappa.eu) and
[in-development](https://r-neer-match.pikappa.eu/dev/) versions of the
package.

## Reproducing Documentation from Source

The README.md file can be created using
[rmarkdown](https://rmarkdown.rstudio.com/).

``` r
rmarkdown::render("README.Rmd", output_format = "md_document")
```

Documentation entries for the package are created using
[roxygen2](https://roxygen2.r-lib.org/index.html) (Wickham, Danenberg,
et al. 2024).

``` r
roxygen2::roxygenize()
```

Finally, the documentaion website uses
[pkgdown](https://pkgdown.r-lib.org/) (Wickham, Hesselberth, et al.
2024).}

``` r
pkgdown::build_site(lazy = TRUE)
```

## Logo

The logo was designed using [Microsoft
Designer](https://designer.microsoft.com/) and [GNU Image Manipulation
Program (GIMP)](https://www.gimp.org/). The hexagon version of the logo
was generated with the R package
[hexSticker](https://github.com/GuangchuangYu/hexSticker). It uses the
[Philosopher](https://fonts.google.com/specimen/Philosopher) font.

# Alternative Software

Several state-of-the-art entity matching (EM) systems have been
developed in recent years, utilizing different methodologies to address
the challenges of EM tasks. Below, we highlight some of the most recent,
best-performing and/or most recognized EM systems:

- [**HierGAT**](https://github.com/CGCL-codes/HierGAT): HierGAT
  introduces a Hierarchical Graph Attention Transformer Network to model
  and leverage interdependence between EM decisions and attributes. It
  uses a graph attention mechanism to identify discriminative words and
  attributes, combined with contextual embeddings to enrich word
  representations, enabling a more nuanced and interconnected approach
  to EM (Yao et al. 2022).

- [**Ditto**](https://github.com/megagonlabs/ditto): Ditto leverages
  pre-trained Transformer-based language models to cast EM as a
  sequence-pair classification task, enhancing matching quality through
  fine-tuning. It incorporates optimizations such as domain-specific
  highlighting, string summarization to retain essential information,
  and advanced data augmentation to improve training, making it both
  efficient and effective for large-scale EM tasks (Li et al. 2020).

- **CorDEL**: CorDEL employs a contrastive deep learning framework that
  moves beyond twin-network architectures by focusing on both syntactic
  and semantic matching signals while emphasizing critical subtle
  differences. The approach includes a simple yet effective variant,
  CorDEL-Sum, to enhance the model’s ability to discern nuanced
  relationships in data (Wang et al. 2020).

- [**DAEM**](https://github.com/nju-websoft/DAEM): This approach
  combines a deep neural network for EM with adversarial active
  learning, enabling the automatic completion of missing textual values
  and the modeling of both similarities and differences between records.
  It integrates active learning to curate high-quality labeled examples,
  adversarial learning for augmented stability, and a dynamic blocking
  method for scalable database handling, ensuring efficient and robust
  EM performance (Huang et al. 2023).

- [**AdaMEL**](https://github.com/DerekDiJin/AdaMEL-supplementary):
  AdaMEL introduces a deep transfer learning framework for multi-source
  entity linkage, addressing challenges of incremental data and source
  variability by learning high-level generic knowledge. It employs an
  attribute-level self-attention mechanism to model attribute importance
  and leverages domain adaptation to utilize unlabeled data from new
  sources, enabling source-agnostic EM while accommodating additional
  labeled data for enhanced accuracy (Jin et al. 2021).

- [**DeepMatcher**](https://github.com/anhaidgroup/deepmatcher): This
  framework is one of the first to introduce deep learning (DL) to
  entity matching, categorizing learning approaches into SIF, RNN,
  Attention, and Hybrid models based on their representational power. It
  highlights DL’s strengths in handling textual and dirty EM tasks while
  identifying its limitations in structured EM, offering valuable
  insights for both researchers and practitioners (Mudgal et al. 2018).

- **SETEM**: SETEM introduces a self-ensemble training method for EM to
  overcome challenges in real-world scenarios, such as small datasets,
  hard negatives, and unseen entities, where traditional Pre-trained
  Language Model (PLM)-based methods often struggle due to their
  reliance on large labeled datasets and overlapping benchmarks. By
  leveraging the stability and generalization of ensemble models, SETEM
  effectively addresses these limitations while maintaining low memory
  consumption and high label efficiency. Additionally, it incorporates a
  faster training method designed for low-resource applications,
  ensuring adaptability and scalability for practical EM tasks (Ding et
  al. 2024).

- **AttendEM**: AttendEM introduces a novel framework for entity
  matching (EM) that enhances transformer architectures through
  intra-transformer ensembling, distinct text rearrangements, additional
  aggregator tokens, and extra self-attention layers. Departing from the
  focus on text cleaning and data augmentation in existing solutions,
  AttendEM innovates within the base model design, offering a distinct
  approach to pairwise duplicate identification across databases (Low,
  Fung, and Xiong 2024).

# Contributors

[Pantelis Karapanagiotis](https://www.pikappa.eu) (maintainer)

[Marius Liebald](https://www.marius-liebald.de) (contributor)

Feel free to share, modify, and distribute. If you implement new
features that might be of general interest, please consider contributing
them back to the project.

# License

The package is distributed under the [MIT license](LICENSE.md).

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-tensorflow2015" class="csl-entry">

Abadi, Martín, Ashish Agarwal, Paul Barham, Eugene Brevdo, Zhifeng Chen,
Craig Citro, Greg S. Corrado, et al. 2015. “TensorFlow:
<span class="nocase">Large-scale</span> Machine Learning on
Heterogeneous Systems.” <https://www.tensorflow.org/>.

</div>

<div id="ref-badreddine2022" class="csl-entry">

Badreddine, Samy, Artur d’Avila Garcez, Luciano Serafini, and Michael
Spranger. 2022. “Logic Tensor Networks.” *Artificial Intelligence* 303:
103649. <https://doi.org/10.1016/j.artint.2021.103649>.

</div>

<div id="ref-keras2015" class="csl-entry">

Chollet, François et al. 2015. “Keras.” <https://keras.io>.

</div>

<div id="ref-ding2024" class="csl-entry">

Ding, Huahua, Chaofan Dai, Yahui Wu, Wubin Ma, and Haohao Zhou. 2024.
“SETEM: <span class="nocase">Self-ensemble</span> Training with
<span class="nocase">Pre-trained Language Models</span> for Entity
Matching.” *Knowledge-Based Systems* 293 (June): 111708.
<https://doi.org/10.1016/j.knosys.2024.111708>.

</div>

<div id="ref-huang2023" class="csl-entry">

Huang, Jiacheng, Wei Hu, Zhifeng Bao, Qijin Chen, and Yuzhong Qu. 2023.
“Deep Entity Matching with Adversarial Active Learning.” *The VLDB
Journal* 32 (1): 229–55. <https://doi.org/10.1007/s00778-022-00745-1>.

</div>

<div id="ref-jin2021" class="csl-entry">

Jin, Di, Bunyamin Sisman, Hao Wei, Xin Luna Dong, and Danai Koutra.
2021. “Deep Transfer Learning for Multi-Source Entity Linkage via Domain
Adaptation.” In *Proceedings of the VLDB Endowment*, 15:465–77.
<https://doi.org/10.14778/3494124.3494131>.

</div>

<div id="ref-karapanagiotis2023" class="csl-entry">

Karapanagiotis, Pantelis, and Marius Liebald. 2023. “Entity Matching
with Similarity Encoding: A Supervised Learning Recommendation Framework
for Linking (Big) Data.” <http://dx.doi.org/10.2139/ssrn.4541376>.

</div>

<div id="ref-li2020" class="csl-entry">

Li, Yuliang, Jinfeng Li, Yoshihiko Suhara, AnHai Doan, and Wang-Chiew
Tan. 2020. “Deep Entity Matching with Pre-Trained Language Models.”
*Proceedings of the VLDB Endowment* 14 (1): 50–60.
<https://doi.org/10.14778/3421424.3421431>.

</div>

<div id="ref-low2024" class="csl-entry">

Low, Jwen Fai, Benjamin C. M. Fung, and Pulei Xiong. 2024. “Better
Entity Matching with Transformers Through Ensembles.” *Knowledge-Based
Systems* 293 (June): 111678.
<https://doi.org/10.1016/j.knosys.2024.111678>.

</div>

<div id="ref-mudgal2018" class="csl-entry">

Mudgal, Sidharth, Han Li, Theodoros Rekatsinas, AnHai Doan, Youngchoon
Park, Ganesh Krishnan, Rohit Deep, Esteban Arcaute, and Vijay
Raghavendra. 2018. “Deep Learning for Entity Matching: A Design Space
Exploration.” In *Proceedings of the 2018 International Conference on
Management of Data*, 19–34. <https://doi.org/10.1145/3183713.3196926>.

</div>

<div id="ref-wang2020" class="csl-entry">

Wang, Zhengyang, Bunyamin Sisman, Hao Wei, Xin Luna Dong, and Shuiwang
Ji. 2020. “CorDEL: A Contrastive Deep Learning Approach for Entity
Linkage.” In *2020 IEEE International Conference on Data Mining (ICDM)*,
1322–27. IEEE. <https://doi.org/10.1109/ICDM50108.2020.00171>.

</div>

<div id="ref-roxygen22024" class="csl-entry">

Wickham, Hadley, Peter Danenberg, Gábor Csárdi, and Manuel Eugster.
2024. “Roxygen2: In-Line Documentation for R.”
<https://roxygen2.r-lib.org/>.

</div>

<div id="ref-pkgdown2024" class="csl-entry">

Wickham, Hadley, Jay Hesselberth, Maëlle Salmon, Olivier Roy, and Salim
Brüggemann. 2024. “Pkgdown: Make Static HTML Documentation for a
Package.” <https://pkgdown.r-lib.org/>.

</div>

<div id="ref-yao2022" class="csl-entry">

Yao, Dezhong, Yuhong Gu, Gao Cong, Hai Jin, and Xinqiao Lv. 2022.
“Entity Resolution with Hierarchical Graph Attention Networks.” In
*Proceedings of the 2022 International Conference on Management of
Data*, 429–42. Philadelphia PA USA: ACM.
<https://doi.org/10.1145/3514221.3517872>.

</div>

</div>
