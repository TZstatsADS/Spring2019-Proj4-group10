# Project: OCR (Optical Character Recognition) 

![image](figs/intro.png)

### [Full Project Description](doc/project4_desc.md)

Term: Spring 2019

+ Team 10
+ Team members
	+ Xiaoxi Zhao xz2740
	+ Seungwook Han sh3264
	+ Hongye Jiang hj2493
	+ Xinzhu Wang xw2581
	+ Jingyue Li jl5283

+ Project summary: In this project, we created an OCR post-processing procedure to enhance Tesseract OCR output. Our work is based on two parts: detection and correction.We implemented the methods listed in D3 and C1. We first extract 12 features from the text and use svm with RBF Kernel to train the model and predict which one is the error word(garbage). Then we realised the correction methods in C1. The C1 paper presents an error correction method for the case in which a word has 1 error -- using digram features. However, we extended the error correction method further by implementing an error correction method for the case in which a word has 2 errors. Finally we report our test confusion matrix.
	
**Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

+ Xiaoxi Zhao: generate features 4-9, implemented svm model with parameter tunning, combined and fixed the correction part and run it to generate the final words output, concatenated the correction result with non-error words to perform evaluation part, write word explaination and generate the final report.
+ Seungwook Han: Generated features 1-3, most frequent symbol feature, non-alphabetical symbols feature, brainstormed and outlined our error detection and correction pipeline with splitting of data, implemented character-wise evaluation metrics, described our implementation details for the project in main.Rmd
+ Hongye Jiang: Genarated the features including bigram, Most frequent symbol, and Non-alphabetical symbols. Wrote an algorithm to determine if each word in OCR output is correct. Combined all these features and if-correct result together into one function for implementing SVM. Implemented word-wise evaluation metrics. Connected each part of this project, added comments and reran the whole project. Prepared for the presentation. 
+ Xinzhu Wang: Implemented whole error correction part based on C1 paper, including detecting and correcting words with only 1 error. Further improved the correction part by taking care of 2 errors' detection and correction. Xinzhu is also responsible to modify and debug any code related to the correction part. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
