# BN-assignment1
This is the code we used to make our report for the course Bayesian Networks at the RU.

NaiveDiagram.r contains a naive implementation of a Bayesian Network.
The network we initially designed (as shown in the report) is implemented in CausalDiagram_0.R.
All of the other CausalDiagram_X.R files are iterations that each attempt to improvement on our network. After iteration _5, we decided the model was good enough to perform inference on.
The inference is done in CausalDiagram_Inference.R and CausalDiagram_Inference_SEM.R, the second of which uses a Structural Equational Model in an attempt to improve on our inference task.

The only data in the Data folder that was used eventually was student-por.csv
