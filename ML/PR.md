# Pattern Recognition
 by Sergio Theodoridis, Konstantinos Koutroumbas
 
 This note is not intended to be a rigorous mathematical walkthrough, refer to the book for details.
 
 #### A simple task illustrating the process of PR
 
 For a task classify cancer regions, the measurements used for the classification are know as _features_, comprising a _feature vector_.
 
 >Keywords: decision line; optimality criterion
 
 - feature generation stage
 
 - feature seleciton stage: in practice, a larger than necessary number of feature candidates is generated, and the best of them is adopted.
 
 - classifier design stage 
 
 - system evaluation stage

> supervised, unsupervised, semi-supervised learning

## Linear Classifier

Read [this](https://math.stackexchange.com/questions/1210545/distance-from-a-point-to-a-hyperplane) and [this](https://en.wikipedia.org/wiki/Linear_classifier) before continuing.

A linear classifier achieves this by making a classification decision based on the value of a linear combination of the characteristics. Affine hyperplanes are used to define decision boundaries in many machine learning algorithms such as linear-combination (oblique) decision trees, and perceptrons.

If the input feature vector to the classifer is a real vector $vec{x}$, then the output score is 

$$
g\left(x \right)=f\left(\vec{w}\cdot\vec{x}\right)=f\left(\sum_{j}w_{j}x_{j}\right)
$$

The weight vector $\vec{w}$ is orthogonal to the decision hyperplane.

Since $dist=\frac{\left|g\left(x\right)\right|}{\left\lVert w\right\rVert }$, fixing $w$, then $g(x)$ determines the distance of a point from the decision hyperplane.

### Perceptron algorithm
Assume there exists a hyperplane s.t.

$$
w^{*T}x>0 \quad	\forall x\in w_{1} \\
w^{*T}x<0 \quad 	\forall x\in w_{2}
$$

The perceptron cost

$$
J\left(w\right)=\sum_{x\in Y}\left(\delta_{x}w^{T}x\right)
$$

where $y$ is the set of all misclassified training vectors and $\delta_x$ is defined as 

$$
\delta_{x}=\begin{cases}
-1 & x\in w_{1}\\
1 & x\in w_{2}
\end{cases}
$$

Apply the gradient descent method, we have

$$
w\left(t+1\right)=w\left(t\right)-\rho_{t}\sum_{x\in Y}\delta_{x}x
$$

The online version is more simpler and more popular

$$
\begin{aligned} 

w\left(t+1\right)	&=w\left(t\right)-\delta_{x}\rho x_{\left(t\right)}\quad\text{if }\delta_{x}w^{T}x_{\left(t\right)}\geq0 \\
w\left(t+1\right)	&=w\left(t\right)\quad\text{otherwise}

\end{aligned}
$$

A basic requirement for the convergence of the perceptron algorithm is the linear separability of the classes. If this is not true, as is usually the case in practice, the perceptron algorithm does not converge. This leads to _pocket_ algorithm.
