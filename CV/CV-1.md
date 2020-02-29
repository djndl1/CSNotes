# Computer Vision

Reference:

[1]Computer Vision: Algorithms and Applications. Chap. 1-4, 6, 11, 14

[2]Computer Vsion: A Modern Approach

Vision is an inverse probelem in which we seek to recover some unknowns given insufficient information to fully specify the solution, resorting to physics-based and probabilistic models to disambiguate between potential problems.

> [forward and inverse modelling](https://astronomy.stackexchange.com/questions/19687/what-does-forward-modeling-mean)

In computer vision, we do the inverse modelling, i.e., to describe the world that we see in one or more images and to reconstruct its properties.

> cognitive: logic proving and planning _versus_ perceptual

It is better to think back from the problem at hand to suitable techniques rather than to  grab the first technique that you may have heard of. This kind of working back from problems to solutions is typical of an __engineering__ approach to the study of vision.

## A history

- 1970s: trying to recover the 3D structure of the world -> "blocks world" from edge detection, generalized cylinders, pictorial structures for elastic arrangements of parts; qualitative approach to understanding intensities and shading variations and explaining them by the effects of image formation phenomena -> intrisic images; many feature-based stereo correspondence algorithms; intensity-based optical flow algorithm

> Description of a visual information processing system
> - Computational theory: the goal and constraints
> - Representations and algorithms
> - Hardware implementation

- 1980s: more sophisticated mathematical techniques. Image pyramids for image blending and coarse-to-fine correspondence search; scale space processing; wavelets; a wide variety of shape-from-X techniques; better edge and contour detection. the unification of stereo, flow, shape-from-X and edge detection; Markov Random Field; Kalman filter;

- 1990s: _projective reconstructions_, which did not require knowledge of camera calibration. factorization techniques. Physics-based vision. Dense stereo correspondence algorithm. _Graph cut_ for global optitimazation. _multi-view steroe algorithms_ for producing complete 3D surfaces. _Active contours_; _particle filters_; _level sets_. _minimum energy_ and _minumu description length_, _normalized cuts_ and _mean shift_. _Statistical learning_. _Image morphing_, _view interpolation_.

- 200s: _Image stitching_, _high dynamic range image capture_, _exposure bracketing_, _tone mapping_, _texture synthesis_, "constrellation model". More efficient algorithms for complex global optimization problems. The application of sophisticaed _machine learning_ techniques to computer vision problem.

## Image Formation

### Geometric primitives and transformation

> perspective: an approximate representation on a flat surface, of an image as it is seen by the eye. The two most characteristic features of perspective are that objects appear smaller as their distance from the observer increases; and that they are subject to _foreshortening_, meaning that an objects's dimension along the line of sight appear shorter than its dimensions across the line of sight.

> Perspective projection is a linear projection where three dimensional objects are projected on a _picture plane_.

#### Geometric primitives

__2D points__: a pair of values $\mathrm{x} = (x, y) \in R^2$ or _homogeneous coordinates_, $\mathrm{x^~}  = (\tilde{x}, \tilde{y}, \tilde{w})\in P^2$

> $P^2 = R^3 - (0,0,0)$ is called the 2D _projective space_.

and can be converted back into an _inhomogeneous vector_ $\mathrm{x}$

$$
\mathrm{x^~} =  w^{~} (x, y, 1)
$$

where $\mathrm{\bar{x}} = (x, y ,1)$ is called _augmented vector_. Homogeneous points whose last element $\tilde{w} = 0$ are called _ideal points_ or _points at infinity_ and do not have an equivalent inhomogeneous representation.

__2D lines__ can also be represented using homogeneous coordinates $\tilde{l} = (a,b,c)$. The corresponding _line equation_ is 

$$
\bar{x} \cdot \tilde{l} = ax+by +c =0
$$

> line at infinity: $\bar{l} = (0, 0, 1)$

The line equation can also be normalized or using _polar coordinates_.

When using homogeneous coordinates, the intersection of twe lines is obtained by their cross product. Similarly, the line joining two points can also be obtained by the cross product of the two points.

__2D conics__ can be written using a _quadric_ equation.

__3D points__ have similar representation to 2D points.

__3D planes__ have similar representation to 2D lines.

__3D lines__ Given two points $(p,q)$ Any other point on the line can be exporesses as a linear combination of these two points. In homogeneous coordinates, the line can be written as 

$$
\tilde{r} = \mu\bar{p} + \lambda \bar{q}
$$

> Degree of freedom: the DoF of a system is the number of parameters of the system that may vary independently

An adequate model of 3D lines can be obtained by estimating their direction and some point within the visible portion of the line or by using the two endpoints.

#### Transformations

See P33-P42 in [1], See also [2D transformation](https://www.tutorialspoint.com/computer_graphics/2d_transformation.htm)

Any equation where an augmented vector appear on both sides, it can always be replaced with a full homogeneous vector.

The rotation+translation is also known as _2D rigid body motion_ or the _2D Euclidean transformation_, the transformation matrix is obtained through polar coordiantes and trigonometry.

>Non-technically, Affine Transformations are translations, dilations/expansions/contractions, rotations, reflections, shears and combinations thereof.
>
>[homography](https://en.wikipedia.org/wiki/Homography):  the relation between two figures (as two representations of the same thing in different perspective) such that for every point, line, or angle in one there is a corresponding point, line, or angle in the other.

The easiest wayt to think of these transformations is as a set of matrices operating on 2D homogeneous coordinate vectors.

>[covector](https://en.wikipedia.org/wiki/Linear_form): is a linear map from a vector space to its field of scalars.

##### 3D rotations

The biggest difference between 2D and 3D coordinate transformations is that the parameterization of the 3D rotation matrix $R$ is not as straightforward but several posssibilities exist.

> intrisic rotations: Intrinsic rotations are elemental rotations that occur about the axes of a coordinate system XYZ attached to a moving body. Therefore, they change their orientation after each elemental rotation. 
>
> extrinsic rotations: Extrinsic rotations are elemental rotations that occur about the axes of the fixed coordinate system xyz. The XYZ system rotates, while xyz is fixed.
>
> [Euler angles](https://en.wikipedia.org/wiki/Euler_angles): three angles introduced by Leonhard Euler to describe the orientation of a rigid body w.r.t. a fixed coordinate system. Any orientation can be achieved by composing three elemental rotations, i.e. rotations about the axes of a coordinate system. Euler angles can be defined by three of these rotations.
>
>[Tait-Bryan angles](https://en.wikipedia.org/wiki/Euler_angles#Tait–Bryan_angles): Normally used for aerospace applications. The Tait-Bryan angles represent rotations about three distinct axes.
>
>[Aircraft principal axes](https://en.wikipedia.org/wiki/Aircraft_principal_axes)
>
> The location and orientation together fully describe how the object is placed in space. Euler's rotation theorem shows that in three dimensions any orientation can be reached with a single rotation around a fixed axis. This gives one common way of representing the orientation using an axis–angle representation. Other widely used methods include rotation quaternions, Euler angles, or rotation matrices.

## 3D to 2D projections

There are two graphical projection categories

- parallel projection

- perspective projection

each with its own protocol

Read [this](https://en.wikipedia.org/wiki/3D_projection) and P42-P45 before continuing

Objects drawn with parallel projection do not appear larger or smaller as they extend closer to or away from the viewer. It is not how our eyes or photography normally work. It can easily result in sitautions where depth and altitude are difficult to gauge.

In perspective projection, parallel lines appear to intersect in the projected image, converge towards a single point (_vanishing point_). Perspective projection is usually categorized into one-point, two-point and three-point perspective, depending on the orientation of the projection plane towards the axes of the depicted object. The vanishing points of all horizontal lines lie on the horizon line

### Camera Models

