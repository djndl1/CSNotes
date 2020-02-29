The accuracy of GPS receiver fit for use on miniature UAVs is measured in meters, making them unsuitable for precision tasks such as landing. IVBS is technique that performs the majority of the control calculations in 2D image space, which eliminates the need for expensive 3D body or inertial reference frames.

# Image-Based Visual Servoing

In IBVS, the task space is defined in two-dimensional image space and the control law is designed as a function of the image error $e$, defined as 

$$
e = p - p_{d}
$$

where $p_{d}$ is the desired location of the _image feature_. For landing, $p_{d}$ is static.

TODO

Three modes:

1. Patrol Mode: the quadrotor patrols over a certain area until the target is detected.

2. IBVS-Guided Tracking Mode: after the target is detected for $0.2$ seconds, the control logic is switched to tracking mode. The purpose of the mode is to stabilize the quadrotor $0.4m$ over the target before attempting the landing maneuver. The desired image feature locations are set to corresponding to how they would look if the quadrotor were positioned at the desired location.

3. The desired image features are set to correspond to the position $0.20m$ directly over the target. If the error norm is below a defined threshold, the thrust is turned off. This small open-loop drop is necessary since the target is now so close to the camera that small state errors can cause the camera to loose tracking for a number of reasons: mainly velocity of the features in iamge space, nonlinear distortions outside the camera calibration range and the FOV.

TODO
