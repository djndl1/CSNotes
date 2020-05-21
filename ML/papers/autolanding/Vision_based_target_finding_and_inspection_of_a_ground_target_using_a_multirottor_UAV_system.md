The paper focuses on target detection and navigation. A downward-looking camera attached to a multirotor is used to find the target on the ground. The UAV descends to the target and hovers above the target for a few seconds to inspect the target. A high-level decision algorithm based on an OODA (observe, orient, decide, and act) loop was developed as a solution to address the problem. The proposed system performed hovering above the target in three different stages: locate, descend, and hover.

The main node receives the center coordinates of the detected target from the target detection node as an input. This is then converted to the target location in the inertial frame by using the pinhole camera model.

The typical mission is:

1. the UAV takes off;

2. searches for a ground target;

3. If a target is found, the UAV changes its original path and hovers above the target  for a few seconds for an action

4. After hovering, the UAV flies to the next waypoint and starts to search for other targets.


# Target Detection Algorithm

Hough circle method; color detection;

# Conversion of 2D Image Coordinates to World Coordinates

image plane => camera frame (a downward-looking frame, not the actual camera orientation) => body frame => inertial frame (world frame)

Note that no gimbal mechanism is used, meaning that the camera is not always looking downward, a correction is needed.

# Navigation Algorithm

In the locate stage, the estimated target position is used to move the UAV laterally to the target's $x$, $y$ position without descending. In the descend stage, the lateral deviation between the target's position and the UAV's position is calculated. If the deviation is within the tolerance, the height of the UAV is reduced by a predefined parameter. Otherwise, the locate stage will be repeated. This process continues until the UAV reaches the hovering height. This process happens in quick succession.

```python
stage = Stage.LOCATE
for img_i in frames:
    current_pose = get_current_UAV_pose()
    if istargetFound():                                       # detect and find the target
        img_seq_found = img_timestamp(img_i)
        target_centroid = compute_target_centroid(img_i)
        last_target_centroid = target_centroid
        target_pose = compute_target_pose(current_pose, target_centroid)
        
        if stage == Stage.LOCATE: # locate and move to the target horizontally
            hover_pose = target_pose
            hover_pose.z = current_pose.z   # but do not descend
            next_pose = hover_pose          # to move 
            stage = Stage.DESCNED  # enter descending stage
        elf stage == Stage.DESCNED:
            if distanceXY(target_pose, current_pose) < t:   # already right over the target
                if hover_height < current_pose.z - descend_step:
                    hover_pose.z = current_pose.z - descend_step
                else:
                    hover_pose.z = hover_height
                    stage = Stage.HOVER
                next_pose = hover_pose
            else:
                stage = Stage.LOCATE # not at the right position, locate again
        elif stage == Stage.HOVER: # already at the hovering height, 
            # tweaking position
            if abs(img_center_i.u - target_centroid.u) > g or abs(img_center_i.v - target_centroid.v) > g: # tolerance in pixel
                dx = (img_center_i.u - target_centroid.u) / camera_resolution * horizontal_step 
                dy = (img__center_i.v - target_centroid.v) / camera_resolution * horizontal_step
                hover_pose.x = current_pose.x + dx
                hover_pose.y = current_pose.y + dy
                next_pose = hover_pose
            else: # hover for some time and leave
                wait(hover_time)
                break
    else:                                       # target not found
        img_seq_not_found = img_timestamp(img_i)
        if stage == Stage.HOVER and img_seq_not_found - img_seq_found < timeout_detection:      # target out of sight before timeout
                dx = (img_center_i.u - last_target_centroid.u) / camera_resolution * horizontal_step 
                dy = (img_center_i.v - last_target_centroid.v) / camera_resolution * horizontal_step
                hover_pose.x = current_pose.x + dx
                hover_pose.y = current_pose.y + dy

``
