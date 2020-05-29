import mxnet as mx
import numpy as np
import cv2

def imcrop(img: np.array, bbox: np.array):
    bbox_int = bbox.astype(np.int)
    return img[bbox[1]: bbox[3], bbox[0]: bbox[2], :]

def feed_bboxed(net, ctx, input_shape, img, ids, scores, thresh, bboxes):
    n_bboxes = bboxes.shape[0]
    for i in range(n_bboxes):
        bbox = bboxes[i, :]
        if scores[i] < thresh or \
            (bbox[0] < 0 and bbox[1] < 0 and bbox[2] < 0 and bbox[3] < 0):
            break
        cropped = imcrop(img, bbox)
        resized = cv2.resize(cropped, input_shape)
        resized = resized.as_in_context(ctx)
        id = net(resized).argmax(-1).asscalar()
        ids[i] = id

def crop_training_images

