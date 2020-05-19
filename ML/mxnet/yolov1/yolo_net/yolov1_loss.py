# -*- coding: utf-8 -*-
# https://blog.emmanuelcaradec.com/humble-yolo-implementation-in-keras/
# https://github.com/TowardsNorth/yolo_v1_tensorflow_guiyu/blob/master/yolo/yolo_net.py

import mxnet as mx
from mxnet import gluon, nd
from mxnet.gluon import nn, loss

class yolov1_loss(loss.Loss):
    def __init__(self, num_classes, S=7, B=2, obj_scale=1.0, noobj_scale=5, coord_scale=5.0, cls_scale=1.0,
                 batch_axis=0, **kwargs):
        super().__init__(None, batch_axis, **kwargs)
        
        self.num_classes = num_classes
        self.S = S
        self.B = B
        self.obj_scale = obj_scale
        self.noobj_scale = noobj_scale
        self.coord_scale = coord_scale
        self.cls_scale = cls_scale

        self.cls_boundary = self.S * self.S * self.num_classes
        self.conf_boundary = self.S * self.S * self.B + self.cls_boundary

    def _split_output(self, F, out):
        classes = F.reshape(F.softmax(out[:, :self.cls_boundary], axis=1), 
                                      (-1, self.S * self.S, self.num_classes))
        confs =  F.reshape(out[:, self.cls_boundary: self.conf_boundary], (-1, self.S, self.S ,self.B))
        boxes = F.reshape(out[:, self.conf_boundary:], (-1, self.S, self.S, self.B, 4))

        return classes, confs, boxes

    def _compute_yolo_iou(self, F, boxes1, boxes2):
        '''
        IoU of corresponding anchors
        '''

        # to corner representation
        x11 = boxes1[:, :, :, :, 0] - boxes1[:, :, :, :, 2] / 2.0
        y11 = boxes1[:, :, :, :, 1] - boxes1[:, :, :, :, 3] / 2.0
        x12 = boxes1[:, :, :, :, 0] + boxes1[:, :, :, :, 2] / 2.0
        y12 = boxes1[:, :, :, :, 1] + boxes1[:, :, :, :, 3] / 2.0
        boxes1_new = nd.stack([x11, y11, x12, y12], axis=-1)
        x21 = boxes2[:, :, :, :, 0] - boxes2[:, :, :, :, 2] / 2.0
        y21 = boxes2[:, :, :, :, 1] - boxes2[:, :, :, :, 3] / 2.0
        x22 = boxes2[:, :, :, :, 0] + boxes2[:, :, :, :, 2] / 2.0
        y22 = boxes2[:, :, :, :, 1] + boxes2[:, :, :, :, 3] / 2.0
        boxes2_new = nd.stack([x21, y21, x22, y22], axis=-1)

        # calculating 2 border points
        upperleft = nd.maximum(boxes1_new[:, :, :, :, :2], boxes2_new[:, :, :, :, :2])
        lowerright = nd.minimum(boxes1_new[:, :, :, :, 2:], boxes2_new[:, :, :, :, 2:])
        
        intersection_dims = nd.maximum(0.0, lowerright - upperleft)
        intersection_area = intersection_dims[:, :, :, :, 0] * intersection_dims[:, :, :, :, 1]

        area1 = boxes1_new[:, :, :, :, 3] * boxes1_new[:, :, :, :, 2]
        area2 = boxes2_new[:, :, :, :, 3] * boxes2_new[:, :, :, :, 2]

        union_area = nd.maximum(1e-8, area1 + area2 - intersection_area)

        return nd.clip(intersection_area / union_area, a_min=0.0, a_max=1.0)
    

    def hybrid_forward(self, F, labels, preds):
        assert labels.shape == preds.shape, "mismatched input shapes"

        cls_preds, conf_preds, box_preds = self._split_output(F, preds)
        cls_labels, conf_labels, box_labels =  self._split_output(F, labels)
        label_response = box_labels[:, :, :, 0]

        preds_label_iou = self._compute_yolo_iou(F, box_preds, box_labels)
