#!/usr/bin/env python3
#-*- coding: utf-8 -*-

from PIL import Image

import os
import glob
import numpy as np
import logging
import pathlib
import shutil
try:
    import xml.etree.cElementTree as ET
except ImportError:
    import xml.etree.ElementTree as ET

CLASSES = ['square', 'round', 'triangle', 'drone', 'bird']

class VOCParser:
    def __init__(self, classes):
        self.classes = classes
        self.class_ids = range(len(self.classes))
        self.index_map = dict(zip(self.classes, self.class_ids))

    def readVOCAnnotation(self, path):
        root = ET.parse(path).getroot()
        img_name = root.find('filename').text
        size = root.find('size')
        width = float(size.find('width').text)
        height = float(size.find('height').text)
        label = []
        for obj in root.iter('object'):
            cls_name = obj.find('name').text.strip().lower()
            if cls_name not in self.classes:
                continue
            cls_id = self.index_map[cls_name]
            xml_box = obj.find('bndbox')
            xmin = (int(xml_box.find('xmin').text) - 1)
            ymin = (int(xml_box.find('ymin').text) - 1)
            xmax = (int(xml_box.find('xmax').text) - 1)
            ymax = (int(xml_box.find('ymax').text) - 1)
            
            label.append([xmin, ymin, xmax, ymax, cls_id, img_name])
        return label

def generate_crops(annotations_path, images_path, classes, target_path):
    globs = str(os.path.join(annotations_path, '*.xml'))
    parser = VOCParser(classes)

    for obj_id in parser.class_ids:
        try:
            os.mkdir(os.path.join(target_path, str(obj_id)))
        except FileExistsError:
            continue

    for i, anno in enumerate(glob.glob(globs)):
        labels = parser.readVOCAnnotation(anno)
        if not labels:
            continue
        img_path = os.path.join(images_path, labels[0][5])
        img_name = pathlib.PurePath(img_path).stem
        if not os.path.isfile(img_path):
            continue
        im = Image.open(img_path)

        for j, label in enumerate(labels):
            obj_img = im.crop((label[0], label[1], label[2], label[3]))
            obj_img.save(os.path.join(target_path, str(label[4]), img_name + '_' + str(j) + '.jpg'))

def extract_test_images(dataset, img_path, target_path):
    with open(dataset, 'r') as f:
        test_img_ids = f.readlines()
    test_img_ids = [ img_id.strip() for img_id in test_img_ids]

    img_folders = os.listdir(img_path)

    if not os.path.exists(target_path):
        os.mkdir(target_path)

    for folder in img_folders:
        source_folder = os.path.join(img_path, folder)        
        if not os.path.isdir(source_folder):
            continue
        
        target_folder = os.path.join(target_path, folder)
        if not os.path.exists(target_folder):
            os.mkdir(target_folder)
        
        imgs = os.listdir(source_folder)
        for img in imgs:
            for test_img in test_img_ids:
                if img.startswith(test_img):
                    shutil.move(os.path.join(source_folder, img),
                                os.path.join(target_folder, img))
                    
            

        

    