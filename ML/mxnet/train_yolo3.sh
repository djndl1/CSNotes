#!/bin/bash

python train_yolo3.py --network mobilenet1.0 --dataset custom --data-shape 224 --batch-size 16 -j 2 --gpus 0 \
       --epochs 30 --lr 0.0001 --lr-decay-period 50 --log-interval 10  --label-smooth
