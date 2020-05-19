"""YOLO Demo script."""
import os
import argparse
import mxnet as mx
import gluoncv as gcv
import glob
gcv.utils.check_version('0.6.0')
from gluoncv.data.transforms import presets
from matplotlib import pyplot as plt

def parse_args():
    parser = argparse.ArgumentParser(description='Test with YOLO networks.')
    parser.add_argument('--network', type=str, default='yolo3_mobilenet0.25_custom',
                        help="Base network name")
    parser.add_argument('--images', type=str, default='',
                        help='Test images, use comma to split multiple.')
    parser.add_argument('--gpus', type=str, default='',
                        help='Training with GPUs, you can specify 1,3 for example.')
    parser.add_argument('--thresh', type=float, default=0.5,
                        help='Threshold of object score when visualize the bboxes.')
    parser.add_argument('--params', type=str, help='Model parameters')
    
    args = parser.parse_args()
    return args

if __name__ == '__main__':
    args = parse_args()
    # context list
    ctx = [mx.gpu(int(i)) for i in args.gpus.split(',') if i.strip()]
    ctx = [mx.cpu()] if not ctx else ctx

    # grab some image if not specified
    

    net = gcv.model_zoo.get_model(args.network, pretrained=False, pretrained_base=False, classes=['drone', 'else'])
    net.load_parameters(args.params)
    net.set_nms(0.45, 200)
    net.collect_params().reset_ctx(ctx=ctx)

    for img_file in glob.glob(args.images):
        ax = None
        x, img = presets.yolo.load_test(img_file)
        x = x.as_in_context(ctx[0])
        ids, scores, bboxes = [xx[0].asnumpy() for xx in net(x)]

        print(ids.shape, scores.shape, bboxes.shape)
        ax = gcv.utils.viz.plot_bbox(img, bboxes, scores, ids, thresh=0.3,
                                    class_names=net.classes, ax=ax)
        plt.show()
