import os, shutil
import math
import random
import glob
import pandas as pd
#%% 

def split_dataset(datafolder, test_ratio):
    test_size = min(max(0, test_ratio), 1.0)
    pic_folders = os.listdir(datafolder)
    pic_folders.sort()

    test_dfs = []
    train_dfs = []

    for class_id, folder_name in enumerate(pic_folders):
        class_folder = os.path.join(datafolder, folder_name)
        pic_names = os.listdir(class_folder)
        num_pics = len(pic_names)

        random.shuffle(pic_names)
        test_size = int(num_pics * test_ratio)
        test_pics_names = random.sample(pic_names, test_size)
        test_labels = [class_id] * len(test_pics_names)
        test_pics_paths = [str(os.path.join(class_folder, pic_name)) for pic_name in test_pics_names]
        test_pics_recs = list(zip(test_pics_paths, test_labels))
            
        this_test_df = pd.DataFrame(test_pics_recs)
        test_dfs.append(this_test_df)

        train_pics_names = list(set(pic_names) - set(test_pics_names))
        train_labels = [class_id] * len(train_pics_names)
        train_pics_paths = [str(os.path.join(class_folder, pic_name)) for pic_name in train_pics_names]
        train_pics_recs = list(zip(train_pics_paths, train_labels))

        this_train_df = pd.DataFrame(train_pics_recs)
        train_dfs.append(this_train_df)

    test_df = pd.concat(test_dfs, ignore_index=True)
    train_df = pd.concat(train_dfs, ignore_index=True)

    return train_df, test_df
        

def generate_image_folders(target_folder, images_df):
    if not os.path.isdir(target_folder):
        os.mkdir(target_folder)

    folders = set(images_df.iloc[:, 1])
    for f in folders:
        new_folder = os.path.join(target_folder, str(f))
        if not os.path.exists(new_folder):
            os.mkdir(new_folder)

    for _, content in images_df.iterrows():
        path = content[0]
        label = content[1]
        sym_folder = os.path.join(target_folder, str(label))
        file_name = os.path.basename(path)
        sym_path = os.path.join(sym_folder, file_name)
        os.symlink(path, sym_path)

    
