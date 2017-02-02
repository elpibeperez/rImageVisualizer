import wx
import Image
import numpy
import json

TEMP_IMAGE_DEFAULT_VALUE = "/tmp/test.gif"


def get_percentile_values(sorted_array, percentage, dim):
    delta = percentage /2. * dim /100.
    return (int(sorted_array[0,delta-1]), int(sorted_array[0, dim-delta-1]));


def correct_array(array,min_value,max_value):
    (x,y) = array.shape
    for i in range(x):
        for j in range(y):
            if array[i,j]>max_value :
                array[i,j] = max_value
            elif array[i,j]<min_value :
                array[i,j] = min_value
    return array

def rescale_array(array, min_new =0, max_new = 255):
    amin = numpy.amin(array)
    amax = numpy.amax(array)
    arange = abs(amax - amin)
    newrange = abs(max_new -min_new)
    (x,y) = array.shape
    for i in range(x):
        for j in range(y):
            array[i, j] = int(((array[i,j]-amin)*newrange/arange) + min_new)
    return array
    
def render_image(mat, image = TEMP_IMAGE_DEFAULT_VALUE):
    if mat != None :
        x = len(mat)
        y = len(mat[0])
        arr = numpy.array(mat)
        arr.resize((x,y))
        arr = rescale_array(arr)
        im = Image.fromarray(arr.astype('uint8'))        
        im.save(image, "gif")
    else:
        pass 


