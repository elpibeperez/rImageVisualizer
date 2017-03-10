import wx
import Image
import numpy

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

def prepare_band(mat, x, y):
    arr = numpy.array(mat)
    arr.resize((x,y))
    arr = rescale_array(arr)
    return arr.astype('uint8')
    

    
def render_image(red, green, blue , image = TEMP_IMAGE_DEFAULT_VALUE):
    if red != None and green != None and blue != None:
        x = len(red)
        y = len(red[0])
        print(x,y,3)
        rgbArray = numpy.zeros((x,y,3), 'uint8')
        rgbArray[..., 0] = prepare_band(red,x,y)
        rgbArray[..., 1] = prepare_band(green,x,y)
        rgbArray[..., 2] = prepare_band(blue,x,y)      
        im = Image.fromarray(rgbArray)
        im.save(image, "gif")
    else:
        pass 

"""
rgbArray = np.zeros((512,512,3), 'uint8')
rgbArray[..., 0] = r*256
rgbArray[..., 1] = g*256
rgbArray[..., 2] = b*256
img = Image.fromarray(rgbArray)
img.save('myimg.jpeg')
"""

