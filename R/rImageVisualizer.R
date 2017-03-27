
rImageVisualizerVisualize = function(){

  library(tcltk2)
  library(rPython)
  library(ripa)
  library(spatialfil)
  library(EBImage)
  ###Constants declarations

  SCRIPT_PATH =  "src/python/"
  IMAGE_GENERATOR_SCRIPT_FILE_NAME = "image-generator-from-matrix.py"
  COLOUR_IMAGE_GENERATOR_SCRIPT_FILE_NAME = "colour-image-generator-from-matrix.py"
  SAVE_MATRIX_SCRIPT_FILE_NAME = "save-matrix-as-image.py"
  PACKAGE_NAME = "rImageVisualizer"
  TEMP_FILENAME = "/tmp/test.gif"


  ###End constant declarations

  create_kernel = function(mat, k){
    output <- list('matrix' = mat, 'kernel' = k)
    class(output) <- 'convKern'
    return(output)
  }

  rescale_array =  function (array, new_min = 0, new_max = 255){
      amin = min(array)
      amax = max(array)
      arange = abs(amax - amin)
      newrange = abs(new_max -new_min)
      x = dim(array)[1]
      y = dim(array)[2]
      for (i in 1:x){
        for (j in 1:y){
          array[i, j] = round(((array[i,j]-amin)*newrange/arange) + new_min,0)
        }
      }
      return(array)
  }
  ##Methods declaration
  generating_image = function (array, id){
    valorSeleccionado = array[id]
    print(dim(get(valorSeleccionado)))
    pythonScript = toString(system.file(paste(SCRIPT_PATH,IMAGE_GENERATOR_SCRIPT_FILE_NAME,sep=""), package = PACKAGE_NAME ))
    python.load(pythonScript)
    python.call("render_image", get(valorSeleccionado))
    if(!is.null(win1$env$matrix)){
      tkdestroy(win1$env$frm)
    }
    load_image(imgfile = TEMP_FILENAME)
    win1$env$matrix = get(valorSeleccionado);
    print(rescale_array(win1$env$matrix))
    win1$env$matrixname = array[id]
  }

  generating_colour_image = function (array, id){
    valorSeleccionado = get(array[id])
    rojo = valorSeleccionado[,,1]
    verde = valorSeleccionado[,,2]
    azul = valorSeleccionado[,,3]
    pythonScript = toString(system.file(paste(SCRIPT_PATH,COLOUR_IMAGE_GENERATOR_SCRIPT_FILE_NAME,sep=""), package = PACKAGE_NAME ))
    python.load(pythonScript)
    python.call("render_image",rojo, verde, azul)
    if(!is.null(win1$env$matrix)){
      tkdestroy(win1$env$frm)
    }
    load_image(imgfile = TEMP_FILENAME)
    win1$env$matrix = valorSeleccionado
    win1$env$matrixname = array[id]
  }


  load_image <- function(imgfile){
    image1 <- tclVar()
    tkimage.create("photo", image1, file = imgfile)
    win1$env$frm <- tk2label(win1, image = image1)
    tkpack(win1$env$frm, expand = TRUE, fill = "both")
    onMouseOver <- function(x, y) {
      tclvalue(labelText) <-paste("X:",x," Y:",y)
    }
    tkbind(win1$env$frm, "<Motion>", onMouseOver)
    tkconfigure(win1$env$frm, cursor = "hand2")
  }

  save_matrix <- function(){
    if(is.null(win1$env$matrix)){
      tkmessageBox(title = "Not matrix opened",
                   message = "You need to open a matrix to save it as an image",
                   icon = "info", type = "ok")
    }else{
      filename <- tclvalue(tkgetSaveFile(initialfile = win1$env$matrixname,
                  filetypes = "{ {JPEG Files} {.jpg .jpeg} } { {GIF Files} {.gif} } { {PNG Files} {.png} }"))
      if(filename != ""){
        save_matrix_as_image_file(win1$env$matrix, filename)
      }
    }
  }

  save_matrix_as_image_file <- function(matrix, filename){
    pythonScript = toString(system.file(paste(SCRIPT_PATH,SAVE_MATRIX_SCRIPT_FILE_NAME,sep=""), package = PACKAGE_NAME ))
    python.load(pythonScript)
    python.call("render_image", matrix, filename)
  }

  get_matrixes_array = function(){
    matrix_list = c()
    for(i in ls(globalenv())){
      if("matrix" ==  class(get(i))){
        matrix_list <- c(matrix_list, i)
      }
    }
    return (matrix_list)
  }

  get_three_bands_matrixes = function(){
    three_bands_list = c()
    for(i in ls(globalenv())){
      if("array" ==  class(get(i)) && 3 == dim(get(i))[3]){
        three_bands_list <- c(three_bands_list, i)
      }
    }
    return(three_bands_list)
  }

  apply_filter = function(kernel){
    if(!is.null(win1$env$matrix)){
    #   #aplicar el filtro
       win1$env$matrix  = filter2(win1$env$matrix, kernel)
       print(win1$env$matrix)
    #   #redibujar imagen temporal
       pythonScript = toString(system.file(paste(SCRIPT_PATH,IMAGE_GENERATOR_SCRIPT_FILE_NAME,sep=""), package = PACKAGE_NAME ))
       python.load(pythonScript)
       python.call("render_image", win1$env$matrix)
    #   #recargar en el dibujo
       tkdestroy(win1$env$frm)
       load_image(imgfile = TEMP_FILENAME)
    #   print(rescale_array(win1$env$matrix))
    #
    }
    #print(kernel)
    #print(filter_list[kernel])
  }

  ##End methods declaration

  black_and_white = get_matrixes_array()
  colour = get_three_bands_matrixes()


  labelText <- tclVar("X:    Y:")

  win1 <- tktoplevel()

  ## MENU CONFIGURATION
  win1$env$menu <- tk2menu(win1)
  tkconfigure(win1, menu = win1$env$menu)
  win1$env$menuFile <- tk2menu(win1$env$menu, tearoff = FALSE)
  win1$env$menuOpenRecent <- tk2menu(win1$env$menuFile, tearoff = FALSE)
  ##START BIG IF B&W images
  if(length(black_and_white) >0){
    tkadd(win1$env$menuOpenRecent, "command", label = black_and_white[1],
          command = function(data) { generating_image(black_and_white, 1)})
  }
  ##END BIG IF
  tkadd(win1$env$menuFile, "cascade", label = "Show matrix Black and White Image ",
        menu = win1$env$menuOpenRecent)

  win1$env$menuOpenRecent <- tk2menu(win1$env$menuFile, tearoff = FALSE)
  ##START BIG IF COLOUR images
  if(length(black_and_white) >0){
    tkadd(win1$env$menuOpenRecent, "command", label = colour[1],
          command = function(data) { generating_colour_image(colour, 1)})
  }
  ##END BIG IF
  tkadd(win1$env$menuFile, "cascade", label = "Show 3 band matrix array as Colour Image ",
        menu = win1$env$menuOpenRecent)

  tkadd(win1$env$menuFile, "command", label = "Save",
        command = function(data){ save_matrix()})
  win1$env$matrix = NULL;

  tkadd(win1$env$menuFile, "command", label = "Quit",
        command = function() tkdestroy(win1))
  tkadd(win1$env$menu, "cascade", label = "File", menu = win1$env$menuFile)

  ##Add filters
  win1$env$menuFilter <- tk2menu(win1$env$menu, tearoff = FALSE)
  tkadd(win1$env$menuFilter, "command", label = "blur",
        command = function() apply_filter(matrix(c(
          1,  1,  1,  1,  1,
          1,  0,  0,  0,  1,
          1,  0,  0,  0,  1,
          1,  0,  0,  0,  1,
          1,  1,  1,  1,  1
        ),nrow = 5, ncol = 5 )))
  tkadd(win1$env$menuFilter, "command", label = "contour",
        command = function() apply_filter(matrix(c(
          -1, -1, -1,
          -1,  8, -1,
          -1, -1, -1
        ),nrow = 3, ncol = 3 )))
  tkadd(win1$env$menuFilter, "command", label = "detail",
        command = function() apply_filter(matrix(c(
          0, -1,  0,
          -1, 10, -1,
          0, -1,  0
        ),nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "edge enhance 1",
        command = function() apply_filter(matrix(c(
          -1, -1, -1,
          -1, 10, -1,
          -1, -1, -1
        ),nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "edge enhance 2",
        command = function() apply_filter(matrix(c(
          -1, -1, -1,
          -1,  9, -1,
          -1, -1, -1
        ),nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "emboss",
        command = function() apply_filter(matrix(c(
          -1,  0,  0,
          0,  1,  0,
          0,  0,  0
        ),nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "find edges",
        command = function() apply_filter(matrix(c(
          -1, -1, -1,
          -1,  8, -1,
          -1, -1, -1 ),nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "smooth",
        command = function() apply_filter(matrix(c(
          1,  1,  1,
          1,  5,  1,
          1,  1,  1
        ), nrow = 3, ncol = 3)))
  tkadd(win1$env$menuFilter, "command", label = "sharpen",
        command = function() apply_filter(matrix(c(
          -2, -2, -2,
          -2, 32, -2,
          -2, -2, -2
        ), nrow = 3, ncol = 3)))

  tkadd(win1$env$menu, "cascade", label = "Filter", menu = win1$env$menuFilter)



  tktitle(win1) <- "Image Visualizer"

  win1$env$frm <- tk2frame(win1, borderwidth = 0, relief = "sunken",
                         padding = 0)

  tkpack(tk2label(win1,
                  textvariable = labelText,
                  width = 40, justify = "left"),
         side = "bottom", expand = FALSE, ipadx = 5, ipady = 5,
         fill = "x")

}

