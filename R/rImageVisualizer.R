library(tcltk2)
library(rPython)

###Constants declarations

SCRIPT_PATH =  "src/python/"
IMAGE_GENERATOR_SCRIPT_FILE_NAME = "image-generator-from-matrix.py"
PACKAGE_NAME = "rImageVisualizer"

###End constant declarations


rImageVisualizerVisualize = function(){

  ##Methods declaration
  generating_image = function (array, id){
    valorSeleccionado = array[id]
    print(dim(get(valorSeleccionado)))
    pythonScript = toString(system.file(paste(SCRIPT_PATH,IMAGE_GENERATOR_SCRIPT_FILE_NAME,sep=""), package = PACKAGE_NAME ))
    print(pythonScript)
    python.load(pythonScript)
    python.call("render_image", get(valorSeleccionado))
    if(!is.null(win1$env$matrix)){
      tkdestroy(win1$env$frm)
    }
    load_image(imgfile = "/tmp/test.gif")

    win1$env$matrix = get(valorSeleccionado);
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
                  filetypes = "{ {JPEG Files} {.jpg .jpeg} } { {GIF Files} {.gif} } { {PNG Files} {.png} }  { {All Files} * }"))
      tkmessageBox(title = "File name",
                   message = paste("Your file will be saved as", filename),
                   icon = "info", type = "ok")

    }
  }
  ##End methods declaration

  print("Visualizing!!!");
  black_and_white = c()
  for(i in ls(globalenv())){
    if("matrix" ==  class(get(i))){
      black_and_white <- c(black_and_white, i)
    }
  }

  labelText <- tclVar("X:    Y:")

  win1 <- tktoplevel()

  ## MENU CONFIGURATION
  win1$env$menu <- tk2menu(win1)
  tkconfigure(win1, menu = win1$env$menu)
  win1$env$menuFile <- tk2menu(win1$env$menu, tearoff = FALSE)
  win1$env$menuOpenRecent <- tk2menu(win1$env$menuFile, tearoff = FALSE)
  ##START BIG IF
  if(length(black_and_white) >0){
    tkadd(win1$env$menuOpenRecent, "command", label = black_and_white[1],
          command = function(data) { generating_image(black_and_white, 1)})
  }


  ##END BIG IF
  tkadd(win1$env$menuFile, "cascade", label = "Show matrix Black and White Image ",
        menu = win1$env$menuOpenRecent)
  tkadd(win1$env$menuFile, "command", label = "Save",
        command = function(data){ save_matrix()})
  win1$env$matrix = NULL;

  tkadd(win1$env$menuFile, "command", label = "Quit",
        command = function() tkdestroy(win1))
  tkadd(win1$env$menu, "cascade", label = "File", menu = win1$env$menuFile)

  tktitle(win1) <- "Image Visualizer"

  win1$env$frm <- tk2frame(win1, borderwidth = 0, relief = "sunken",
                         padding = 0)

  tkpack(tk2label(win1,
                  textvariable = labelText,
                  width = 40, justify = "left", background = "#ffffff"),
         side = "bottom", expand = FALSE, ipadx = 5, ipady = 5,
         fill = "x")

}

