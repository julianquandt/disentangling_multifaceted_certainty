
if(!exists("data_helpers_executed")){
  
  data_helpers_executed = TRUE
  
  
  
  #' Download file from the OSF
  #'
  #' Extending functionality of the osf_download function from the osfr package by allowing to load the downloaded file into R.
  #'
  #' @param x A file path (file-id) on the osf (see osfr::osf_retrieve).
  #' @param path A path to where a temporary local copy of the osf-file should be stored (including file-name).
  #' @param overwrite Whether local file should be overwritten if already existing.
  #' @param local_file_remove Whether local file should be removed afterwards
  #' @param ... Additional arguments passed to read.csv()
  #'
  #' @return downloaded file
  #' @export
  #'
  #' @examples
  #' Combine multiple data-frames of same kind
  #' osf_node <- osf_retrieve_node("XXXXX")
  #' osf_datafiles <- osf_ls_files(osf_node, path = "data", n_max = Inf)
  #' combined_data <- NULL
  #' for(i in 1:nrow(osf_datafiles)){
  #'   tmp_download <- osf_load_file(osf_datafiles[i,]$id)
  #'   combined_data <- rbind(combined_data, tmp_download)
  #' } 
  osf_load_file <- function(x, filename, path = "./", conflicts = "overwrite", local_file_remove = TRUE, ...){
    
    osf_download(osf_retrieve_file(x), path = path, conflicts = conflicts)  # download file as temporary file to disk
    tmp_file <- read.csv(filename, ...)  # read the temp-file into R
    if (local_file_remove) {  # remove file if activated
      file.remove(paste0(path, filename))
    }
    return(tmp_file)  # return file
  }
  
  
  
  #' Upload Files to the OSF 
  #'
  #' Uploads a given file to the OSF (PAT token required) by using functionality of `osfr`` package.
  #'
  #' @param osf_node A character-string: code of osf-node.(see osfr::retrieve_node())
  #' @param remote_path A charachter string path (OR ONLY FILENAME IN CASE osf_direct_dir is specified) to the intended storage location on the osf.
  #' @param local_path  A character-string path to the local file to be uploaded.
  #' @param overwrite Whether local file should be overwritten if already existing.
  #' @param local_file_remove Whether local file should be removed afterwards.
  #' @param osf_direct_dir If specified, the path can be determined manually instead of checking path for every file (much faster than giving file via filepath)
  #'
  #' @return 
  #' @export 
  #'
  #' @examples
  #' Upload using automatic path handling given a node
  #' osf_upload_file("XXXXX", remote_path = "data/combined_data.csv")
  #' 
  #' Manual specification of same path with osf_direct_dir (faster because not checking directory in every loop iteration)
  #' osf_upload_file("XXXXX", remote_path = "combined_data.csv", osf_direct_dir = "data")
  osf_upload_file <- function(osf_node, remote_path, local_path, conflicts  = "error",  local_file_remove = TRUE, osf_direct_dir = NA){
    
    file_remotepath <- dirname(remote_path)  # retrieve path to remote file
    file_basename <- basename(remote_path)  # retrieve filename of remote file
    
    if (is.na(osf_direct_dir)) {  # if remote directory is not specified via osf_direct_dir
      
      if (grepl( "/", file_remotepath)) {  # check if path includes sub-folders
        # split path into lower and upper (i.e. destination directory) part
        lower_path <- substr(file_remotepath, 1, stri_locate_last(file_remotepath, fixed = c("/"))) # locate last folder change in path and get lower part
        upper_path <- substr(file_remotepath, stri_locate_last(file_remotepath, fixed = c("/")) + 1, nchar(file_remotepath)) # and upper path
      } else {# if path has no sub-directories upper path is given directory
        lower_path <- NULL
        upper_path <- file_remotepath
      }
      if (upper_path %!in% osf_ls_files(osf_node, path = lower_path, n_max = Inf, type = "folder")$name) {  # check if directory already exists on OSF
        remote_dir <- osf_mkdir(osf_node,file_remotepath)  # if not, create dir
      } else {
        remote_dir <- osf_ls_files(osf_node, path = lower_path, n_max = Inf, type = "folder")[which(osf_ls_files(osf_node, path = lower_path, n_max = Inf, type = "folder")$name == upper_path),]  # if exists, get remote-path id
      }
      
      
    } else {
      remote_dir <- osf_direct_dir  # pass osf_direct_dir to upload function
    }
    
    osf_upload(remote_dir, path = local_path, conflicts = "overwrite")  # upload file
    
    # if applicable, remove local file after upload
    if (local_file_remove) {
      file.remove(local_path)
    }
    
  }
  
  # function to scale and/or center all variables in data-frame
  standardize_vars <- function(data, vars = c(), s = TRUE, c = TRUE, factorize = FALSE, ordered = c()) {
    
    tmp_data <- data
    
    if (!factorize) {
      for (i in 1:ncol(tmp_data)) {
        if (length(vars) == 0) {
          if (class(tmp_data[,i]) %in% c("integer", "numeric")) {
            if (s) {
              tmp_colname <- paste0(colnames(tmp_data[i]), "_s")
              data <- cbind(data, scale(tmp_data[,i], scale = T))
              names(data)[ncol(data)] <- tmp_colname
            }
            if (c) {
              tmp_colname <- paste0(colnames(tmp_data[i]), "_c")
              data <- cbind(data, scale(tmp_data[,i], scale = F))
              names(data)[ncol(data)] <- tmp_colname
            }
          }
        } else {
          if (names(data)[i] %in% vars) {
            if (s) {
              tmp_colname <- paste0(colnames(tmp_data[i]), "_s")
              data <- cbind(data, scale(tmp_data[,i], scale = T))
              names(data)[ncol(data)] <- tmp_colname
            }
            if (c) {
              tmp_colname <- paste0(colnames(tmp_data[i]), "_c")
              data <- cbind(data, scale(tmp_data[,i], scale = F))
              names(data)[ncol(data)] <- tmp_colname
            }
          }
        }
      }
    } else {
      for (i in 1:ncol(tmp_data)) {
        if (names(data)[i] %in% vars) {
          if (names(data)[i] %in% ordered) {
            tmp_colname <- paste0(colnames(tmp_data[i]), "_ford")
            data <- cbind(data, factor(tmp_data[,i], ordered = T))
          } else {
            tmp_colname <- paste0(colnames(tmp_data[i]), "_f")
            data <- cbind(data, factor(tmp_data[,i]))
          }
          names(data)[ncol(data)] <- tmp_colname
        }
      }
    }
    return(data)
  }
  

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_violinhalf <- function(mapping = NULL,
                            data = NULL,
                            stat = "ydensity",
                            position = "dodge",
                            trim = TRUE,
                            flip = FALSE,
                            scale_factor = 1,
                            scale = c("area", "count", "width"),
                            show.legend = NA,
                            inherit.aes = TRUE,
                            ...) {
  scale <- match.arg(scale)

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomViolinHalf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      flip = flip,
      scale_factor = scale_factor,
      ...
    )
  )
}

GeomViolinHalf <- ggproto("GeomViolinHalf", Geom,
  extra_params = c("na.rm", "flip", "scale_factor"),
  setup_data = function(data, params) {
    data$width <- data$width %||% params$width %||% (resolution(data$x, FALSE) * 0.9)

    data <- do.call(rbind, lapply(split(data, data$group), function(.group) {
      .group$ymin <- min(.group$y)
      .group$ymax <- max(.group$y)
      .group$xmin <- .group$x
      .group$xmax <- .group$x + .group$width / 2
      .group
    }))
  },
  draw_group = function(data, panel_scales, coord, flip, scale_factor) {
    data$xminv <- data$x

    if (is.logical(flip)) {
      if (flip) {
        data$xmaxv <- data$x - scale_factor * data$violinwidth * (data$xmax - data$x)
      } else {
        data$xmaxv <- data$x + scale_factor * data$violinwidth * (data$xmax - data$x)
      }
    } else if (is.numeric(flip)) {
      if (unique(data$group) %in% flip) {
        data$xmaxv <- data$x - scale_factor * data$violinwidth * (data$xmax - data$x)
      } else {
        data$xmaxv <- data$x + scale_factor * data$violinwidth * (data$xmax - data$x)
      }
    }

    mindata <- maxdata <- data
    mindata$x <- mindata$xminv
    mindata <- mindata[order(mindata$y), , drop = FALSE]
    maxdata$x <- maxdata$xmaxv
    maxdata <- maxdata[order(maxdata$y, decreasing = TRUE), , drop = FALSE]
    newdata <- rbind(mindata, maxdata)

    newdata <- rbind(newdata, newdata[1, ])

    grob <- GeomPolygon$draw_panel(newdata, panel_scales, coord)
    grob$name <- grid::grobName(grob, "geom_violinhalf")
    grob
},

  draw_key = draw_key_polygon,
  default_aes = aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    linewidth = 0.5,
    alpha = NA,
    linetype = "solid"
  ),
  required_aes = c("x", "y")
)




  beexplot <- function(d, x, y, factor_order = NULL, color_pal = "Spectral", ...){
    
    # y_med <- median(d$y)
    # y_q25 <- unname(quantile(raw_data[,which(names(raw_data) == outcome)], 0.25))
    # y_q75 <- unname(quantile(raw_data[,which(names(raw_data) == outcome)], 0.75))
    # d_tmp <- d
    x <- deparse(substitute(x))
    if(!is.null(factor_order)){
      d[[x]] <- factor(d[[x]], levels = factor_order)
    } else {
      d[[x]] <- factor(d[[x]])
    }
    y <- deparse(substitute(y))
    tsum <- tapply(d[[y]], d[[x]], summary)
    y_med <-  sapply(1:length(tsum), function(x) unname(tsum[[x]][3]))
    y_q25 <- sapply(1:length(tsum), function(x) unname(tsum[[x]][2]))
    y_q75 <-  sapply(1:length(tsum), function(x) unname(tsum[[x]][5]))
    
    
    # point_colors <- sapply(1:nrow(d), function(i) RColorBrewer::brewer.pal(11, color_pal)[which(levels(d[[x]]) == d[[x]][i])])
    point_colors <- sapply(1:nrow(d), function(i) c("#005073", "#107dac", "#189ad3", "#1ebbd7", "#71c7ec")[which(levels(d[[x]]) == d[[x]][i])])
    line_colors <- sapply(levels(d[[x]]), function(i) c("#001822", "#042533", "#072e3f", "#093840", "#213b46")[which(levels(d[[x]]) == i)])
    
    p <- 
      ggplot() + 
      geom_beeswarm(data = d, aes(x = d[[x]], y = d[[y]], group = d[[x]]), color = point_colors, alpha = .20) +
      scale_x_discrete(labels= levels(d[[x]])) +
      stat_summary(aes(levels(d[[x]]), y = y_med),fun.y = mean,fun.ymin = mean, fun.ymax = mean,
                   geom = "crossbar", width = 0.5, size = 0.3, linetype = 1, colour = line_colors) +
      stat_summary(aes(levels(d[[x]]), y = y_q25),fun.y = mean,fun.ymin = mean, fun.ymax = mean,
                   geom = "crossbar", width = 0.5, size = 0.3, linetype = 1, colour = line_colors) +
      stat_summary(aes(levels(d[[x]]), y = y_q75),fun.y = mean,fun.ymin = mean, fun.ymax = mean,
                   geom = "crossbar", width = 0.5, size = 0.3, linetype = 1, colour = line_colors) +
      labs(x = gsub("-|_|\\.", " ", x), y = gsub("-|_|\\.", " ", y), title = paste0("Beexplot of ", gsub("-|_|\\.", " ", y), " per ", gsub("-|_|\\.", " ", x)))+
      theme_bw()
    return(p)
  }
  



}


