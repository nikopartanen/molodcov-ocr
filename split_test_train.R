### This is Niko Partanen's example R script that splits the National
### Library of Finland's dataset OCR Ground Truth Pages (Swedish Fraktur)
### into line-image–text-file pairs that can be used with training
### models with Tesseract. Same approach works easily also with Ocropy
### Data source:
### https://digi.kansalliskirjasto.fi/opendata

library(tidyverse)
library(xml2)
library(measurements)
library(magick)
library(fs)

if (dir_exists("test")){
  dir_delete("test")
}

if (dir_exists("train")){
  dir_delete("train")
}

# We are taking a random sample when test and train set are
# divided, so setting seed is necessary to ensure reproducibility

set.seed(20170818)

# This function reads Alto file and saves it as individual files.
# Directory is simply the folder name, in this case test or train. 

alto2lines <- function(xml_file, target_directory){
  
  if (! dir_exists(target_directory)){
    dir_create(target_directory) 
  }
  
  xml_basename <- str_extract(xml_file, "[^/]+(?=.xml)")
  
  xml <- read_xml(xml_file)
  
  image_tif <- xml_file %>% str_replace_all(".xml$", ".tif") %>%
    image_read() %>%
    image_threshold(type = "white") %>%
    image_convert(colorspace = "Gray") # simple binarization
  
  # Alto files use tenth of millimeter as the measure unit,
  # so units need to be converted to pixels -- this seems to work
  
  mm10inch <- function(number){
    measurements::conv_unit((number / 10), "mm", "inch") * 300
  }
  
  # This function saves individual line
  
  save_line <- function(page_image, info){
    crop_string <- str_glue("{info$width}x{info$height}+{info$hpos}+{info$vpos}")
    cropped_line <- image_crop(page_image, crop_string)
    image_write(cropped_line, str_glue("{target_directory}/{xml_basename}-{info$order_id}.tif"))
    write_lines(info$content, str_glue("{target_directory}/{xml_basename}-{info$order_id}.gt.txt"))
  }
  
  count_edits <- function(edits){
    length(edits[edits == "true"]) / length(edits)
  }
  
  # Saving the lines is done in the end of this function
  
  xml %>% xml_find_all("//TextLine") %>%
    map_df(~ tibble(id = .x %>% xml_attr("ID"),
                    hpos = .x %>% xml2::xml_attr("HPOS") %>% as.numeric(),
                    vpos = .x %>% xml_attr("VPOS") %>% as.numeric(),
                    width = .x %>% xml_attr("WIDTH") %>% as.numeric(),
                    height = .x %>% xml_attr("HEIGHT") %>% as.numeric(),
                    content = .x %>% xml_find_all("./String") %>% xml_attr("CONTENT") %>%
                      paste0(collapse = " "),
                    box_id = .x %>% xml_attr("ID"),
                    status = .x %>% xml_find_all("./String") %>% 
                      xml_attr("CHANGED") %>%
                      count_edits,
                    height_page = .x %>%
                      xml_find_first("//Page") %>%
                      xml_attr("HEIGHT") %>% as.numeric,
                    width_page = .x %>%
                      xml_find_first("//Page") %>%
                      xml_attr("WIDTH") %>% as.numeric)
    ) %>% # View
    mutate_if(is.double, mm10inch) %>% # here we do mm10inch
    filter(! content == '') %>%
    filter(! status > 0.60) %>%
    mutate(order_id = 1:n() %>% str_pad(width = 4, pad = "0")) %>%
    mutate(xmax = hpos + width,
           xmin = hpos,
           ymax = vpos + height,
           ymin = vpos) %>% 
    mutate_if(is.double, round, digits = 0) %>%
    split(.$order_id) %>%
    walk(~ {save_line(page_image = image_tif, info = .x)})
  
}

# Here we list the files

gt_files <- dir("postilla/testi/", 
                pattern = "xml", 
                recursive = TRUE,
                full.names = TRUE)

# Here the test and train set are split, 15% goes to testing.
# OCR training usually splits another test set while training,
# but for further evaluation this is useful.

test <- sample(gt_files, length(gt_files) * 0.15)
train <- gt_files[! (gt_files %in% test)]

# Both file lists are processed into lines

test %>%
  walk(~ {alto2lines(xml_file = .x, target_directory = "test")})

train %>%
  walk(~ {alto2lines(xml_file = .x, target_directory = "train")})