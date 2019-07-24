### This is Niko Partanen's R script that splits the National
### Library of Finland's Fenno-Ugrica collections Alto XML files
### into line-image–text-file pairs that can be proofread, and
### used with training models with Tesseract. 
### The approach is to reuse the ABBYY FineReader's line detection,
### and just to create a new, possibly improved, OCR results to
### accompany those
### Data source:
### http://fennougrica.kansalliskirjasto.fi/handle/10024/87597

library(tidyverse)
library(xml2)
library(measurements)
library(magick)
library(fs)


# This function reads Alto file and saves it as individual files.
# Directory is simply the folder name, in this case test or train. 

# output_textfile should be .txt or .gt.txt

alto2lines <- function(xml_file, target_directory, output_textfile){
  
  # In the beginning we want an empty directory
  # ATTENTION WITH THIS
  
  #if (dir_exists(target_directory)){
  #  dir_delete(target_directory)
  #}
  
  #dir_create(target_directory)
  
  xml_basename <- str_extract(xml_file, "[^/]+(?=.xml)")
  
  xml <- read_xml(xml_file)
  
  image_tif <- xml_file %>% str_replace_all(".xml$", ".tif") %>%
    image_read() %>%
    image_threshold(type = "white") %>%
    image_convert(colorspace = "Gray") # simple binarization
  
  # Alto files use tenth of millimeter as the measure unit,
  # so units need to be converted to pixels -- this seems to work
  # !! Fenno-Ugrica Alto uses pixels, this should probably be checked
  # !! automatically.
  
  #  mm10inch <- function(number){
  #    measurements::conv_unit((number / 10), "mm", "inch") * 300
  #  }
  
  # This function saves individual line
  
  save_line <- function(page_image, info){
    crop_string <- str_glue("{info$width}x{info$height}+{info$hpos}+{info$vpos}")
    cropped_line <- image_crop(page_image, crop_string)
    image_write(cropped_line, str_glue("{target_directory}/{xml_basename}-{info$order_id}.png"), format = "png")
    write_lines(info$content, str_glue("{target_directory}/{xml_basename}-{info$order_id}{output_textfile}"))
  }
  
  #  count_edits <- function(edits){
  #    length(edits[edits == "true"]) / length(edits)
  #  }
  
  # Saving the lines is done in the end of this function.
  # But we start that only in the case there are TextLines
  # at the page!
  
  if (length(xml %>% xml_find_all("//d1:TextLine")) != 0){
    
    xml %>% xml_find_all("//d1:TextLine") %>%
      map_df(~ tibble(# id = .x %>% xml_attr("ID"),
        hpos = .x %>% xml2::xml_attr("HPOS") %>% as.numeric(),
        vpos = .x %>% xml_attr("VPOS") %>% as.numeric(),
        width = .x %>% xml_attr("WIDTH") %>% as.numeric(),
        height = .x %>% xml_attr("HEIGHT") %>% as.numeric(),
        content = .x %>% xml_find_all("./d1:String") %>% xml_attr("CONTENT") %>%
          paste0(collapse = " "),
        # box_id = .x %>% xml_attr("ID"),
        height_page = .x %>%
          xml_find_first("//d1:Page/d1:PrintSpace") %>%
          xml_attr("HEIGHT") %>% as.numeric,
        width_page = .x %>%
          xml_find_first("//d1:Page/d1:PrintSpace") %>%
          xml_attr("WIDTH") %>% as.numeric)
      ) %>% # View
      #    mutate_if(is.double, mm10inch) %>% # here we do mm10inch
      filter(! content == '') %>%
      #    filter(! status > 0.60) %>%
      mutate(order_id = 1:n() %>% str_pad(width = 4, pad = "0")) %>%
      mutate(xmax = hpos + width,
             xmin = hpos,
             ymax = vpos + height,
             ymin = vpos) %>% 
      mutate_if(is.double, round, digits = 0) %>%
      split(.$order_id) %>%
      walk(~ {save_line(page_image = image_tif, info = .x)})
    
  }
  
}
