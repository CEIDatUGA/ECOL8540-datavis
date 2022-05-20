modelgraph <- function(states, flows, groups=NULL) {
  # States
  
  # state classes 
  if(!("class" %in% names(states))) {states$class <- "undefined"}
  
  # state labels 
  if(!("label" %in% names(states))) {states$label <- as.character(NA)}
  states$label <- ifelse(states$class != "invisible" & is.na(states$label), 
                         states$name, # copy name to label 
                         replace(states$label, is.na(states$label), "")
  )
  # state positions 
  if(!("position" %in% names(states))) {states$position <- ""}
  for(i in 2:nrow(states)){
    if(is.na(states$position[i]) | states$position[i] == ""){
      states$position[i] <- paste0("right of=",states$name[i-1])
    }
  }
  
  states_tex <- "% states\n"
  for (i in 1:nrow(states)) {
    tex <- paste0(
      r"(\node[)", states$class[i], # named class sets the color and style
      r"(] ()", states$name[i], # name
      r"() [)", states$position[i], # position
      r"(] {)", states$label[i], # label
      r"(})",";\n"
    )
    states_tex <- paste0(states_tex, tex)
  }
  
  # Flows
  if(!("arrow.properties" %in% names(flows))) {flows$arrow.properties <- ""}
  if(!("label.properties" %in% names(flows))) {flows$label.properties <- "above"}
  
  flows_tex <- "% flows\n\\path [->]\n"
  for (i in 1:nrow(flows)) {
    tex <- paste0(
      r"(()", flows$from[i], r"() )", # Flow from this state
      r"(edge[)", flows$arrow.properties[i], r"(] )", # arrow positioning
      r"(node[)", flows$label.properties[i], r"(] )", # label positioning
      r"({)", flows$label[i],  r"(} )", # label
      r"(()", flows$to[i],  r"{)}",
      "\n")
    flows_tex <- paste0(flows_tex, tex)
  }
  flows_tex <- paste0(flows_tex, ";\n")
  
  # Groups
  
  if("group" %in% names(states) & is.null(groups)) {
    groups <- data.frame(name = unique(states$group),
                         class = "population"
    )
  }
  
  if(!is.null(groups) & !("states" %in% names(groups))) {
    g <- data.frame(name = unique(states$group))
    for(i in 1:nrow(g)) {
      g$states <- list(c(states$name[which(states$group == g$name[i])]))
    }
    groups <- merge(x=groups,y=g,by="name", all.x=TRUE)
  }
  
  groups_tex <- ""
  if(!is.null(groups)){
    for (i in 1:nrow(groups)) {
      tex <- paste0(
        r"(\node[nodegroup, )", groups$class[i], r"(, )", 
        "fit=", paste(paste0("(",groups$states[i][[1]],")"),collapse = " "),  r"(, )",
        r"(label={)", groups$label[i], r"(}] )",
        r"{(}",  groups$name[i],  r"[]) {}]",
        ";\n"
      )
      groups_tex <- paste0(groups_tex, tex)
    }
  }
  
  groups_tex <- paste0("% groups\n", r"(\begin{scope}[on background layer])",
                       "\n", groups_tex, 
                       r"(\end{scope})", "\n"
  )
  
  g <- paste0(
    r"(\begin{tikzpicture}[modelgraph])","\n\n",
    states_tex, "\n", flows_tex, "\n", groups_tex, "\n",
    r"(\end{tikzpicture})", "\n"
  ) 
  return(g)
} 

saveModelgraph <- function(fragment, stem, format="tex", standalone=FALSE, 
                           includes=rmarkdown::metadata$'header-includes', 
                           dpi=300, compression = "lzw") {
  if(!format %in% c("tex","pdf","png","tiff")) {
    stop("Format must be tex, pdf, png, or tiff.")
  }
  if(format=="tex" & standalone==FALSE){
    writeLines(fragment,paste0(stem,".tex"))
  }
  if((standalone==TRUE) | format %in% c("pdf","png","tiff")){
    tex <- paste0(r"---(\documentclass{standalone})---","\n",
                  r"---(\input{modelgraphs.tex})---","\n",
                  paste(includes, collapse  = "\n"),"\n",
                  r"---(\begin{document})---","\n",
                  fragment,"\n",
                  r"---(\thispagestyle{empty})---","\n",
                  r"---(\end{document})---","\n"
    )
    writeLines(tex, paste0(stem,".tex"))
  }
  if(format %in% c("pdf","png","tiff")){
    tinytex::xelatex(paste0(stem,".tex"))
  }
  if(format %in% c("png","tiff")){
    bitmap <- pdftools::pdf_render_page(
      pdf = paste0(stem,".pdf"),
      dpi = dpi, antialias = TRUE
    )
    png::writePNG(bitmap, paste0(stem,".png"))
    if(format == "tiff") {
      nativeRaster <- png::readPNG(paste0(stem,".png"), native = TRUE)
      tiff::writeTIFF(nativeRaster, paste0(stem,".tiff"),
                      bits.per.sample = 8L,
                      compression = compression,
                      reduce = TRUE)
    }
  }
}

# tools::texi2pdf("SEIRS_modelegraph_standalone.tex", clean=TRUE, index=FALSE)
# ,texi2dvi = "emulation")

# # "png"  "jpeg" "jpg"  "tiff" "pnm" 
# pdftools::pdf_convert(
# pdf = "SEIRS_modelegraph_standalone.pdf",
# format = "png",
# pages = 1,
# filenames = NULL,
# dpi = 300,
# antialias = TRUE,
# verbose = TRUE
# )
# 
# 

# Magick
# im <- magick::image_read_pdf("SEIRS_modelegraph_standalone.pdf", pages = 1, density = 300)

# # MacOS
# system("sips -s dpiHeight 1200 -s dpiWidth 1200 -s format png  SEIRS_modelegraph_standalone.pdf --out SEIRS_modelegraph_standalone.png")
# 
# # poppler
# system("pdftoppm -singlefile -png SEIRS_modelegraph_standalone.pdf SEIRS_modelegraph_standalone")