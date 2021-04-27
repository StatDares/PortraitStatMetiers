
#  ------------------------------------------------------------------------
#
# Title : Module documentation
#    By : Dares
#  Date : 2020
#
#  ------------------------------------------------------------------------

file_documentation = "documentation/README.md"
if (version_site_dares){
  file_documentation = "documentation/README_without_intro.md"
}

documentation_UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container-fluid",
      tags$div(
        class = "col-xs-12 col-md-8 col-md-offset-2",
        includeMarkdown(path = file_documentation), style = "text-align: justify;"
      )
    )
  )
}

documentation <- function(input, output, session) {
  
}

