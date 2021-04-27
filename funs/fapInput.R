
#' Menu deroulant pour selectionner un code FAP
#'
#' @param inputId Id de l'input.
#' @param selected Valeur initiale de l'input.
#' @param width Largeur de l'element.
#'
#' @return Tag HTML a inclure dans l'UI
#' @export
#' 
#' @importFrom htmltools tags tagAppendAttributes
#' @importFrom shinyWidgets pickerInput pickerOptions
#'
fapInput <- function(inputId, selected = "A0Z", keywords = NULL, width = "auto") {
  
  if (!is.null(keywords) & is.data.table(keywords)) {
    faps <- data.table(fap = unlist(liste_libelles(), use.names = FALSE))
    faps <- merge(x = faps, y = keywords, by = "fap", all.x = TRUE, sort = FALSE)
    keywords <- faps$keywords
  } 
  
  tags$div(
    class = "center-block",
    htmltools::tagAppendAttributes(
      pickerInput(
        inputId = inputId,
        label = NULL, 
        choices = liste_libelles(),
        selected = selected,
        width = width,
        options = pickerOptions(
          liveSearch = TRUE, 
          liveSearchPlaceholder = "Saisissez un mot clef",
          size = 20, 
          style = "btn-default btn-round"
        ),
        choicesOpt = list(
          tokens = keywords
        ),
        inline = FALSE
      ),
      class = "center-block",
      style = "text-align: center;"
    )
  )
}






