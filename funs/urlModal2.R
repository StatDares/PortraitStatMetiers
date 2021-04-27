

urlModal2 <- function (url, title = "Bookmarked application link", subtitle = NULL) {
  subtitleTag <- tagList(
    br(), span(
      class = "text-muted", 
      subtitle
    ), 
    span(
      id = "shiny-bookmark-copy-text", class = "text-muted"
    )
  )
  modalDialog(
    title = title,
    easyClose = TRUE, 
    footer = modalButton("Fermer"),
    tags$textarea(
      class = "form-control", 
      rows = "1", style = "resize: none;", readonly = "readonly", 
      url
    ),
    subtitleTag,
    tags$script(
      "$('#shiny-modal').
        one('show.bs.modal', function() {
          setTimeout(function() {
            var $textarea = $('#shiny-modal textarea');
            $textarea.innerHeight($textarea[0].scrollHeight);
          }, 200);
        });
      $('#shiny-modal')
        .one('shown.bs.modal', function() {
          $('#shiny-modal textarea').select().focus();
        });
      $('#shiny-bookmark-copy-text')
        .text(function() {
          if (/Mac/i.test(navigator.userAgent)) {
            return 'Appuyez sur <U+2318>-C pour copier.';
          } else {
            return 'Appuyez sur Ctrl-C pour copier.';
          }
        });"
    )
  )
}
