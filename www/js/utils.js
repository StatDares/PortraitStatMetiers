
// JavaScript utilitaires

function scrollTop() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}

function ajoutTitre(ns) {
  var titre = document.getElementById(ns + "titre");
  if (titre !== null) {
    var titreModal = document.getElementById(ns + "titre_modal");
    titreModal.innerHTML = titre.innerHTML;
  }
}

$(function() {
  $(".navbar-header").addClass("hidden-sm");
  
  $(".scroll-top").on("click", scrollTop);
});


