package library

/**
 * Implementation du trait ProductionResultat
 */
object MonProductionResultat extends ProductionResultat { // Thomas D et Cody
   final val css: String = "a{text-decoration:none;font-size:18px;padding:4px;font-family:Roboto,sans-serif;font-weight:300;color:#fff;transition:transform ease .5s;text-align:center}a:hover{transform:scale(1.2);font-size:25px;color:#7a47f3}h1{margin:30px;font-size:40px;font-family:Roboto,sans-serif;font-weight:900}.links{display:flex;justify-content:center;align-items:center;flex-direction:column;margin:0}.container{height:100%;width:100%;display:flex;align-items:center;flex-direction:column;margin:0;overflow-y:auto}body{color:#fff;background-color:#1a191b}"
   /**
   * A partir d'une liste de couples (titre,URL), produit un document Html, qui
   * liste les solutions sous la forme de liens cliquables
   *
   * @param l la liste des couples solution (titre,URL)
   * @return le document Html listant les solutions
   */
  def resultat2html(l: List[(String, String)]): Html = {
    Tag("html", List(),
      List(
        Tag("head", List(),
          List(
            Tag("meta", List(("content", "text/html"), ("charset", "utf-8")), List()),
            Tag("link", List(("rel", "preconnect"), ("href", "https://fonts.gstatic.com")), List()),
            Tag("link", List(("rel", "stylesheet"), ("href", "https://fonts.googleapis.com/css2?family=Roboto:wght@300;900&display=swap")), List()),
            Tag("title", List(), List(Text("Recherche Viva Street"))), Tag("style", List(), List(Text(css))))),

        Tag("body", List(), List(
          Tag("div", List(("class", "container")), List(
            Tag("h1", List(), List(Text("Voici les résultats de votre recherche :"))),
            Tag("div", List(("class", "links")), resultat2htmlRec(l))))))))
  }

   /**
    * Sous Fonction de resultat2html qui A partir de la liste de couples (titre,URL),
    * Produit une liste de Tag correspondant aux balise <a> avec les liens cliquables
    * 
    * @param l la liste des couples solution (titre,URL)
    * @return la liste des balises des liens cliquables
    */
  private def resultat2htmlRec(l: List[(String, String)]): List[Tag] = {
    l match {
      case (title, link) :: rest =>
       Tag("a", List(("href", link)), List(Text(title))) :: resultat2htmlRec(rest)
      case _ => List()
    }
  }
}
