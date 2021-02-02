package library

object MonProductionResultat extends ProductionResultat { // William et Thomas D
  final val css: String = "a{text-decoration:none;font-size:18px;margin:4px;font-family:'Roboto', sans-serif;font-weight:300;color:white;} a:hover{transition:all 0.05s;font-size:19px;color:rgb(122, 71, 243);} h1{margin:30px;font-size:40px;font-family:'Roboto', sans-serif;font-weight:900;}.links{display:flex;justify-content:center;align-items:center;flex-direction:column;margin:0;}.container{height:100%;width:100%;display:flex;justify-content:center;align-items:center;flex-direction:column;margin:0;}body{color:white;background-color:rgb(26, 25, 27);}"

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
            Tag("h1", List(), List(Text("Voici les résultats de vos recherches"))),
            Tag("div", List(("class", "links")), resultat2htmlRec(l))))))))
  }

  private def resultat2htmlRec(l: List[(String, String)]): List[Tag] = {
    l match {
      case (title, link) :: rest =>
        Tag("a", List(("href", link)), List(Text(title))) :: resultat2htmlRec(rest)
      case _ => List()
    }
  }
}