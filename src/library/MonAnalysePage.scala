package library;

object MonAnalysePage extends AnalysePage { //partie Zoé

  val objFiltrageUrls: FiltrageURLs = MonFiltrageURLs
  val objFiltrageHtml: FiltrageHtml = MonFiltrageHtml

  /**
   * A partir d'une URL de requête sur le site de référence et d'une expression exp,
   * retourne une liste de pages issues de la requête et satisfaisant l'expression.
   *
   * @param url l'URL de la requête sur le site de référence
   * @param exp l'expression à vérifier sur les pages trouvées
   * @return la liste des couples (titre,ref) où ref est l'URL d'une page
   * satisfaisant l'expression et titre est son titre.
   */
  def resultats(url: String, exp: Expression): List[(String, String)] = {
    val html: Html = UrlProcessor.fetch(url)
    val lUrls: List[String] = MonFiltrageURLs.filtreAnnonce(html)
    var lcouples: List[(String, Html)] = associeHtml(lUrls)
    var lcouplesvalides: List[(String, Html)] = couplesValides(lcouples, exp)
    lienTitre(lcouplesvalides)
  }

  /**
   * @param lUrls une liste d'urls
   * @return la liste de couples qui associe aux Urls en entrée une page html
   */
  private def associeHtml(lUrls: List[String]): List[(String, Html)] = {

    lUrls match {
      case Nil            => Nil
      case first :: reste => List((first, UrlProcessor.fetch(first))) ++ associeHtml(reste)
    }
  }

  /**
   * @param lcouples une liste de couples Url-Html associés
   * @return une liste de couples Url-Html (liste composée des couples de lcouples qui satisfont la requête uniquement)
   */
  private def couplesValides(lcouples: List[(String, Html)], exp: Expression): List[(String, Html)] = {
    lcouples match {
      case Nil => Nil
      case h :: r => if (MonFiltrageHtml.filtreHtml(h._2, exp)) List(h) ++ couplesValides(r, exp)
      else couplesValides(r, exp)
    }
  }
  
  /**
   * @param lcouplesvalides une liste de couples Url-Html
   * @return une liste de html composée des html de chaque couples de lcouplesvalides
   */
  private def htmlvalides(lcouplesvalides: List[(String, Html)]): List[Html] = {
    lcouplesvalides match {
      case Nil    => Nil
      case h :: r => List(h._2) ++ htmlvalides(r)
    }
  }

  /**
   * @param lcouplesvalides une liste de couples url-html qui satisfont la requête
   * @return une liste de couples String/String (Url/Titre) où Titre est le titre de la page
   */
  // parcourt toute la liste des couples valides, et on parcourt l'html pour trouver le titre de la page
  private def lienTitre(lcouplesvalides: List[(String, Html)]): List[(String, String)] = {

    lcouplesvalides match {
      case Nil    => Nil
      case h :: r => List((trouveTitre(htmlvalides(List(h))), h._1)) ++ lienTitre(r)
    }
  }
  
  /**
   * @param lhtml une liste de html
   * @return le titre de la page trouvé
   */
  //renvoie le titre de la page parcourt tous les tags, si le tag est un titre récupérer son texte
  private def trouveTitre(lhtml: List[Html]): String = {
    lhtml match {
      case Nil => ""
      case h :: r => h match {
        case Text(_) => trouveTitre(r)
        case Tag(n, _, l) => if (n.equals("title")) annexeTrouveTitre(l)
        else trouveTitre(r) ++ trouveTitre(l)
      }
    }
  }

  /**
   * @param lhtml une liste de html
   * @return le titre de la page trouvé
   */
  private def annexeTrouveTitre(lhtml: List[Html]): String = {
    lhtml match {
      case Nil => ""
      case h :: r => h match {
        case Text(x)      => x
        case Tag(n, _, l) => annexeTrouveTitre(r) ++ annexeTrouveTitre(l)
      }
    }
  }
}

