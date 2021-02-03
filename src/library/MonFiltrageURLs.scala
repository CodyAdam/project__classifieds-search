package library;

object MonFiltrageURLs extends FiltrageURLs { // par Arthur et Mael

  /**
   * A partir d'un document Html h, rend la liste des URLs accessibles à partir
   * de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
   * d'annonces du site de référence
   *
   * @param h le document Html
   * @return la liste des URLs d'annonces contenues dans h
   */
  def filtreAnnonce(h: Html): List[String] = {
    filtreAnnonceRec(List(h))
  }

  /**
   * A partir d'un liste d'éléments de type Html, rend la liste des URL qui sont des annonces 
   * en parcourant en récursif chaque élément html
   * @param lH la liste des éléments html du document html
   * @return la liste des URLs d'annonces contenues dans lH
   */
  private def filtreAnnonceRec(lH: List[Html]): List[String] = {
    lH match {
      case Nil => Nil
      case html :: rest => html match {
        case Text(_) => filtreAnnonceRec(rest)
        case Tag(name, attributes, children) =>
          if (name.equals("a")) {
            val link = linkIsValid(attributes);
            if (link != "") List(link) ++ filtreAnnonceRec(children) ++ filtreAnnonceRec(rest)
            else filtreAnnonceRec(children) ++ filtreAnnonceRec(rest)
          } else
            filtreAnnonceRec(children) ++ filtreAnnonceRec(rest)
      }
    }
  }

  
  /**
   * A partir d'une liste d'attributs d'une balise renvoie le lien sous forme de String si c'est une annonce
   * @param l une liste de tuples de chaînes de caractères 
   * @return une URL valide correspondant à une annonce
   */
  private def linkIsValid(l: List[(String, String)]): String = {
    var link = "";
    var isAProduct = false;
    for ((attributeName, attributeValue) <- l) {
      if (attributeName == "class" && attributeValue == "clad__ad_link") isAProduct = true;
      else if (attributeName == "href" && attributeValue.contains("http")) link = attributeValue;
    }
    if (isAProduct) return link;
    return "";
  }
}
