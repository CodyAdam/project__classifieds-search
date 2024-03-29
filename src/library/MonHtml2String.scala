package library

object MonHtml2String extends Html2String { // William B & Thomas D
  /**
   * Produit la chaîne de caractère correspondant à un document Html
   *
   * @param h le document Html
   * @return la chaîne de caractère représentant h
   */
  def process(h: Html): String = {
    h match {
      case (Tag(balise, attributs, childs)) => "<" + balise + processRecAttributs(attributs) + ">" + "\n" + processRec(childs) + "</" + balise + ">" //gère les balises
      case (Text(texte))                    => texte + "\n"
      case null                             => ""
    }
  }

  /**
   * Produit la chaîne de caractère correspondant à une liste d'éléments Html
   * 
   * @param l une liste d'éléments de type Html 
   * @return la chaine de caractère représentant l
   */
  private def processRec(l: List[Html]): String = {
    l match {
      case (Tag(balise, attributs, childs) :: d) => "<" + balise + processRecAttributs(attributs) + ">" + "\n" + processRec(childs) + "</" + balise + ">" + "\n" + processRec(d)
      case Text(texte) :: reste                  => texte + "\n" + processRec(reste)
      case Nil                                   => ""
    }
  }

   /**
   * Produit la chaîne de caractères correspondant aux attributs d'un élément Html
   * 
   *@param l une liste d'attributs dans la page Html
   *@return la chaine de caractère représentant l
   */
  private def processRecAttributs(l: List[(String, String)]): String = {
    l match {
      case (att1, att2) :: Nil   => " " + att1 + "=\"" + att2 + "\""
      case (att1, att2) :: reste => " " + att1 + "=\"" + att2 + "\";" + processRecAttributs(reste)
      case Nil                   => ""
    }
  }
}
