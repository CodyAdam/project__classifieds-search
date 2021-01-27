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
    var lH = List(h)
    filtreAnnonceRec(lH)
  }

  private def filtreAnnonceRec(h: List[Html]): List[String] = {
    h match {
      case Nil => Nil
      case html :: l2 => html match {
        case Text(_) => filtreAnnonceRec(l2)
        case Tag(a, l, child) =>
          if (a.equals("a")) {
            List(baliseListeMatch(l)) ++ filtreAnnonceRec(child) ++ filtreAnnonceRec(l2)
          }
          else
            filtreAnnonceRec(child) ++ filtreAnnonceRec(l2)
      }
    }
  }

  private def baliseListeMatch(l: List[(String, String)]): String = {
    l match {
      case Nil => ""
      case (a, b) :: l2 =>
        if (a == "href") b
        else baliseListeMatch(l2)
    }
  }

}
