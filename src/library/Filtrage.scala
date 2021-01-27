package library

object Filtrage extends FiltrageHtml { // Par Thomas B
  /**
   * A partir d'un document Html h et d'une requête e, dit si le document
   * satisfait l'expression e
   *
   * @param h le document Html
   * @param e l'expression
   * @return true si le document satisfait l'expression e
   */
  def filtreHtml(h: Html, e: Expression): Boolean = {
    e match {
      case Word(w)   => annexe(h, w)
      case And(a, b) => filtreHtml(h, a) && filtreHtml(h, b)
      case Or(a, b)  => filtreHtml(h, a) || filtreHtml(h, b)
    }
  }

  def annexe(h: Html, e: String): Boolean = {
    h match {
      case Text(a) => a.contains(e)
      case Tag(_, _, a) => {
        a match {
          case first :: Nil   => annexe(first, e)
          case first :: reste => annexe(first, e) || annexe2(reste, e)
          case Nil            => false
        }
      }
    }
  }

  def annexe2(h: List[Html], e: String): Boolean = {
    h match {
      case first :: Nil   => annexe(first, e)
      case first :: reste => annexe(first, e) || annexe2(reste, e)
      case Nil            => false
    }
  }
}