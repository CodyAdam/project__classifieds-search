package library

object MonFiltrageHtml extends FiltrageHtml { // Par Thomas B
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
			case Word(mot)   => annexe(h, mot)
			case And(expression1, expression2) => filtreHtml(h, expression1) && filtreHtml(h, expression2)
			case Or(expression1, expression2)  => filtreHtml(h, expression1) || filtreHtml(h, expression2)
			}
	}

	/**
	 * A partir d'un document Html h et d'un String e, dit si le document contient e
	 * 
	 * @param h le document html
	 * @param e le String
	 * @return true si le document contient e
	 */
	def annexe(h: Html, e: String): Boolean = {
			h match {
			case Text(texte) => texte.contains(e)
			case Tag(_, _, htmlList) => {
				htmlList match {
				case first :: Nil   => annexe(first, e)
				case first :: reste => annexe(first, e) || annexe2(reste, e)
				case Nil            => false
				}
			}
			}
	}

	/**
	 * A partir d'une liste de document Html h et d'un String e, dit si les document contienent e
	 * 
	 * @param h la liste de document html
	 * @param e le String
	 * @return true si e est contenu dans la liste des documents html
	 */
	def annexe2(h: List[Html], e: String): Boolean = {
			h match {
			case first :: Nil   => annexe(first, e)
			case first :: reste => annexe(first, e) || annexe2(reste, e)
			case Nil            => false
			}
	}
}
