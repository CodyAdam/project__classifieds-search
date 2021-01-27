package library

trait AnalysePage{
/** A partir d'une URL de requête sur le site de référence et d'une expression exp, 
    retourne de pages issues de la requête et satisfaisant l'expression.

    @param url l'URL de la requête sur le site de référence
    @param exp l'expression à vérifier sur les pages trouvées
    @return la liste des couples (titre,ref) où ref est l'URL d'une page
            satisfaisant l'expression et titre est son titre. */
  val objFiltrageUrls:FiltrageURLs
  val objFiltrageHtml:FiltrageHtml
  def resultats(url:String,exp:Expression):List[(String,String)]
}


trait FiltrageURLs{
/** A partir d'un document Html h, rend la liste des URLs accessibles à partir
    de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
    d'annonces du site de référence
 
    @param h le document Html
    @return la liste des URLs d'annonces contenues dans h
 */
  def filtreAnnonce(h:Html):List[String]
}
object MonFiltrageURLs extends FiltrageURLs {

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

trait FiltrageHtml{
/** A partir d'un document Html h et d'une requête e, dit si le document
    satisfait l'expression e 

    @param h le document Html
    @param e l'expression 
    @return true si le document satisfait l'expression e
*/
  def filtreHtml(h:Html,e:Expression):Boolean
}

trait ProductionResultat{
/** A partir d'une liste de couples (titre,URL), produit un document Html, qui
    liste les solutions sous la forme de liens cliquables

    @param l la liste des couples solution (titre,URL)
    @return le document Html listant les solutions
 */
  def resultat2html(l:List[(String,String)]):Html
}


trait Html2String{
/** Produit la chaîne de caractère correspondant à un document Html
    
    @param h le document Html
    @return la chaîne de caractère représentant h
 */
  def process(h:Html):String
}

object ToString extends Html2String{
  
  def process(h:Html):String = {
    h match{
      case(Tag(balise,attributs,childs)) => "<" + balise + " " +  processRecAttributs(attributs) + ">" + "\n" + processRec(childs)  + "</" + balise + ">" //gère les balises
      case(Text(texte)) => texte +"\n"
      case null => ""
     
    }
    
  }
  
  def processRec(l : List[Html] ) : String = {
    l match{
         case(Tag(balise,attributs,childs) :: d) => "<" + balise + " " + processRecAttributs(attributs) + ">"  + "\n"+ processRec(childs)  + "</" + balise + ">" +"\n" + processRec(d)
         case Text(texte) :: reste => texte + "\n" + processRec(reste)
         case Nil => ""
         
    }
  }
  
  def processRecAttributs(l : List[(String,String)] ) : String = {
    l match{
      case (att1,att2) :: Nil => att1  + "=\"" + att2 + "\""
      case (att1,att2) :: reste => att1 + "=\"" + att2 + "\";" + processRecAttributs(reste) 
      case Nil => ""
      
    }
  }

                       				
}

