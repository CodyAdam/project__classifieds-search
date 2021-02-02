package library;

object MyAnalysePage extends AnalysePage {//partie Zoé
  
  def resultats(url:String,exp:Expression):List[(String,String)]={
    
    val html:Html = library.UrlProcessor.fetch(url)
    val lUrls:List[String] = MonFiltrageURLs.filtreAnnonce(html)
    var lcouples:List[(String, Html)] = annexe(lUrls)
    var lcouplesvalides:List[(String, Html)] = annexe2(lcouples, exp)
    annexe3(lcouplesvalides)
    
  }
  
  def annexe(lUrls:List[String]):List[(String, Html)] ={
    
    lUrls match {
      case Nil => Nil
      case first::reste => List((first, library.UrlProcessor.fetch(first))) ++ annexe (reste)
      
    }
  }
  
  def annexe2(lcouples:List[(String, Html)], exp:Expression):List[(String, Html)] = {
   lcouples match{
     case Nil => Nil
     case h::r => if (Filtrage.filtreHtml(h._2,exp)) List(h) ++ annexe2(r,exp)
                  else annexe2(r, exp)
   }
  }
  
  def htmlvalides(lcouplesvalides:List[(String, Html)]):List[Html] = {
    lcouplesvalides match {
      case Nil => Nil
      case h::r => List(h._2) ++ htmlvalides(r)
    }
  }
  
  // parcourt toute la liste des couples valides, et on parcourt l'html pour trouver le titre de la page
  def annexe3(lcouplesvalides:List[(String, Html)]): List[(String, String)] = {
    
  lcouplesvalides match {
    case Nil => Nil
    case h::r => List((annexe4(htmlvalides(List(h))) , h._1)) ++ annexe3(r)
  }
  
  }
  //renvoie le titre de la page parcourt tous les tags, si le tag est un titre récupérer son texte 
  def annexe4(html:List[Html]) : String = {
    html match{
      case Nil => ""
      case h::r => h match{
                    case Text(_) => annexe4(r)
                    case Tag(n,_,l)=> if (n.equals("title")) annexe5(l)
                                      else annexe4(r) ++ annexe4(l)
      }
      
      
    }
  }
  
  def annexe5(html:List[Html]):String = {
     html match{
      case Nil => ""
      case h::r => h match{
                    case Text(x) => x
                    case Tag(n,_,l)=> annexe5(r) ++ annexe5(l)
      }
      
      
    }
  }
  
}
