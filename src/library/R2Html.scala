package library

object R2Html extends ProductionResultat{
  def resultat2html(l:List[(String,String)]):Html = {
    Tag("html",List(),
         List(Tag("head",List(),
                  List(Tag("meta",List(("content","text/html"),("charset","iso-8859-1")),List()),
                  		Tag("title",List(),List(Text("Recherche Viva Street"))))),
              Tag("body",List(),List(
                  Text("&nbsp"),
                  Tag("center",List(),resultat2htmlRec(l))))))
  }
  
  
  def resultat2htmlRec(l:List[(String,String)]): List[library.Tag] = {
    l match {
      case (s1,s2) :: b => 
        Tag("p",List(),List(Tag("a", List(("href",s2)),List(Text(s1),Tag ("img",List(),List()))))) :: resultat2htmlRec(b)
      case _ => List()
    }
  }
}
