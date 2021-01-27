package library

object HtToString extends Html2String{
  
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
