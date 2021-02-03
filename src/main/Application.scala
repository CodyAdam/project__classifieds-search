package main;

import library.ExpressionParser.readExp;
import java.io.FileWriter;
import java.io.File;
import java.awt.Desktop;
import library._;

object Application extends App { // Romain
  // Récupère l'expression de recherche
  val exp = readExp;
  println("\nProcessing your expression ...");
  
  // récupère les mots clés et génère les urls de recherches
  val listKeyWords = RequestVivastreet.getKeyWordsExpression(exp);
  val listRequests = RequestVivastreet.getRequestsList(listKeyWords);

  // Fait les recherches, récupère les résultats sous forme d'objet HTMl et le convertit en String
  println("Searching results ...");
  val listResults = RequestVivastreet.getResults(listRequests, exp);
  val html = MonProductionResultat.resultat2html(listResults);
  val htmlString = MonHtml2String.process(html); 
  
  // Ecris l'HTML sous forme de chaine de caractères dans un fichier
  println("Writing results ...");
  val file = new FileWriter("result.html");
  try {
    file.write(htmlString);
  } finally file.close()
  
  // Ouvre le fichier HTML généré
  println("Showing results ...");
  val desktop = Desktop.getDesktop();
  desktop.open(new File("result.html"));
  
  println("Done ✅");
}

private object RequestVivastreet {
  /**
   * Récupère une liste de mots clés à partir de l'expression
   *
   * @param exp une Expression 
   * @return une liste de String qui représente les tags à rechercher sur vivastreet suivant les expressions exp 
   */
  def getKeyWordsExpression(exp: Expression): List[String] = {
    exp match {
      case Word(w)     => List(w);
      case And(e1, e2) => getKeyWordsExpression(e1) ++ getKeyWordsExpression(e2);
      case Or(e1, e2)  => getKeyWordsExpression(e1) ++ getKeyWordsExpression(e2);
    }
  }

  /**
   * Récupère une liste des urls de recherches à partir d'une liste de mots clés
   * 
   * @param keyWords une liste de mots clés
   * @return une liste des urls de recherches vivastreet
   */
  def getRequestsList(keyWords: List[String]): List[String] = {
    keyWords match {
      case Nil           => Nil;
      case first :: rest => List("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=" + first + "&cat_1=&geosearch_text=&searchGeoId=0") ++ getRequestsList(rest);
    }
  }

  /**
   * Récupère une liste qui associe un titre d'annonce à son lien
   * 
   * @param listRequests liste des urls de recherches
   * @param exp une expression de recherche
   * @return une liste qui associe un titre d'annonce à son lien
   */
  def getResults(listRequests: List[String], exp: Expression): List[(String, String)] = {
    listRequests match {
      case Nil           => Nil;
      case first :: rest => MonAnalysePage.resultats(first, exp) ++ getResults(rest, exp);
    }
  }
}
