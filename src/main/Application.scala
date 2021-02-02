package main;

import library.ExpressionParser.readExp;
import java.io.FileWriter;
import java.io.File;
import java.awt.Desktop;
import library._;

object Application extends App { // Romain
  val exp = readExp;
  
  println("\nProcessing your expression ...");
  val listKeyWords = RequestVivastreet.getKeyWordsExpression(exp);
  val listRequests = RequestVivastreet.getRequestsList(listKeyWords);

  println("Searching results ...");
  val listResults = RequestVivastreet.getResults(listRequests, exp);
  val html = MonProductionResultat.resultat2html(listResults);
  val htmlString = MonHtml2String.process(html); 
  
  println("Writing results ...");
  val file = new FileWriter("result.html");
  try {
    file.write(htmlString);
  } finally file.close()
  
  println("Showing results ...");
  val desktop = Desktop.getDesktop();
  desktop.open(new File("result.html"));
  
  println("Done ✅");
}

private object RequestVivastreet {
  def getKeyWordsExpression(exp: Expression): List[String] = {
    exp match {
      case Word(w)     => List(w);
      case And(e1, e2) => getKeyWordsExpression(e1) ++ getKeyWordsExpression(e2);
      case Or(e1, e2)  => getKeyWordsExpression(e1) ++ getKeyWordsExpression(e2);
    }
  }

  def getRequestsList(keyWords: List[String]): List[String] = {
    keyWords match {
      case Nil           => Nil;
      case first :: rest => List("https://search.vivastreet.com/annonces/fr?lb=new&search=1&start_field=1&keywords=" + first + "&cat_1=&geosearch_text=&searchGeoId=0") ++ getRequestsList(rest);
    }
  }

  def getResults(listRequests: List[String], exp: Expression): List[(String, String)] = {
    listRequests match {
      case Nil           => Nil;
      case first :: rest => MonAnalysePage.resultats(first, exp) ++ getResults(rest, exp);
    }
  }
}
