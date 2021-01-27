package main

import library.ExpressionParser.readExp;
import java.io.FileWriter;

object Application extends App{
  val exp = readExp;

  
  
  val file = new FileWriter("results.html")
  try {
    file.write(htmlFinalResult)
  } finally
    file.close()
}