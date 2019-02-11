
import org.w3.banana.jena.JenaModule
import scala.util._
import java.net.URL
import org.w3.banana._, io._, binder._, jena._
import scala.concurrent._
import scala.util.{Try, Success, Failure}


class Triples[A,B,C] (triples:List[(A,B,C)]) {

  def groupBySubj () : Map[A,Map[B,C]] = {
    val m = triples.groupBy(_._1)
    m map {case (key,value) => (key, (value map ( (x)=>(x._2,x._3) )).toMap)}
    
  }

  def groupByPred () : Map[B,Map[A,C]] = {
    val m = triples.groupBy(_._2)
    m map {case (key,value) => (key, (value map ( (x)=>(x._1,x._3) )).toMap)}
  }
}


trait SPARQLExampleDependencies
  extends RDFModule
  with RDFOpsModule
  with SparqlOpsModule
  with SparqlHttpModule

trait SPARQLExample extends SPARQLExampleDependencies { self =>

  import ops._
  import sparqlOps._
  import sparqlHttp.sparqlEngineSyntax._


  def toTriples [a,b,c] ( it :Iterator[Rdf#Solution] )  =  {
    (it map { row =>
      (row("s").get.toString(), "def", row("def").get.toString().substring(0, row("def").get.toString().indexOf("^") ))
    }).toList
  }

def main(args: Array[String]): Unit = {

    /* gets a SparqlEngine out of a Sparql endpoint */
    val endpoint = new URL("http://dbcells.org:3030/dbcells")


    val query = parseSelect("""
        PREFIX wgs84_pos: <http://www.w3.org/2003/01/geo/wgs84_pos#>
        PREFIX geo: <http://www.opengis.net/ont/geosparql#> 
            SELECT ?s ?def ?pol
             WHERE {
                ?s geo:asWKT ?pol.
                ?s <http://dbcells.org/ontology/deforest2008> ?def.
                ?s ?p ?o.
                ?s geo:sfWithin ?o2.
                ?o2 geo:sfWithin "BRA".
            }
        LIMIT 300000
    """).get

    val rs: Rdf#Solutions = endpoint.executeSelect(query).get

   val it: Iterator[Rdf#Solution] = rs.iterator()

    val cells = toTriples (rs.iterator())
    
    val triples = new Triples (cells)
    println (triples.groupBySubj().keys)
    val mapCells = triples.groupBySubj()
    println (mapCells("http://dbcells.org/onehalf/133640")("def"))
    
   val mapDef = triples.groupByPred()("def")
    print(mapDef)
    

  }
}
import org.w3.banana.jena.JenaModule
object SPARQLExampleWithJena extends SPARQLExample with JenaModule


