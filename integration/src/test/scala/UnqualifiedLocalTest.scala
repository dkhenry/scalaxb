import java.io.{File}
import scalaxb.compiler.{Config}

object UnqualifiedLocalTest extends TestBase {
  val inFile  = new File("integration/src/test/resources/unqualified.xsd")
  lazy val generated = module.process(inFile, "unqualified", tmp)

  "unqualified.scala file must compile so that Foo can be used" in {
    (List("""scalaxb.toXML[unqualified.Foo](scalaxb.fromXML[unqualified.Foo](""" +
    """<unq:foo xmlns:unq="http://www.example.com/unqualified" attribute1="bar">""" +
    "<string1></string1>" +
    """</unq:foo>), """ +
    """Some("http://www.example.com/unqualified"), "foo", """ +
    """scalaxb.toScope(Some("unq") -> "http://www.example.com/unqualified") ).toString"""),
     generated) must evaluateTo("""<unq:foo attribute1="bar" xmlns:unq="http://www.example.com/unqualified">""" +
    "<string1></string1>" +
    "</unq:foo>", outdir = "./tmp")
  }

  "unqualified.scala file must compiled with an alternative toXML" in {
    (List("""scalaxb.toXML[unqualified.Foo](scalaxb.fromXML[unqualified.Foo](""" +
    """<unq:foo xmlns:unq="http://www.example.com/unqualified" attribute1="bar">""" +
    "<string1></string1>" +
    """</unq:foo>), """ +
    """Some("http://www.example.com/unqualified"), Some("foo"), """ +
    """scalaxb.toScope(Some("unq") -> "http://www.example.com/unqualified") ).toString"""),
     generated) must evaluateTo("""<unq:foo attribute1="bar" xmlns:unq="http://www.example.com/unqualified">""" +
    "<string1></string1>" +
    "</unq:foo>", outdir = "./tmp")
  }

  "unqualified.scala file must compile so that Foo can be used without toplevel prefix" in {
    (List("""scalaxb.toXML[unqualified.Foo](scalaxb.fromXML[unqualified.Foo](""" +
    """<unq:foo xmlns:unq="http://www.example.com/unqualified" attribute1="bar">""" +
    "<string1></string1>" +
    """</unq:foo>), "foo", """ +
    """scalaxb.toScope(Some("unq") -> "http://www.example.com/unqualified") ).toString"""),
     generated) must evaluateTo("""<foo attribute1="bar" xmlns:unq="http://www.example.com/unqualified">""" +
    "<string1></string1>" +
    "</foo>", outdir = "./tmp")
  }

  /*
  val inFile2  = new File("integration/src/test/resources/qualified.xsd")
  lazy val generated2 = (new Driver).process(inFile2, "qualified", tmp)

  "qualified.scala file must compile so that Foo can be used" in {
    (List("""scalaxb.toXML[qualified.Foo](scalaxb.fromXML[qualified.Foo](""" +
    """<q:foo xmlns:q="http://www.example.com/qualified" q:attribute1="bar">""" +
    "<q:string1></q:string1>" +
    """</q:foo>), """ +
    """Some("http://www.example.com/qualified"), "foo", """ +
    """scalaxb.toScope(Some("q") -> "http://www.example.com/qualified") ).toString"""),
     generated2) must evaluateTo("""<q:foo q:attribute1="bar" xmlns:q="http://www.example.com/qualified">""" +
    "<q:string1></q:string1>" +
    "</q:foo>", outdir = "./tmp")

  }
  */
}
