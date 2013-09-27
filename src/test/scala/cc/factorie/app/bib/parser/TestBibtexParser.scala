package cc.factorie.app.bib.parser

import org.scalatest.junit.JUnitSuite
import cc.factorie.app.bib.parser.Dom.Name
import org.junit.Test
import java.io.File
import collection.mutable.ArrayBuffer

class TestBibtexParser extends JUnitSuite with cc.factorie.util.FastLogging {

  def testMichaelsStuff(): Unit = {

    val path = """C:\Users\Luke\Downloads\failed\failed\failed"""

    val fileTexts = new File(path).listFiles().toList
      .filter(_.isFile)
      .map(f => (f.getName, scala.io.Source.fromFile(f.getPath, "ISO-8859-1").toArray.mkString))

    val results = fileTexts map {
      case (name, fileText) =>
        Dom.stringToDom(fileText).fold(err =>
          Left("""
Error on file: "%s"
Error text: "%s" """ format (name, err)),
          _ =>
            Right("""
Success on file: "%s" """ format name))
    }

    val (failures, successes) = (new ArrayBuffer[String], new ArrayBuffer[String])
    results.foreach(_.fold(failures.+=, successes.+=))

    val failuresCauseNotBibtex = failures.filter(_.contains("`@' expected"))
    val failuresCauseMismatchedQuote = failures.filter(_.contains("`\"' expected but \u001A found"))
    val failuresCauseBadNames = failures.filter(_.contains("fragment between commas"))

    failuresCauseNotBibtex.foreach(failures.-=)
    failuresCauseMismatchedQuote.foreach(failures.-=)
    failuresCauseBadNames.foreach(failures.-=)

    successes.foreach(logger.debug(_))
    failures.foreach(logger.debug(_))

    logger.debug("Failures cause bad names:")
    failuresCauseBadNames.foreach(logger.debug(_))

    if (!failures.isEmpty)
      sys.error(
        "Failed! Successes: %d Failures %d FailuresCauseNotBibtex: %d FailuresCauseMismatchedQuote: %d FailuresCauseBadNames: %d" format
        (successes.length, failures.length, failuresCauseNotBibtex.length,
          failuresCauseMismatchedQuote.length, failuresCauseBadNames.length))
  }

  @Test def allTests(): Unit = {

    def assertParse[T](parser: DocumentParser.Impl.Parser[T], str: String): DocumentParser.Impl.ParseResult[T] = {
      val result = DocumentParser.Impl.parseAll(parser, str)
      assert(result.successful, result.toString + " " + result.getClass.getName)
      result
    }

    def assertParseAndDocify(parser: DocumentParser.Impl.Parser[List[AST.Entry]], str: String, print: Boolean = false): Unit = {
      val parseResult = assertParse(parser, str)
      assert(parseResult.successful, parseResult)
      val res = Dom.astToDom(AST.Document(parseResult.get))
      if (print) logger.debug(res)
    }

    assertParse(DocumentParser.Impl.braceDelimitedNoOuterLiteral, "{Something Great}")
    assertParse(DocumentParser.Impl.literal, "{Something Great}")
    assertParse(DocumentParser.Impl.literalOrSymbol, "{Something Great}")
    assertParse(DocumentParser.Impl.value, "{Something Great}")

    assertParse(DocumentParser.Impl.quoteDelimitedLiteral, "\"Something Great\"")
    assertParse(DocumentParser.Impl.literal, "\"Something Great\"")
    assertParse(DocumentParser.Impl.literalOrSymbol, "\"Something Great\"")
    assertParse(DocumentParser.Impl.value, "\"Something Great\"")

    assertParse(DocumentParser.Impl.numericLiteral, "123")
    assertParse(DocumentParser.Impl.literal, "123")
    assertParse(DocumentParser.Impl.literalOrSymbol, "123")
    assertParse(DocumentParser.Impl.value, "123")

    assertParse(DocumentParser.Impl.SYMBOL, "asda5")
    assertParse(DocumentParser.Impl.literalOrSymbol, "asda5")
    assertParse(DocumentParser.Impl.value, "asda5")

    assertParse(DocumentParser.Impl.tag, "asda5 = { 132 as qwe  asd }")

    assertParse(DocumentParser.Impl.value, "asda5 # asda5")

    assertParse(DocumentParser.Impl.commentEntry, "comment{wooooo!}")

    assertParse(DocumentParser.Impl.preambleEntry, "preamble{wooooo}")

    assertParse(DocumentParser.Impl.stringEntry, "string{wooooo = 1231}")
    assertParse(DocumentParser.Impl.anyEntry, "@string{wooooo = 1231}")
    assertParse(DocumentParser.Impl.anyEntry, "@string{  wooooo  = {asd} }")

    assertParse(DocumentParser.Impl.anyEntry, "@string{  wooooo  = {asd} }")
    assertParse(DocumentParser.Impl.anyEntry, "@preamble{  wooooo}")
    assertParse(DocumentParser.Impl.anyEntry, "@comment{  wooooo }")

    assertParse(DocumentParser.Impl.anyEntry, "@florb{  wooooo }")
    assertParse(DocumentParser.Impl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123 }")
    assertParse(DocumentParser.Impl.anyEntry, "@florb{  wooooo, x = {y}, fg = sdf13, z = 123, }")
    assertParse(DocumentParser.Impl.anyEntry, "@florb{  wooooo, x = {y}, fg =\"sdf13\", z = 123, }")
    assertParse(DocumentParser.Impl.anyEntry,
      """@florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }""")

    assertParse(DocumentParser.Impl.freeComment, "i am the king of the owrld!!")
    assertParse(DocumentParser.Impl.freeComment, """i am the king of the

      owrld!!""")

    assertParse(DocumentParser.Impl.WS ~> DocumentParser.Impl.anyEntry,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }""")

    assertParse((DocumentParser.Impl.WS ~> DocumentParser.Impl.anyEntry).+,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }""")

    assertParse(DocumentParser.Impl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }""")

    assertParse(DocumentParser.Impl.bibTex,
      """ @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

      """
    )

    assertParse(DocumentParser.Impl.bibTex,
      """
       Hi, everybody!

       @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }

 free comments are coool
 @florb{  wooooo,
        x = {y},
        fg ="sdf13",
        z = 123 #  asd,
      }


      """)


    assertParse(DocumentParser.Impl.bibTex,
      """
          @article {mrx05,
          auTHor = "Mr. X",
          Title = {Something Great},
          publisher = "nob" # "ody",
          YEAR = 2005
          }
      """)

    assertParse(
      DocumentParser.Impl.braceDelimitedNoOuterLiteral,
      "{Interannual Variability of planet-encircling dust activity on {M}ars}")

    // this sample is from: http://amath.colorado.edu/documentation/LaTeX/reference/faq/bibstyles.html
    val coloradoSample = assertParse(DocumentParser.Impl.bibTex,
      """

@string{jgr = "J.~Geophys.~Res."}

@MISC{primes,
   author = "Charles Louis Xavier Joseph de la Vall{\'e}e Poussin",
   note = "A strong form of the prime number theorem, 19th century",
   year = 1879
   }

@INBOOK{chicago,
   title = "The Chicago Manual of Style",
   publisher = "University of Chicago Press",
   edition = "Thirteenth",
   year = 1982,
   pages = "400--401",
   key = "Chicago"
   }

@BOOK{texbook,
   author = "Donald E. Knuth",
   title= "The {{\TeX}book}",
   publisher = "Addison-Wesley",
   year = 1984
   }

@BOOK{latexbook,
   author = "Leslie Lamport",
   title = "{\LaTeX \rm:} {A} Document Preparation System",
   publisher = "Addison-Wesley",
   year = 1986
   }

@UNPUBLISHED{btxdoc,
   author = "Oren Patashnik",
   title = "{Using BibTeX}",
   note = "Documentation for general BibTeX users",
   month = jan,
   year = 1988
   }

@UNPUBLISHED{btxhak,
   author = "Oren Patashnik",
   title = "Designing BibTeX Styles",
   note = "The part of BibTeX's documentation
                            that's not meant for general users",
   month = jan,
   year = 1988
   }

@BOOK{strunk,
   author = "Strunk, Jr., William and E. B. White",
   title = "The Elements of Style",
   publisher = "Macmillan",
   edition = "Third",
   year = 1979
   }

@book{vanleunen,
   title = "A Handbook for Scholars",
   author = "Mary-Claire van Leunen",
   publisher = "Knopf",
   year = "1979"
   }

@ARTICLE{Zurek:1993,
   AUTHOR  = {Zurek, R. W. and Martin, L. J.},
   TITLE   = {Interannual Variability of planet-encircling dust activity on {M}ars},
   YEAR    = {1993},
   JOURNAL = jgr,
   VOLUME  = {98},
   NUMBER  = {E2},
   PAGES   = {3247--3259}
}

@Article{Narendra_1990,
  author =       {K.S.Narendra and K.S.Parthsarathy},
  title =        {Identification and Control of Dynamical System
                  using Neural Networks},
  journal =      "IEENN",
  year =         {1990},
  volume =    {1},
  number =    {1},
  month =     {},
  pages =     {4-27},
  note =      {},
  annote =    {}
}


      """)

    assert(coloradoSample.successful, coloradoSample)
    Dom.astToDom(AST.Document(coloradoSample.get))

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
    @InProceedings{dredze-EtAl:2007:EMNLP-CoNLL2007,
      author    = {Dredze, Mark  and  Blitzer, John  and  Pratim Talukdar, Partha  and  Ganchev, Kuzman  and  Graca, Jo\~ao  and  Pereira, Fernando},
      title     = {Frustratingly Hard Domain Adaptation for Dependency Parsing},
      booktitle = {Proceedings of the CoNLL Shared Task Session of EMNLP-CoNLL 2007}
      pages     = {1051--1055},
      url       = {http://www.aclweb.org/anthology/D/D07/D07-1112}
    }
    """)

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
    @InProceedings{BanikACL09-shortpaper,
      author =       {Eva Banik},
      title =        {Extending a Surface Realizer to Generate Coherent Discourse},
      booktitle =    {Proceedings of the Short Papers of the Joint conference of the Association for Computational Linguistics and the Asian Federation of Natural Language Processing (ACL-IJCNLP-09), Singapore},
      year =         2009
    }

    @inproceedings{webdb03-smwea,
            title={{ODISSEA: A Peer-to-Peer Architecture for Scalable Web Search and Information Retrieval}},
            author={T. Suel and C. Mathur and J. Wu and J. Zhang and A. Delis
                    and M. Kharrazi and X. Long and K. Shanmugasunderam},
            booktitle={{6th International Workshop on the Web and Databases (WebDB)}},
            month={June},
            year={2003},
            address={San Diego, CA}
            }

    @inproceedings{1333582,
     author = {Donglai Zhang and Paul Coddington and Andrew Wendelborn},
     title = {Binary Data Transfer Performance over High-Latency Networks Using Web Service Attachments},
     booktitle = {E-SCIENCE '07: Proceedings of the Third IEEE International Conference on e-Science and Grid Computing},
     year = {2007},
     isbn = {0-7695-3064-8},
     pages = {261--269},
     doi = {http://dx.doi.org/10.1109/E-SCIENCE.2007.16},
     publisher = {IEEE Computer Society}


    }
        """)

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
@inproceedings{nahm:icml-wkshp02,
  author =       {Un Yong Nahm and Mikhail Bilenko and Raymond J.
                 Mooney},
  title =        {Two Approaches to Handling Noisy Variation in Text
                 Mining},
  booktitle =    {Proceedings of the ICML-2002 Workshop on
                 Text Learning},
  pages =        {18--27},
  year =         2002,
}
    }
        """)

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
@article{1814808,
 author = {Kauppinen, Tomi and Mantegari, Glauco and Paakkarinen, Panu and Kuittinen, Heini and Hyv\"{o}nen, Eero and Bandini, Stefania},
 title = {Determining relevance of imprecise temporal intervals for cultural heritage information retrieval},
 journal = {Int. J. Hum.-Comput. Stud.},
 volume = {68},
 number = {9},
 year = {2010},
 issn = {1071-5819},
 pages = {549--560},
 doi = {http://dx.doi.org/10.1016/j.ijhcs.2010.03.002},
 publisher = {Academic Press, Inc.},
 address = {Duluth, MN, USA},
 }""")

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
@inproceedings{sen07:coordinating,
	Author = {Sen, Rohan and Hackmann, Gregory and Haitjema, Mart and Roman, Gruia-Catalin and Gill, Christopher},
	Booktitle = {Lecture Notes in Computer Science},
	Pages = {249--267},
	Title = {Coordinating Workflow Allocation and Execution in Mobile Environments},
	Url = {http://dx.doi.org/10.1007/978-3-540-72794-1_14},
	Volume = {4467}
	Year = {2007}
}""")

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
    @COMMENT This file was generated by bib2html.pl <https://sourceforge.net/projects/bib2html/> version 0.94
    @COMMENT written by Patrick Riley <http://sourceforge.net/users/patstg/>
    @COMMENT This file came from Evgeniy Gabrilovich's publication pages at
    @COMMENT http://www.gabrilovich.com/pubs.html
    @Proceedings{Bunescu:2008:WikiAI,
      title        = "Proceedings of the AAAI Workshop on Wikipedia and Artificial Intelligence: An Evolving Synergy (WikiAI)",
      year         = 2008,
      editor       = "Razvan Bunescu and Evgeniy Gabrilovich and Rada Mihalcea",
      month        = "July",
      organization = "Association for the Advancement of Artificial Intelligence",
      publisher    = "{AAAI} Press",
      note         = "AAAI Technical Report WS-08-15"
    }
    """, print = true)

    assertParseAndDocify(DocumentParser.Impl.bibTex, """
    @article{acmalg295,
        author="H. Sp\"{a}th",
        title="Exponential Curve Fit",
        volume=10,
        number=2,
        pages="87",
        year=1967,
        month="February",
        journal=cacm
    }
    """, print = true)

    expectResult(NameParser.stringToNames("Graca, Jo\\~ao"))(List(Name("Jo\\~ao", "", "Graca", "")))

    expectResult(NameParser.stringToNames("Ludwig von Beethoven"))(List(Name("Ludwig", "von", "Beethoven", "")))
    expectResult(NameParser.stringToNames("von Beethoven, Ludwig"))(List(Name("Ludwig", "von", "Beethoven", "")))
    expectResult(NameParser.stringToNames("Jones, Jr., John-Paul"))(List(Name("John Paul", "", "Jones", "Jr.")))
    expectResult(NameParser.stringToNames("John Paul Jones"))(List(Name("John Paul", "", "Jones", "")))

    expectResult(NameParser.stringToNames("John Paul Jones and Jones, John Paul"))(
      List(Name("John Paul", "", "Jones", ""), Name("John Paul", "", "Jones", "")))
    expectResult(NameParser.stringToNames("John Paul Jones and Ludwig von Beethoven"))(
      List(Name("John Paul", "", "Jones", ""), Name("Ludwig", "von", "Beethoven", "")))

    expectResult(NameParser.stringToNames("Charles Louis Xavier Joseph de la Vallee Poussin"))(
      List(Name("Charles Louis Xavier Joseph", "de la", "Vallee Poussin", "")))

    expectResult(NameParser.stringToNames("{Barnes} {and} {Noble} {Inc.}"))(List(Name("Barnes", "and", "Noble Inc.", "")))

    expectResult(NameParser.stringToNames("Ralph Alpher and Bethe, Hans and George Gamow"))(
      List(Name("Ralph", "", "Alpher", ""), Name("Hans", "", "Bethe", ""), Name("George", "", "Gamow", "")))
    expectResult(NameParser.stringToNames("K.S.Narendra"))(List(Name("K. S.", "", "Narendra", "")))

    expectResult(NameParser.stringToNames("{\\e'}cole"))(List(Name("", "", "{\\e'}cole", "")))

    expectResult(NameParser.stringToNames("John-Paul Jones and Bill Thompson"))(
      List(Name("John Paul", "", "Jones", ""), Name("Bill", "", "Thompson", "")))

    expectResult(NameParser.stringToNames("{\\e'}col{\\e'}"))(List(Name("", "", "{\\e'}col{\\e'}", "")))

    expectResult(NameParser.stringToNames("{hey ho lotsa stu\\}ff}"))(List(Name("", "", "hey ho lotsa stu\\}ff", "")))
    expectResult(NameParser.stringToNames("{Jean} {de la Fontaine du} {Bois Joli}"))(List(Name("Jean", "de la Fontaine du", "Bois Joli", "")))
    expectResult(NameParser.stringToNames("Jean de la Fontaine du Bois Joli"))(List(Name("Jean", "de la Fontaine du", "Bois Joli", "")))

    val clx1 = NameParser.stringToNames("Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin").head
    expectResult(clx1)(Name("Charles Louis Xavier Joseph", "de la", "Vall{\\'e}e Poussin", ""))
    val clx2 = Dom.stringToDom("@thing{asdf, author = \"Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin\"}")
      .right.get.entries.head._2.authors.get.head
    expectResult(clx2)(Name("Charles Louis Xavier Joseph", "de la", "Vall{\\'e}e Poussin", ""))
    val clx3 = Dom.stringToDom("@thing{asdf, author = {Charles Louis Xavier Joseph de la Vall{\\'e}e Poussin}}")
      .right.get.entries.head._2.authors.get.head
    expectResult(clx3)(Name("Charles Louis Xavier Joseph", "de la", "Vall{\\'e}e Poussin", ""))

    assert(clx1 == clx2 && clx2 == clx3, (clx1, clx2, clx3))

    val ksn1 = NameParser.stringToNames("K.S.Narendra").head
    expectResult(ksn1)(Name("K. S.", "", "Narendra", ""))
    val ksn2 = Dom.stringToDom("@thing{asdf, author = \"K.S.Narendra\"}")
      .right.get.entries.head._2.authors.get.head
    expectResult(ksn2)(Name("K. S.", "", "Narendra", ""))
    val ksn3 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra}}")
      .right.get.entries.head._2.authors.get.head
    expectResult(ksn3)(Name("K. S.", "", "Narendra", ""))
    val ksn4 = Dom.stringToDom("@thing{asdf, author = {K.S.Narendra and Hugh Jass}}")
      .right.get.entries.head._2.authors.get.head
    expectResult(ksn4)(Name("K. S.", "", "Narendra", ""))

    assert(ksn1 == ksn2 && ksn2 == ksn3 && ksn3 == ksn4, (ksn1, ksn2, ksn3, ksn4))

    if (false) {

      // didn't check in files for testing since they're pretty big - if interested, go to BibNet or I can provide

      val fileText = scala.io.Source.fromFile("inputs/case-based-reasoning.bib.txt").mkString
      val res = Dom.stringToDom(fileText, false)
      //println(res)

      def timed[T](showTime: Long => String)(body: => T) = {
        val start = System.currentTimeMillis
        val result = body
        val time = showTime(System.currentTimeMillis - start)
        logger.debug(time)
        (result, time)
      }

      val filePath2 = "inputs/domain-decomp.bib.txt"
      val file2 = scala.io.Source.fromFile(filePath2).toArray
      val fileText2 = file2.mkString

      val numLines = file2.length
      val numMb = new java.io.File(filePath2).length / 1024.0 / 1024.0

      val (result, time) =
        timed(t =>
          "domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
          (numMb, numLines, t, (1000.0 * numMb) / t, (1000.0 * numLines) / t)) {
          Dom.stringToDom(fileText2, false)
        }

      //    println(result)
      logger.debug(time)
      val sizeMult = 10
      val bigtext = List.range(0, sizeMult).map(_ => fileText2).mkString
      val (bigresult, bigtime) =
        timed(t =>
          "%d times domain-decomp.bib (%f MB, %d lines) parsed and dom-ified in %d ms (%f MB/sec, %f lines/sec)" format
          (sizeMult, numMb * sizeMult, numLines * sizeMult, t, (1000.0 * numMb * sizeMult) / t, (1000.0 * numLines * sizeMult) / t)) {
          Dom.stringToDom(bigtext, false)
        }
    }
  }
}
