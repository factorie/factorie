package cc.factorie.app.nlp.embeddings
import cc.factorie.la.DenseTensor1
import java.io._
import java.util.zip.{GZIPInputStream, GZIPOutputStream}


object GoogleEmbedding {
           def loadVocabAndVectors(googleEmbeddingFile: String, takeTopN: Int = -1, encoding: String = "ISO-8859-15") = {
             val in = googleEmbeddingFile.endsWith(".gz") match { 
               case true =>  new DataInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(googleEmbeddingFile))))
               case false => new DataInputStream(new BufferedInputStream(new FileInputStream(googleEmbeddingFile)))
             }
             val V = in.readInt()
             var D = in.readInt()
             D = if (takeTopN == -1 || D < takeTopN) D else takeTopN
             println("Vocab Size : ${V} dim : ${D}")
             
             var i = 0; var j = 0;
             var arr =  new Array[DenseTensor1](V)
             val vocab = new Array[String](V)
             for (v <- 0 until V) { 
                   vocab(v) = in.readUTF()
                   arr(v) = new DenseTensor1(D, 0)
                   for (d <- 0 until D)
                      arr(v)(d) = in.readDouble()
             }
             (vocab, arr)    
        }
        def storeInPlainText(googleEmbeddingFile: String, takeTopN: Int = -1, encoding: String = "ISO-8859-15"): Unit = {
            val (vocab, arr) = loadVocabAndVectors(googleEmbeddingFile, takeTopN)
            val V = arr.size
            assert(V == vocab.size) // should match
            val D = arr(0).dim1
            val out = new PrintWriter(googleEmbeddingFile + ".txt", encoding)
            out.write("${V} ${D}\n")
            for (v <- 0 until V) {
               out.write(vocab(v) + " " ); out.flush(); 
               for (d <- 0 until D) {
                 out.write(arr(v)(d) + " "); out.flush();
               }
               out.write("\n"); out.flush();
            }
            out.close()
            println("Storing Storing")
            
            
        }
        def storeInGZip(googleEmbeddingFile: String, takeTopN: Int = -1, encoding: String = "ISO-8859-15"): Unit =  {
          val (vocab, arr) = loadVocabAndVectors(googleEmbeddingFile, takeTopN)
            val V = arr.size
            assert(V == vocab.size) // should match
            val D = arr(0).dim1
            val outFileName = if (!googleEmbeddingFile.endsWith(".gz")) googleEmbeddingFile+".gz" else googleEmbeddingFile // making sure of not adding .gz.gz extention
            val out = new OutputStreamWriter(new GZIPOutputStream(new BufferedOutputStream(new FileOutputStream(outFileName))), encoding)
            out.write("${V} ${D}\n")
            for (v <- 0 until V) {
               out.write(vocab(v) + " " ); out.flush(); 
               for (d <- 0 until D) {
                 out.write(arr(v)(d) + " "); out.flush();
               }
               out.write("\n"); out.flush();
            }
          out.close()
          println("Done Storing")
        }
        
}
