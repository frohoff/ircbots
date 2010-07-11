package org.frohoff.ircbots

import twitter4j._
import collection.JavaConversions._
import org.apache.lucene.analysis.standard._
import org.apache.lucene.document._
import org.apache.lucene.index._
import org.apache.lucene.queryParser._
import org.apache.lucene.search._
import org.apache.lucene.store._
import org.apache.lucene.util.{Version => Ver}

object App extends Application {
	println("getting quotes")

	var quotes =
		new TwitterQuoteProvider("shitmydadsays").quotes.map(x => x.copy(person="Dad", content=x.content.replaceAll(".*[\"“](.*)[\"”].*","$1"))) ++
		new ImdbQuoteProvider("tt0118715").quotes ++ (
			new TwitterQuoteProvider("tweetnorris").quotes.map(x => x.copy(content=x.content.replaceAll(".*[\"“](.*)[\"”].*http://.*","$1"))) ++
			new TwitterQuoteProvider("FAKECHUCKNORRIS").quotes ++
			new TwitterQuoteProvider("chuckfacts").quotes ++
			new TwitterQuoteProvider("chucknorris_").quotes ++
			new TwitterQuoteProvider("chuck_facts").quotes ++
			new TwitterQuoteProvider("chuckthefacts").quotes
				.map(x => x.copy(content=x.content.replaceAll("(?i).*fact:\\s*(.*)","$1"))))
			.flatMap(x => if(x.content.contains("@") || x.content.contains("...")) None else Some(x))
			.map(_.copy(person="Chuck Norris"))

	val dir = new RAMDirectory()
	val writer = new IndexWriter(dir,
		new StandardAnalyzer(Ver.LUCENE_30),
		IndexWriter.MaxFieldLength.LIMITED);

	quotes.foreach{ x =>
		writer.addDocument({val d = new Document()
			d.add(new Field("id", "" + x.id , Field.Store.YES, Field.Index.NOT_ANALYZED))
			d.add(new Field("subId", "" + x.subId , Field.Store.YES, Field.Index.NOT_ANALYZED))
			d.add(new Field("person", x.person  , Field.Store.YES, Field.Index.ANALYZED_NO_NORMS))
			d.add(new Field("content", x.content , Field.Store.YES, Field.Index.ANALYZED_NO_NORMS))
			d })
	}
	writer.commit();

    val searcher = new IndexSearcher(dir);

	writer.optimize();
    writer.close();

    print(">")
	Stream.continually(Console.readLine).foreach(line => {
		val parser = new QueryParser(Ver.LUCENE_30,
              "content",
              new StandardAnalyzer(Ver.LUCENE_30))
		val query = parser.parse(line)
		val hitsPerPage = 10;
      // Search for the query
      val collector = TopScoreDocCollector.create(5 * hitsPerPage, false);
      searcher.search(query, collector);
      val hits = collector.topDocs().scoreDocs;
      val hitCount = collector.getTotalHits();
      if (hitCount > 0) {
    	  hits.foreach(x => {
    	 	  println(x.score + " ; " + searcher.doc(x.doc).get("person") + " : " + searcher.doc(x.doc).get("content"))
    	  })
      } else
    	  println("<no hits>")
		print(">")
	})
}
