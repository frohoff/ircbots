package org.frohoff.ircbots

import org.apache.http.impl.client._
import org.apache.http.client.methods._
import org.ccil.cowan.tagsoup.jaxp._
import xml._
import javax.xml.parsers._

class ImdbQuoteProvider(id:String) extends QuoteProvider {
	val client = new DefaultHttpClient
	def quotes = {
		println("fetching from imdb:" + id)
		val response = client.execute(new HttpGet("http://www.imdb.com/title/" + id + "/quotes"))
		val parser = SAXParserFactory.newInstance(classOf[SAXFactoryImpl].getName, getClass.getClassLoader).newSAXParser
		val xml = XML.withSAXParser(parser).load(response.getEntity.getContent)
		val divs = xml \\ "div" filter {d => (d \ "@class").text == "sodatext" }
		def collapse(ns:Seq[Node]):Seq[Node] = {
			ns flatMap {
				_ match {
					case Text(text) if (text.trim.isEmpty) => None
					case <br/> | <i>{_}</i> => None
					case o => Some(o)
				}
			} match {
				case Seq(Text(a), Text(b), rest @ _*) => {
					collapse(Text(a + b) :: rest.toList)
				} case Seq(Text(a), rest @ _ *) if (a.replaceAll("\\W+","").isEmpty) => {
					collapse(rest)
				} case Seq(o, rest @ _*) => {
					o :: collapse(rest).toList
				} case Seq() => {
					Seq()
				}
			}
		}
		def extract(ns:Seq[Node]):List[Quote] = {
			collapse(ns) match {
				case Seq(<b><a>{Text(person)}</a></b>, Text(quote), rest @ _*) => {
					Quote(person, quote.trim.replaceAll("[\\[\\]\\n:]+", ""), 1, 1) :: extract(rest)
				} case Seq(<p>{_}</p>) => {
					Nil
				} case o => {
					println("couldn't match: " + o)
					Nil
				}
			}
		}
		val quotes = divs flatMap { d => extract(d.child).zipWithIndex.map(e => e._1.copy(subId=e._2))}
		println("fetched " + quotes.size + " from imdb:" + id)
		quotes
	}
}

object ImdbQuoteProvider extends Application {
	val prov = new ImdbQuoteProvider("tt0118715")
	println(prov.quotes.map(_.toString).foldLeft("")(_+"\n"+_))
}