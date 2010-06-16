package org.frohoff.ircbots

trait QuoteProvider {
	def quotes:Seq[Quote]
	case class Quote(person:String, content:String, id:Long, subId:Long = 0)
}