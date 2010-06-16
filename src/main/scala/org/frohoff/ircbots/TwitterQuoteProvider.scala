package org.frohoff.ircbots

import twitter4j._
import collection.JavaConversions._

class TwitterQuoteProvider(account:String) extends QuoteProvider {
	val twitter = new TwitterFactory().getInstance
	def quotes = {
		val timeline = Stream.from(1)
			.map(i => twitter.getUserTimeline(account, new Paging(i, 200)))
			.takeWhile(_.size > 1)
			.flatten
		println("fetched " + timeline.size + " from twitter:" + account)
		timeline.zipWithIndex.map(e => Quote(account, e._1.getText, e._2, 1))
	}
}