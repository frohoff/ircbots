package org.frohoff.ircbots

import twitter4j._
import collection.JavaConversions._

class TwitterQuoteProvider(account:String) extends QuoteProvider {
	val twitter = new TwitterFactory().getInstance
	def quotes = try {
		println("fetching from twitter:" + account)
		val timeline = Stream.from(1)
			.map(i => retry(3)(twitter.getUserTimeline(account, new Paging(i, 200))))
			.takeWhile(_.size > 1)
			.flatten
		println("fetched " + timeline.size + " from twitter:" + account)
		timeline.zipWithIndex.map(e => Quote(account, e._1.getText, e._2, 1))
	} catch { case e => e.printStackTrace(); Seq() }

	private def retry[T](times:Int)(code: => T):T = {
		try {
			code
		} catch {
			case e => {
				if (times > 0) {
					println(e + ": retrying " + times + "more times")
					retry(times - 1) {
						code
					}
				} else
					throw e
			}
		}
	}
}