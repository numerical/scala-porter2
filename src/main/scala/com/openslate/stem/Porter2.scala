package com.openslate.stem

import math.max


object Porter2 {
  private val vowels = Set('a', 'e', 'i', 'o', 'u', 'y')
  private val doubles = Set("bb", "dd", "ff", "gg", "mm", "nn", "pp", "rr", "tt")
  private val li_endings = Set('c', 'd', 'e', 'g', 'h', 'k', 'm', 'n', 'r', 't')
  // private val secondary1b = Set("at", "bl", "iz")

  private val step0_suffs = List("'s'", "'s", "'")
  private val step1a_suffs = List("sses", "ied", "ies", "ss", "us", "s")
  private val step2_suffs = Map(
    "tional" -> "tion",
    "enci" -> "ence",
    "anci" -> "ance",
    "abli" -> "able",
    "entli" -> "ent",
    "izer" -> "ize",
    "ization" -> "ize",
    "ational" -> "ate",
    "ation" -> "ate",
    "ator" -> "ate",
    "alism" -> "al",
    "aliti" -> "al",
    "alli" -> "al",
    "fulness" -> "ful",
    "ousli" -> "ous",
    "iveness" -> "ive",
    "iviti" -> "ive",
    "biliti" -> "ble",
    "bli" -> "ble",
    "ogi" -> "og",
    "fulli" -> "ful",
    "lessli" -> "less",
    "li" -> "")
  private val step3_suffs = Map(
    "tional" -> "tion",
    "ational" -> "ate",
    "alize" -> "al",
    "icate" -> "ic",
    "iciti" -> "ic",
    "ical" -> "ic",
    "ful" -> "",
    "ness" -> "",
    "ative" -> "")
  private val step4_suffs = Map(
    "al" -> "",
    "ance" -> "",
    "ence" -> "",
    "er" -> "",
    "ic" -> "",
    "able" -> "",
    "ible" -> "",
    "ant" -> "",
    "ement" -> "",
    "ment" -> "",
    "ent" -> "",
    "ism" -> "",
    "ate" -> "",
    "iti" -> "",
    "ous" -> "",
    "ive" -> "",
    "ize" -> "",
    "ion" -> "")
  private val special_words = Map(
    "skis" -> "ski",
    "skies" -> "sky",
    "dying" -> "die",
    "lying" -> "lie",
    "tying" -> "tie",
    "idly" -> "idl",
    "gently" -> "gentl",
    "ugly" -> "ugli",
    "early" -> "earli",
    "only" -> "onli",
    "singly" -> "singl",
    "sky" -> "sky",
    "news" -> "news",
    "howe" -> "howe",
    "atlas" -> "atlas",
    "cosmos" -> "cosmos",
    "bias" -> "bias",
    "andes" -> "andes",
    "inning" -> "inning",
    "innings" -> "inning",
    "outing" -> "outing",
    "outings" -> "outing",
    "canning" -> "canning",
    "cannings" -> "canning",
    "herring" -> "herring",
    "herrings" -> "herring",
    "earring" -> "earring",
    "earrings" -> "earring",
    "proceed" -> "proceed",
    "proceeds" -> "proceed",
    "proceeded" -> "proceed",
    "proceeding" -> "proceed",
    "exceed" -> "exceed",
    "exceeds" -> "exceed",
    "exceeded" -> "exceed",
    "exceeding" -> "exceed",
    "succeed" -> "succeed",
    "succeeds" -> "succeed",
    "succeeded" -> "succeed",
    "succeeding" -> "succeed")

  private class Word(var word: String) {
    // Consolidate UTF8 single quote characters to ASCII single quote \u0027
    word = word.replace("\u2019", "\u0027")
    word = word.replace("\u2018", "\u0027")
    word = word.replace("\u201B", "\u0027")

    private def r(word: String): String = {
      word.dropWhile(!vowels(_)).dropWhile(vowels(_)).drop(1)
    }

    def r1(): String = {
      word match {
        case word if word.startsWith("gener") || word.startsWith("arsen") => word.substring(5)
        case word if word.startsWith("commun") => word.substring(6)
        case _ => r(word)
      }
    }

    def r2(): String = r(r1)

    def shortSyllable(): Boolean = {
      val vc = vowels ++ "wxY".toSet
      word match {
        case word if word.size < 2 => false
        case word if word.size == 2 => if (vowels(word(0)) && !vowels(word(1))) true else false
        case word if !vowels(word(word.size - 3)) && vowels(word(word.size - 2)) && !vc(word(word.size - 1)) => 
          true
        case _ => false
      }
    }

    def short(): Boolean = {
      r1.isEmpty && shortSyllable()
    }

    def hasVowel(beg: Int = 0, end: Int = this.size): Boolean = {
      val i = if (beg >= 0) beg else word.size + beg
      val e = if (end >= 0) end else word.size + end
      word.substring(i, e).exists(vowels(_))
    }

    def ends(end: String): Boolean = word.endsWith(end)
    def endsWith(end: String): Boolean = word.endsWith(end)

    def suffixReplace(suff: String, repl: String): Unit = {
      word = word.substring(0, word.size - suff.size) + repl
    }

    def suffixReplace(sufflength: Int, repl: String): Unit = {
      word = word.substring(0, word.size - sufflength) + repl
    }

    def prefixReplace(pref: String, repl: String): Unit = {
      word = repl + word.substring(pref.size + 1)
    }

    def prefixReplace(preflen: Int, repl: String): Unit = {
      word = repl + word.substring(preflen + 1)
    }

    def cutRight(n: Int): Unit = {
      word = word.substring(0, word.size - n)
    }

    def append(suff: String): Unit = {
      word = word + suff
    }

    def updated(i: Int, c: Char) = {
      word = word.updated(i, c)
    }

    def apply(n: Int): Char =  word(n)
    def size(): Int = word.size
    def length(): Int = word.size
    def suffix(n: Int): String = word.substring(word.size - n)
    def prefix(n: Int): String = word.substring(0, n)
    override def toString = s"Word($word, $r1, $r2)"
  }


  private def step0(word: Word): Unit = {
    for (suff <- step0_suffs) {
      if (word.ends(suff)) {
        word.cutRight(suff.size)
        return
      }
    }
  }

  private def step1a(word: Word): Unit = {
    // Order of if operations matter here
    if (step1a_suffs.exists(word.ends(_))) {
      if (word.ends("sses"))
        word.cutRight(2)
      else if (word.ends("ied") || word.ends("ies"))
        if (word.size > 4) word.cutRight(2) else word.cutRight(1)
      else if (word.ends("ss") || word.ends("us")) {}  // Do nothing!
      else if (word.ends("s") && word.size > 2 && word.hasVowel(end = -2))
        word.cutRight(1)
    }
  }

  private def step1b(word: Word): Unit = {
    if (word.ends("eedly")) {
      if (word.r1.endsWith("eedly")) {
        word.cutRight(3)
        return
      } else return
    } else if (word.ends("eed")) {
      if (word.r1.endsWith("eed")) {
        word.cutRight(1)
        return
      } else return
    } else {
      val suffs = List("ingly", "edly", "ing", "ed")
      val end = suffs.find(word.ends(_)).getOrElse("")
      if (end.size > 0 && word.hasVowel(end = -end.size)) {
        word.cutRight(end.size)

        if (List("at", "bl", "iz").find(word.ends(_)).isDefined) {
          word.append("e")
        } else if (word.size >= 2 && doubles(word.suffix(2))) {
          word.cutRight(1)
        } else if (word.short) {
          word.append("e")
        }
      }
    }
  }

  private def step1c(word: Word): Unit = {
    if (word.size <= 2)  // certain edge cases require this
      return
    val ultimate = word(word.size - 1)
    val penultimate = word(word.size - 2)
    if (Set('Y', 'y')(ultimate) && !vowels(penultimate)) {
      word.suffixReplace(1, "i")
    }
  }

  private def step2(word: Word): Unit = {
    val suffs = step2_suffs.keys.toList.sortBy(0 - _.size)
    val end = suffs.find(word.ends(_)).getOrElse("")
    if (end.size > 0) {
      if(word.r1.endsWith(end)) {
        if (end == "ogi" && !word(word.size - 4).equals('l')) return
        if (end == "li" && !li_endings(word(word.size - 3))) return
        word.suffixReplace(end, step2_suffs(end))
      }
    }
  }

  private def step3(word: Word): Unit = {
    val suffs = step3_suffs.keys.toList.sortBy(0 - _.size)
    val end = suffs.find(word.ends(_)).getOrElse("")
    if (end.size > 0) {
      if (word.r1.endsWith(end)) {
        if (end == "ative" && !word.r2.endsWith(end))
          return
        word.suffixReplace(end, step3_suffs(end))
      }
    }
  }

  private def step4(word: Word): Unit = {
    val suffs = step4_suffs.keys.toList.sortBy(0 - _.size)
    val end = suffs.find(word.ends(_)).getOrElse("")
    if (end.size > 0) {
      if (word.r2.endsWith(end)) {
        if (end == "ion" && !Set('s', 't')(word(word.size - end.size - 1)))
          return
        word.suffixReplace(end, "")
      }
    }
  }

  private def step5(word: Word): Unit = {
    if (word.size == 0) return
    val ultimate = word(word.size - 1)
    if (ultimate == 'e') {
      if (word.r2.endsWith("e")) {
        word.suffixReplace(1, "")
      } else if (word.r1.endsWith("e")) {
        val w = new Word(word.prefix(word.size - 1))
        if (!w.shortSyllable()) {
          word.suffixReplace(1, "")
        }
      }
    } else if (ultimate == 'l') {
      if (word.r2.endsWith("l") && word.size > 2 && word(word.size - 2).equals('l'))
        word.suffixReplace(1, "")
    }
  }

  def stem(s: String): String = {
    // println(s"Start of function: $s")
    val lower = s.toLowerCase
    if (special_words.keys.toSet(lower)) return special_words(lower)
    val word = new Word(lower)
    if (word.size <= 2) return word.word
    // TODO Need to replace all other apostrophes with ASCII ' (u0027)
    if (word(0).equals('\'')) word.prefixReplace(0, "")
    if (word(0).equals('y')) word.prefixReplace(0, "Y")
    for (i <- 1 to word.size - 1) {
      if (word(i).equals('y') && vowels(word(i-1))) word.updated(i, 'Y')
    }
    step0(word)
    step1a(word)
    step1b(word)
    step1c(word)
    step2(word)
    step3(word)
    step4(word)
    step5(word)
    word.word.toLowerCase
  }
}
