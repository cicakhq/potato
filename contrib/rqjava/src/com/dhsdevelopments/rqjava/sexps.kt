package com.dhsdevelopments.rqjava

import java.io.*
import java.util.*

class SexpParser(input: Reader) {
    companion object {
        val CHAR_NAME_MAP = buildCharNameMap()
    }

    val reader: PushbackReader

    init {
        reader = PushbackReader(input)
    }

    private fun readChar(): Char {
        val ch = reader.read()
        if (ch == -1) {
            throw IOException("End of file while reading stream")
        }
        return ch.toChar()
    }

    private fun readCharSkipSpace(): Char {
        while (true) {
            val ch = readChar()
            if (!ch.isWhitespace()) {
                return ch
            }
        }
    }

    private fun unreadChar(ch: Char) {
        reader.unread(ch.toInt())
    }

    fun parseSexp(): Any {
        val ch = readCharSkipSpace()
        return when (ch.toChar()) {
            '(' -> readList()
            '"' -> readString()
            '|' -> readEscapedSymbol()
            '#' -> readExtended()
            else -> readAtom(ch)
        }
    }

    fun readList(): SexpCons {
        val first = parseSexp()
        val ch = readCharSkipSpace()
        if(ch == ')') {
            return SexpCons(first, SexpSymbol.makeNull())
        }

        if(ch == '.') {
            val chAfterPeriod = readChar()
            if(chAfterPeriod.isWhitespace()) {
                val second = parseSexp()
                if (readCharSkipSpace() != ')') {
                    throw SexpParseException("Only a single element after period is allowed")
                }
                return SexpCons(first, second)
            }
            else {
                unreadChar(chAfterPeriod)
            }
        }

        unreadChar(ch)
        val second = readList()
        return SexpCons(first, second)
    }

    fun readArray(): List<Any> {
        val result = ArrayList<Any>()
        while (true) {
            val ch = readCharSkipSpace()
            if (ch == ')') {
                return result
            }
            else {
                unreadChar(ch)
                result.add(parseSexp())
            }
        }
    }

    fun readExtended(): Any {
        val ch = readChar()
        return when (ch) {
            '(' -> readArray()
            '\\' -> readSexpChar()
            else -> throw SexpParseException("Illegal extension char: $ch")
        }
    }

    fun readSexpChar(): Char {
        val s = readSymbolString()
        if (s.length == 1) {
            return s[0]
        }
        else {
            val ch = CHAR_NAME_MAP[s.toLowerCase()]
            if (ch == null) {
                throw SexpParseException("Illegal character name: s")
            }
            return ch
        }
    }

    fun readString(): String {
        val result = StringBuilder()
        while (true) {
            val ch = readChar()
            when (ch) {
                '"' -> return result.toString()
                '\\' -> result.append(readChar())
                else -> result.append(ch)
            }
        }
    }

    fun readAtom(ch: Char): Any {
        if (isSymbolChar(ch)) {
            unreadChar(ch)
            return readSymbol()
        }
        else if (ch.isDigit()) {
            unreadChar(ch)
            return readNumber()
        }
        else {
            throw SexpParseException("Unexpected character: ${ch.toInt()}")
        }
    }

    fun readSymbol(): SexpSymbol {
        return SexpSymbol.makeSymbol(readSymbolString())
    }

    fun readSymbolString(): String {
        val buf = StringBuilder()
        buf.append(readChar())
        while (true) {
            val next = readChar()
            if (!isSymbolCharCont(next)) {
                unreadChar(next)
                return buf.toString()
            }
            buf.append(next)
        }
    }

    fun readEscapedSymbol(): SexpSymbol {
        val buf = StringBuilder()
        while (true) {
            val ch = readChar()
            when (ch) {
                '\\' -> buf.append(readChar())
                '|' -> return SexpSymbol.makeSymbol(buf.toString())
                else -> buf.append(ch)
            }
        }
    }

    fun readNumber(): Int {
        val buf = StringBuilder()
        while (true) {
            val ch = readChar()
            if (ch >= '0' && ch <= '9') {
                buf.append(ch);
            }
            else {
                unreadChar(ch)
                return buf.toString().toInt()
            }
        }
    }

    private fun isSymbolChar(ch: Char): Boolean {
        return ch.isLetter() || ch == '-' || ch == '*' || ch == '_' || ch == '+' || ch == '%'
    }

    private fun isSymbolCharCont(ch: Char): Boolean {
        return isSymbolChar(ch) || ch.isDigit() || ch == '.'
    }
}

private fun buildCharNameMap(): Map<String, Char> {
    return mapOf(
            "newline" to '\n',
            "return" to '\r',
            "space" to ' ')
}

class SexpParseException(s: String) : Exception(s)

class SexpSymbol(val name: String, val isNull: Boolean) {
    companion object {
        fun makeSymbol(name: String): SexpSymbol {
            return SexpSymbol(name, name == "NIL")
        }

        fun makeNull(): SexpSymbol {
            return makeSymbol("NIL")
        }
    }

    override fun toString(): String {
        return "Symbol(name='$name')"
    }
}

class SexpCons(val first: Any, val second: Any) {
    fun nthElement(index: Int): Any {
        var curr = this
        for (i in 0..(index - 1)) {
            if (this.second !is SexpCons) {
                throw IllegalStateException("Element not a cons")
            }
            curr = this.second
        }
        return curr.first
    }

    override fun toString(): String {
        return "SexpCons(first=$first, second=$second)"
    }
}

fun parseSexp(s: String): Any {
    val parser = SexpParser(StringReader(s))
    return parser.parseSexp()
}

fun parseSexp(s: Reader): Any {
    val parser = SexpParser(BufferedReader(s))
    return parser.parseSexp()
}

class TestSexp {
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val res = parseSexp("((a . b) \"foo\" \"bar\" (palle \"he\\\"llo\") test-bar |symbol with spaces| 12 #\\a #\\Space #(1 2 3))")
            println("res = $res\n")
        }
    }
}
