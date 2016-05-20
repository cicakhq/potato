package com.dhsdevelopments.rqjava

import java.io.*
import java.nio.file.Files
import java.util.*

class SexpParser(input: Reader) {
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
            if (ch != ' ' && ch != '\t' && ch != '\n') {
                return ch
            }
        }
    }

    private fun unreadChar(ch: Char) {
        reader.unread(ch.toInt())
    }

    fun parseSexp(): Any {
        val ch = readChar()
        return when (ch.toChar()) {
            '(' -> readList()
            '"' -> readString()
            else -> readAtom(ch)
        }
    }

    fun readList(): List<Any> {
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
            return readSymbol(ch)
        }
        else if (ch.isDigit()) {
            unreadChar(ch)
            return readNumber()
        }
        else {
            throw IOException("Unexpected character: ${ch.toInt()}")
        }
    }

    fun readSymbol(ch: Char): Symbol {
        val buf = StringBuilder()
        buf.append(ch)
        while (true) {
            val next = readChar()
            if (!isSymbolCharCont(next)) {
                unreadChar(next)
                return Symbol(buf.toString())
            }
            buf.append(next)
        }
    }

    fun readNumber(): Int {
        val buf = StringBuilder()
        while(true) {
            val ch = readChar()
            if(ch >= '0' && ch <= '9') {
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

class Symbol(val name: String) {
    override fun toString(): String{
        return "Symbol(name='$name')"
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

class TestSexp
{
    companion object {
        @JvmStatic
        fun main(args: Array<String>) {
            val res = parseSexp("(\"foo\" \"bar\" (palle \"he\\\"llo\") test-bar 12)")
            println("res = $res\n")
        }
    }
}
