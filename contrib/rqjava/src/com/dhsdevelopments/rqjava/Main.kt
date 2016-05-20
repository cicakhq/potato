package com.dhsdevelopments.rqjava

class Main {
    companion object {
        @JvmStatic
        fun main(args: Array<String>): Unit {
            val conn = PotatoConnection("localhost", "/", "elias", "foo")
            conn.connect()

            conn.subscribeChannel("4e8ff1e0e1c7d80e8e4e5cea510147db", { msg -> println("From: ${msg.fromName} (${msg.from}), text: ${msg.text}") })
            conn.registerCmd("as") { cmd ->
                println("cmd=${cmd.cmd}, args=${cmd.args}, channel=${cmd.channel}, domain=${cmd.domain}, user=${cmd.user}")
                conn.sendMessage(cmd.user, cmd.channel, "HTML injection by command handler", cmd.args)
            }
        }
    }
}
