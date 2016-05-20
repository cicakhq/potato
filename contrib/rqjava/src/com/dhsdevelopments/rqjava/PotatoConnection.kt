package com.dhsdevelopments.rqjava

import com.google.gson.Gson
import com.rabbitmq.client.*
import java.io.BufferedReader
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.util.logging.Level
import java.util.logging.Logger

class PotatoConnection(val host: String,
                       val virtualHost: String,
                       val username: String,
                       val password: String) {

    companion object {
        val SEND_MESSAGE_EXCHANGE_NAME = "chat-image-response-ex"
    }

    private var conn: Connection? = null

    val amqpConn: Connection
        get() = conn!!

    fun connect() {
        val fac = ConnectionFactory()
        fac.virtualHost = virtualHost
        fac.username = username
        fac.password = password
        fac.host = host

        conn = fac.newConnection()
    }

    fun disconnect() {
        val connCopy = synchronized(this) {
            conn.apply { conn = null }
        }

        if (connCopy == null) {
            throw IllegalStateException("Already disconnected")
        }

        connCopy.close()
    }

    fun subscribeChannel(cid: String, callback: (Message) -> Unit): ChannelSubscription {
        return ChannelSubscription(this, cid, callback)
    }

    fun registerCmd(cmd: String, callback: (Command) -> Unit): CmdRegistration {
        return CmdRegistration(this, cmd, callback)
    }

    fun sendMessage(sender: String, cid: String, text: String, extraHtml: String? = null) {
        val channel = amqpConn.createChannel()
        try {
            val content = StringBuilder()
            content.append("(:POST (\"")
            content.append(cid)
            content.append("\" :SENDER \"")
            content.append(sender)
            content.append("\" :TEXT \"")
            content.append(escapeSexpString(text))
            content.append("\"")
            if(extraHtml != null) {
                content.append(" :EXTRA-HTML \"")
                content.append(escapeSexpString(extraHtml))
                content.append("\"")
            }
            content.append("))")
            channel.basicPublish(SEND_MESSAGE_EXCHANGE_NAME, cid, null, content.toString().toByteArray(CHARSET_NAME_UTF8))
        }
        finally {
            channel.close()
        }
    }

    private fun escapeSexpString(text: String) = text.replace("\"", "\\\"")
}
