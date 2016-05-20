package com.dhsdevelopments.rqjava

import com.google.gson.Gson
import com.rabbitmq.client.AMQP
import com.rabbitmq.client.Channel
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import java.io.ByteArrayInputStream
import java.io.InputStreamReader
import java.nio.charset.Charset
import java.util.logging.Level
import java.util.logging.Logger

class ChannelSubscription(val conn: PotatoConnection, val cid: String, val callback: (Message) -> Unit) {
    private val rqChannel: Channel

    companion object {
        private val logger = Logger.getLogger(ChannelSubscription::class.qualifiedName)
        private val UTF8 = Charset.forName("UTF-8")
    }

    init {
        rqChannel = conn.amqpConn.createChannel()
        try {
            val q = rqChannel.queueDeclare("", true, false, true, null)
            // routing key format: DOMAIN.CHANNEL.SENDER
            rqChannel.queueBind(q.queue, "message-send-ex", "*.$cid.*")

            val consumer = object : DefaultConsumer(rqChannel) {
                override fun handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: ByteArray) {
                    val gson = Gson()

                    val message = InputStreamReader(ByteArrayInputStream(body), UTF8).use {
                        gson.fromJson(it, Message::class.java)
                    }

                    callback(message)
                }
            }

            rqChannel.basicConsume(q.queue, true, consumer)
        }
        catch(e: Exception) {
            try {
                rqChannel.close()
            }
            catch(e2: Exception) {
                logger.log(Level.SEVERE, "Error closing channel when handling exception in subscribeChannel", e2)
            }
            throw e
        }
    }

    fun disconnect() {
        rqChannel.close()
    }
}