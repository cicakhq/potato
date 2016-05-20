package com.dhsdevelopments.rqjava

import com.rabbitmq.client.AMQP
import com.rabbitmq.client.Channel
import com.rabbitmq.client.DefaultConsumer
import com.rabbitmq.client.Envelope
import java.nio.charset.Charset

class CmdRegistration(potatoConnection: PotatoConnection, cmd: String, callback: (Command) -> Unit) {
    private val rqChannel: Channel

    init {
        rqChannel = potatoConnection.amqpConn.createChannel()
        val q = rqChannel.queueDeclare("", true, false, true, null)
        rqChannel.queueBind(q.queue, "slashcommand-ex", "*.*.*.$cmd")

        val consumer = object : DefaultConsumer(rqChannel) {
            override fun handleDelivery(consumerTag: String, envelope: Envelope, properties: AMQP.BasicProperties, body: ByteArray) {
                callback(Command(body, properties))
            }
        }
        rqChannel.basicConsume(q.queue, true, consumer)
    }

    fun disconnect() {
        rqChannel.close()
    }
}

class Command(body: ByteArray, props: AMQP.BasicProperties) {
    val domain = props.headers.get("domain").toString()
    val channel = props.headers.get("channel").toString()
    val user = props.headers.get("user").toString()
    val cmd: String
    val args: String

    init {
        val data = parseSexp(String(body, CHARSET_NAME_UTF8)) as List<*>
        cmd = data[0] as String
        args = data[1] as String
    }
}
