package com.dhsdevelopments.rqjava

import com.google.gson.annotations.SerializedName

class Message {
    @SerializedName("id")
    lateinit var id: String

    @SerializedName("created_date")
    lateinit var createdDate: String

    @SerializedName("channel")
    lateinit var channel: String

    @SerializedName("from")
    lateinit var from: String

    @SerializedName("from_name")
    lateinit var fromName: String

    @SerializedName("text")
    lateinit var text: String

    @SerializedName("use_math")
    var useMath: Boolean = false

    @SerializedName("hash")
    lateinit var hash: String
}
