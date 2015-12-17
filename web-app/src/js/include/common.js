// -*- mode:js2 -*-

export function ajaxRequestBlock (url, data, callback) {
    $("#loading-dialog").dialog( "open" );
    ajaxRequest( url, data, function (xml) {
        $("#loading-dialog").dialog( "close" );
        callback( xml );
    });
}

function filterControlChars( string ) {
    return string.replace( /[\x00-\x1F\x7F-\x9F]/g, "" );
}

export function ajaxRequest (url, data, callback, error = null) {
    $.ajax( {
        type:        "POST",
        url:          url,
        data:         JSON.stringify( data ),
        dataType:    "text",
        contentType: "text/plain",
        processData:  false,
        success:      function( result ) {
            if( callback ) {
                var data = JSON.parse( filterControlChars( result ) );
                callback( data );
            }
        },
        error: error
    } );
}

export function ajaxRequestJson (url, data, callback) {
    ajaxRequest( url, data, function( xml ) {
        callback( JSON.parse( xml ) );
    } );
}

export function getURLParameter (name) {
    return decodeURI(
        (RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,null])[1]
    );
}

export function printableSize (size) {
    var kb = 1024;
    var mb = kb * 1024;
    //var gb = mb * 1024;
    if( size > mb ) {
        return Math.floor( size / mb ) + "\u00a0MB";
    }
    else if( size > kb ) {
        return Math.floor( size / kb ) + "\u00a0kB";
    }
    else {
        return size + "\u00a0B";
    }
}

export function getParameterByName (name) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

export function isEnabled (name) {
    var ref = getParameterByName(name).toUpperCase();
    return ((ref === 'TRUE') || (ref === 'YES') || (ref === '1'));
}

export function debug( message ) {
    if (window.potatoDebug) {
        console.log("DEBUG: "+message);
    }
}

window.potatoDebug = isEnabled("debug");
