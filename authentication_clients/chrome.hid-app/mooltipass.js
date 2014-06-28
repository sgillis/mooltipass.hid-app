/* CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at src/license_cddl-1.0.txt
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at src/license_cddl-1.0.txt
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */

/* Copyright (c) 2014 Darran Hunt. All rights reserved. */

/*!      \file mooltipass.js
*        \brief        Mooltipass Chrome HID App plugin
*        Created: 30/5/2014
*        Author: Darran Hunt
*/


/*
 * The Basic sequence is:
 * 1. extension detects credential input fields in a web page.
 * 2. extension sends request for credential values to this app
 * 3. app connects to mooltipass
 * 4. app sets context based on the URL of the web page
 * 5. app requests each of the credentials from the mooltipass
 * 6. app sends all of the credentials to the extension
 * 7. extension fills in the input fields in the web page.
 */

var device_info = { "vendorId": 0x16d0, "productId": 0x09a0 };      // Mooltipass
//var device_info = { "vendorId": 0x16c0, "productId": 0x0486 };    // Teensy 3.1

var packetSize = 64;    // number of bytes in an HID packet
var payloadSize = packetSize - 2;

var reContext = /^\https?\:\/\/([\w.]+)/;   // URL regex to extract base domain for context

// Commands that the MP device can send.
var CMD_DEBUG        = 0x01;
var CMD_PING         = 0x02;
var CMD_VERSION      = 0x03;
var CMD_CONTEXT      = 0x04;
var CMD_GET_LOGIN    = 0x05;
var CMD_GET_PASSWORD = 0x06;
var CMD_SET_LOGIN    = 0x07;
var CMD_SET_PASSWORD = 0x08;
var CMD_CHECK_PASSWORD = 0x09;
var CMD_ADD_CONTEXT  = 0x0A;
var CMD_EXPORT_FLASH        = 0x30;    // resp: 0x30 packets until 0x31
var CMD_EXPORT_FLASH_END    = 0x31;
var CMD_IMPORT_FLASH_BEGIN  = 0x32;    // confirmed by 0x32,0x01
var CMD_IMPORT_FLASH        = 0x33;    // send 4x60 byte + 1x24 byte packets, acked with 0x33,0x01
var CMD_IMPORT_FLASH_END    = 0x34;
var CMD_EXPORT_EEPROM       = 0x35;    // resp: 0x35 packets until 0x36
var CMD_EXPORT_EEPROM_END   = 0x36;
var CMD_IMPORT_EEPROM_BEGIN = 0x37;    // confirmed by 0x37,0x01
var CMD_IMPORT_EEPROM       = 0x38;    // send packet, acked with 0x38,0x01
var CMD_IMPORT_EEPROM_END   = 0x39; 

var CMD_ERASE_EEPROM        = 0x40;
var CMD_ERASE_FLASH         = 0x41;
var CMD_ERASE_SMC           = 0x42;
var CMD_ALLOCATE_SLOT       = 0x43;
var CMD_WRITE_SLOT          = 0x44;
var CMD_ERASE_SLOTS         = 0x45;
var CMD_DRAW_SLOT           = 0x46;
var CMD_FLASH_READ          = 0x47;

var connection = null;  // connection to the mooltipass
var authReq = null;     // current authentication request
var context = null;
var contextGood = false;
var createContext = false;
var loginValue = null;

var connectMsg = null;  // saved message to send after connecting

var FLASH_PAGE_COUNT = 512;
var FLASH_PAGE_SIZE = 264;
var EEPROM_SIZE = 1024;

var exportData = null;        // arraybuffer for receiving exported data
var exportDataUint8 = null;   // uint8 view of exportData 
var exportDataEntry = null;   // File entry for flash export
var exportDataOffset = 0;     // current data offset in arraybuffer

var mediaData = null;        // arraybuffer of media file data for slot storage
var mediaDataOffset = 0;     // current data offset in mediaData array

var importProrgessBar = null;
var exportProgressBar = null;
var uploadProgressBar = null;

// map between input field types and mooltipass credential types
var getFieldMap = {
    password:   CMD_GET_PASSWORD,
    email:      CMD_GET_LOGIN,
    username:   CMD_GET_LOGIN,
    user_id:    CMD_GET_LOGIN,
    name:       CMD_SET_LOGIN
};

var setFieldMap = {
    password:   CMD_SET_PASSWORD,
    email:      CMD_SET_LOGIN,
    username:   CMD_SET_LOGIN,
    user_id:    CMD_SET_LOGIN,
    name:       CMD_SET_LOGIN
};


/**
 * convert a string to a uint8 array
 * @param str the string to convert
 * @returns the uint8 array representing the string with a null terminator
 * @note does not support unicode yet
 */
function strToArray(str) 
{
    var buf = new Uint8Array(str.length+1);
    for (var ind=0; ind<str.length; ind++) 
    {
        buf[ind] = str.charCodeAt(ind);
    }
    buf[ind] = 0;
    return buf;
}
 

/**
 * convert a uint8 array to a string
 * @param buf the array to convert
 * @returns the string representation of the array
 * @note does not support unicode yet
 */
function arrayToStr(buf)
{
    res = '';
    for (var ind=0; ind<buf.length; ind++) 
    {
        if (buf[ind] == 0) 
        {
            return res;
        } else {
            res += String.fromCharCode(buf[ind]);
        }
    }
    return res;
}


/**
 * reset all state data
 */
function reset()
{
    connection = null;  // connection to the mooltipass
    //authReq = null;     // current authentication request
    //context = null;
    //contextGood = false;
    //createContext = false;
    loginValue = null;

    exportData = null;        // arraybuffer for receiving exported data
    exportDataUint8 = null;   // uint8 view of exportData 
    exportDataEntry = null;   // File entry for flash export
    exportDataOffset = 0;     // current data offset in arraybuffer
}

/**
 * Connect to the mooltipass
 */
function connect(msg)
{
    reset();
    console.log('Connecting...');
    if (msg)
    {
        connectMsg = msg;
    }
    chrome.hid.getDevices(device_info, onDeviceFound);
}


/**
 * Send a binary message to the mooltipass
 * @param type the request type to send (e.g. CMD_VERSION)
 * @param content Uint8 array message content (optional)
 */
function sendRequest(type, content)
{
    msg = new ArrayBuffer(packetSize);
    header = new Uint8Array(msg, 0);
    body = new Uint8Array(msg, 2);

    if (content)
    {
        header.set([content.length, type], 0);
        body.set(content, 0);
        log('#messageLog','body '+JSON.stringify(body)+'\n');
    }
    else
    {
        header.set([0, type], 0);
    }

    if (!connection)
    {
        connect(msg);
        return;
    }

    chrome.hid.send(connection, 0, msg, function() 
    {
        if (!chrome.runtime.lastError) 
        {
            //log('#messageLog','Send complete\n')
            //chrome.hid.receive(connection, packetSize, onContextAck);
        }
        else
        {
          log('#messageLog', 'Failed to send to device: '+chrome.runtime.lastError.message+'\n');
          reset();
        }					
    });
}


/**
 * Send a command and string to the mooltipass
 * @param type the command type (e.g. CMD_SET_PASSWORD)
 * @param str the string to send with the command
 */
function sendString(type, str)
{
    sendRequest(type, strToArray(str));
}


/**
 * Send a command and string to the mooltipass
 * @param type the command type (e.g. CMD_SET_PASSWORD)
 * @param data array of uint8arrays to combine into a message
 */
function sendRequestArray(type, data)
{
    payload = new ArrayBuffer(packetSize);
    body = new Uint8Array(payload, 0);
    var offset = 0;

    log('#messageLog', 'building request from array length '+data.length+'\n');

    for (var ind=0; ind<data.size; ind++)
    {
        log('#messageLog', 'adding array length '+data[ind].length+' '+JSON.stringify(data[ind])+'\n');
        body.set(data[ind], offset);
        offset += data[ind].length;
    }
    sendRequest(type, body);
}


/**
 * Push the current pending credential onto the
 * credentials list with the specified value.
 * @param value the credential value
 */
function storeField(value)
{
    if (authReq.pending) {
        authReq.pending.value = value;
        authReq.credentials.push(authReq.pending);
        authReq.pending = null;
    }
    else
    {
        console.log('err: storeField('+value+') no field pending');
    }
}


/**
 * Get the next credential field value from the mooltipass
 * The pending credential is set to the next one, and
 * a request is sent to the mooltipass to retreive its value.
 */
function getNextField()
{
    if (authReq && authReq.type == 'inputs')
    {
        if (authReq.inputs.length > 0) 
        {
            authReq.pending = authReq.inputs.pop();
            var type = authReq.pending.type;

            if (type in getFieldMap)
            {
                if (type == 'login')
                {
                    loginValue = null;
                }
                console.log('get '+type+' for '+authReq.context+' '+authReq.pending.type);
                log('#messageLog',  'get '+type+'\n');
                sendRequest(getFieldMap[type]);
            }
            else
            {
                console.log('getNextField: type "'+authReq.pending.type+'" not supported');
                authReq.pending = null;
                getNextField(); // try the next field
            }
        } else {
            // no more input fields to fetch from mooltipass, send credentials to the web page
            chrome.runtime.sendMessage(authReq.senderId, {type: 'credentials', fields: authReq.credentials});
            log('#messageLog','sent credentials to '+authReq.senderId+'\n');
            authReq = null;
        }
    }
    else
    {
        log('#messageLog', 'no authReq\n');
    }
}


/**
 * Set the next credential field value from the mooltipass
 * The pending credential is set to the next one, and
 * a request is sent to the mooltipass to set its value.
 */
function setNextField()
{
    if (authReq && authReq.type == 'update')
    {
        if (authReq.inputs.length > 0) 
        {
            authReq.pending = authReq.inputs.pop();
            var type = authReq.pending.type;

            if (type in setFieldMap)
            {
                console.log('set '+type+' for '+authReq.context+' '+authReq.pending.type+' to '+authReq.pending.value);
                log('#messageLog', 'set '+type+' = "'+authReq.pending.value+'"\n');
                sendString(setFieldMap[type], authReq.pending.value);
            }
            else
            {
                console.log('setNextField: type "'+authReq.pending.type+'" not supported');
                authReq.pending = null;
                getSetField(); // try the next field
            }
        } else {
            // no more input fields to set on mooltipass
            // XXX todo add an error check / ACK back to the web page?
            log('#messageLog', 'update finished \n');
            authReq = null;
        }
    }
    else
    {
        log('#messageLog',  'no authReq\n');
    }
}

function setContext(create)
{
    createContext = create;
    log('#messageLog', 'Set context: "'+authReq.context+'" \n');
    // get credentials from mooltipass
    contextGood = false;
    sendString(CMD_CONTEXT, authReq.context);
}

function saveToEntry(entry, data) 
{
    entry.createWriter(function(fileWriter) 
    {
        fileWriter.onwriteend = function(e) 
        {
            if (this.error)
            {
                log('#exportLog', 'Error during write: ' + this.error.toString() + '\n');
            }
            else
            {
                if (fileWriter.length === 0) {
                    // truncate has finished
                    var blob = new Blob([data], {type: 'application/octet-binary'});
                    log('#exportLog',  'writing '+data.length+' bytes\n');
                    fileWriter.write(blob);
                    log('#exportLog', 'Save complete\n');
                } else {
                }
            }
        }
        fileWriter.truncate(0);
    });
}

function log(logId, text)
{
    var box = $(logId);
    if (text) {
        box.val(box.val() + text);
        box.scrollTop(box[0].scrollHeight);
    } else {
        box.val('');
    }
}

/**
 * Initialise the app window, setup message handlers.
 */
function initWindow()
{
    var connectButton = document.getElementById("connect");
    var clearButton = document.getElementById("clear");
    var clearDebugButton = document.getElementById("clearDebug");
    var exportFlashButton = document.getElementById("exportFlash");
    var exportEepromButton = document.getElementById("exportEeprom");
    var exportStoreButton = document.getElementById("exportStore");
    var eraseEepromButton = document.getElementById("eraseEeprom");
    var eraseFlashButton = document.getElementById("eraseFlash");
    var eraseSmartcardButton = document.getElementById("eraseSmartcard");
    var loadSlotButton = document.getElementById("loadSlot");
    var eraseSlotsButton = document.getElementById("eraseSlots");
    var drawSlotButton = document.getElementById("drawSlot");

    // clear contents of logs
    $('#messageLog').html('');
    $('#debugLog').html('');
    $('#exportLog').html('');
    $('#developerLog').html('');
    var messageLog = $('#messageLog');

    connectButton.addEventListener('click', function() { connect(); });
    clearButton.addEventListener('click', function() { log('#messageLog'); });
    clearDebugButton.addEventListener('click', function() {  log('#debugLog'); });
    
    exportFlashButton.addEventListener('click', function() 
    {
        chrome.fileSystem.chooseEntry({type:'saveFile', suggestedName:'mpflash.bin'}, function(entry) {
            if (entry)
            {
                log('#exportLog', 'save mpflash.img\n');
                exportDataEntry = entry;
                exportData = null;
                exportProgressBar.progressbar('value', 0);
                sendRequest(CMD_EXPORT_FLASH);
            }
        });
    });

    exportEepromButton.addEventListener('click', function() 
    {
        chrome.fileSystem.chooseEntry({type:'saveFile', suggestedName:'mpeeprom.bin'}, function(entry) {
            if (entry)
            {
                log('#exportLog', 'save mpeeprom.img\n');
                exportDataEntry = entry;
                exportData = null;
                exportProgressBar.progressbar('value', 0);
                sendRequest(CMD_EXPORT_EEPROM);
            }
        });
    });

    exportStoreButton.addEventListener('click', function() 
    {
        chrome.fileSystem.chooseEntry({type:'saveFile', suggestedName:'flash.bin'}, function(entry) {
            if (entry)
            {
                log('#exportLog', 'save flash.bin\n');
                exportDataEntry = entry;
                exportData = null;
                exportProgressBar.progressbar('value', 0);
                var size = payloadSize - 5;
                args = new Uint8Array([0, 0, 0, 0, payloadSize-5]);
                sendRequest(CMD_FLASH_READ, args);
            }
        });
    });

    eraseFlashButton.addEventListener('click', function() 
    {
        $('#eraseConfirm').dialog({
            buttons: {
                "Erase Mooltipass Flash?": function() 
                {
                    log('#developerLog', 'Erasing flash... ');
                    sendRequest(CMD_ERASE_FLASH);
                    $(this).dialog('close');
                },
                Cancel: function() 
                {
                    $(this).dialog('close');
                }
            }
        });
    });

    eraseEepromButton.addEventListener('click', function() 
    {
        $('#eraseConfirm').dialog({
            buttons: {
                "Erase Mooltipass EEPROM?": function() 
                {
                    log('#developerLog', 'Erasing EEPROM... ');
                    sendRequest(CMD_ERASE_EEPROM);
                    $(this).dialog('close');
                },
                Cancel: function() 
                {
                    $(this).dialog('close');
                }
            }
        });
    });

    eraseSmartcardButton.addEventListener('click', function() 
    {
        $('#eraseConfirm').dialog({
            buttons: {
                "Erase Mooltipass Smartcard?": function() 
                {
                    log('#developerLog', 'Erasing smartcard... ');
                    sendRequest(CMD_ERASE_SMC);
                    $(this).dialog('close');
                },
                Cancel: function() 
                {
                    $(this).dialog('close');
                }
            }
        });
    });

    loadSlotButton.addEventListener('click', function(e)
    {
        chrome.fileSystem.chooseEntry({type: 'openFile'}, function(entry) {
            entry.file(function(file) {
                var reader = new FileReader();

                reader.onerror = function(e)
                {
                    log('#messageLog', 'Failed to read media file\n');
                    log('#messageLog', e+'\n');
                };
                reader.onloadend = function(e) {
                    mediaData = reader.result;  // keep data in a global for writing to mp
                    mediaDataOffset = 0;
                    // Start sending it to the mooltipass
                    size = new Uint8Array([mediaData.byteLength & 0xFF, mediaData.byteLength >> 8])
                    log('#messageLog', 'allocating slot for '+mediaData.byteLength+' bytes from media file\n');
                    sendRequest(CMD_ALLOCATE_SLOT, size);
                };

                reader.readAsArrayBuffer(file);
            });
        });
    });

    eraseSlotsButton.addEventListener('click', function()
    {
        $('#eraseConfirm').dialog({
            buttons: {
                "Erase all storage slots?": function() 
                {
                    log('#developerLog', 'Erasing smartcard... ');
                    sendRequest(CMD_ERASE_SLOTS);
                    $(this).dialog('close');
                },
                Cancel: function() 
                {
                    $(this).dialog('close');
                }
            }
        });
    });

    drawSlotButton.addEventListener('click', function()
    {
        log('#messageLog', 'Drawing slot 1');
        slot = new Uint8Array([1]);
        sendRequest(CMD_DRAW_SLOT, slot);
    });


    chrome.runtime.onMessageExternal.addListener(function(request, sender, sendResponse) 
    {
        console.log(sender.tab ?  'from a content script:' + sender.tab.url : 'from the extension');

        switch (request.type)
        {
            case 'inputs':
                console.log('URL: '+request.url);

                // sort the fields so that the login is first
                request.inputs.sort(function(a, b)
                {
                    if (a == 'login')
                    {
                        return 0;
                    } else {
                        return 1;
                    }
                });

                console.log('inputs:');
                for (var ind=0; ind<request.inputs.length; ind++)
                {
                    id = 'id' in request.inputs[ind] ? request.inputs[ind].id : request.inputs[ind].name;
                    console.log('    "'+id+'" type='+request.inputs[ind].type);
                }
                authReq = request;
                authReq.senderId = sender.id;
                authReq.credentials = [];
                //request.context = getContext(request); URL -> context
                match = reContext.exec(request.url);
                if (match.length > 0) {
                    if (!context || context != match[1]) {
                        context = match[1];
                        console.log('context: '+context);
                    } else {
                        console.log('not updaing context '+context+' to '+match[1]);
                    }
                }
                authReq.context = context;

                setContext(false);
                break;

            case 'update':
                authReq = request;
                authReq.senderId = sender.id;
                match = reContext.exec(request.url);
                if (match.length > 0) {
                    authReq.context = match[1];
                    console.log('auth context: '+authReq.context);
                }
                console.log('update:');
                for (var ind=0; ind<request.inputs.length; ind++)
                {
                    id = 'id' in request.inputs[ind] ? request.inputs[ind].id : request.inputs[ind].name;
                    console.log('    "'+id+'" type='+request.inputs[ind].type+', value="'+request.inputs[ind].value);
                }

                // sort the fields so that the login is first
                authReq.inputs.sort(function(a, b)
                {
                    if (a == 'login')
                    {
                        return 0;
                    } else {
                        return 1;
                    }
                });

                if (!contextGood || (context != authReq.context)) {
                    setContext(true);
                } else {
                    setNextField();
                }
                break;

            default:
                break;
        }

    });

    // configure jquery ui elements
	$("#manage").accordion();
	$("#developer").accordion();
	importProrgessBar = $("#importProgressbar").progressbar({ value: 0 });
	exportProgressBar = $("#exportProgressbar").progressbar({ value: 0 });
	uploadProgressBar = $("#uploadProgressbar").progressbar({ value: 0 });
    $("#connect").button();
    $("#clear").button();
    $("#clearDebug").button();
    $("#exportFlash").button();
    $("#exportEeprom").button();
    $("#exportStore").button();
    $("#eraseFlash").button();
    $("#eraseEeprom").button();
    $("#eraseSmartcard").button();
    $("#loadSlot").button();
    $("#eraseSlots").button();
    $("#drawSlot").button();
    $("#tabs").tabs();
};


/**
 * Handler for receiving new data from the mooltipass.
 * Decodes the HID message and updates the HTML message divider with
 * to report the received message.
 * @param data the received data
 */
function onDataReceived(data) 
{
    var bytes = new Uint8Array(data);
    var msg = new Uint8Array(data,2);
    var len = bytes[0]
    var cmd = bytes[1]

    if ((cmd != CMD_DEBUG) && (cmd < CMD_EXPORT_FLASH))
    {
        console.log('Received CMD ' + cmd + ', len ' + len + ' ' + JSON.stringify(msg));
    }

    switch (cmd) 
    {
        case CMD_DEBUG:
        {
            var msg = "";
            for (var i = 0; i < len; i++) 
            {
                    msg += String.fromCharCode(bytes[i+2]);
            }
            log('#debugLog', msg);
            break;
        }
        case CMD_PING:
            log('#messageLog', 'command: ping\n');
            break;
        case CMD_VERSION:
        {
            var version = "" + bytes[2] + "." + bytes[3];
            log('#messageLog', 'Connected to Mooltipass ' + version + '\n');
            if (authReq) 
            {
                log('#messageLog', 'Context: "'+authReq.context+'"\n');
                sendString(CMD_CONTEXT, authReq.context);
                console.log('Initial set context "'+authReq.context+'"');
            }
            break;
        }
        case CMD_ADD_CONTEXT:
            contextGood = (bytes[2] == 1);
            if (!contextGood)
            {
                log('#messageLog',  'failed to create context '+authReq.context+'\n');
            } else {
                log('#messageLog', 'created context "'+authReq.context+'" for '+authReq.type+'\n');
                log('#messageLog', 'setting context "'+authReq.context+'" for '+authReq.type+'\n');
                console.log('Added context, now set context "'+authReq.context+'" for '+authReq.type);
                sendString(CMD_CONTEXT, authReq.context);
            }
            break;

        case CMD_CONTEXT:
            contextGood = (bytes[2] == 1);

            if (contextGood) {
                log('#messageLog', 'Active: "'+authReq.context+'" for '+authReq.type+'\n');
                console.log('Successfully set context "'+authReq.context+'" for '+authReq.type);
            } else {
                console.log('Failed to set context "'+authReq.context+'"');
                log('#messageLog','Unknown context "'+authReq.context+'" for '+authReq.type+'\n');
            }

            if (authReq) 
            {
                if (contextGood)
                {
                    switch (authReq.type)
                    {
                        case 'inputs':
                            // Start getting each input field value
                            getNextField();
                            break;
                        case 'update':
                            setNextField();
                            break;
                        default:
                            break;
                    }
                }
                else if (createContext) 
                {
                    createContext = false;
                    log('#messageLog','add new context "'+authReq.context+'" for '+authReq.type+'\n');
                    sendString(CMD_ADD_CONTEXT, authReq.context);
                } else {
                    // failed to set up context
                    authReq = null;
                }
            }
            break;

        // Input Fields
        case CMD_GET_LOGIN:
            if ((len > 1) && (loginValue == null)) {
                loginValue = arrayToStr(new Uint8Array(data.slice(2)));
            }
        case CMD_GET_PASSWORD:
        {
            if (len > 1) 
            {
                if (authReq && authReq.pending) {
                    log('#messageLog',  authReq.pending.type);
                    var value = arrayToStr(new Uint8Array(data.slice(2)));
                    log('#messageLog', ': "'+value+'"\n');
                    storeField(value);
                } else {
                    // no pending credential request
                }
            }
            else 
            {
                log('#messageLog', 'no value found for '+authReq.pending.type+'\n');
            }
            getNextField();
            break;
        }

        // update and set results
        case CMD_SET_LOGIN:
            if (authReq && authReq.type == 'inputs' && authReq.pending) {
                if (bytes[2] == 1)
                {
                    console.log('get '+authReq.pending.type+' for '+authReq.context);
                    log('#messageLog', 'get '+authReq.pending.type+'\n');
                    sendRequest(getFieldMap[authReq.pending.type]);
                    break;
                }
                // fallthrough
            }
        case CMD_SET_PASSWORD:
        {
            var type = (authReq && authReq.pending) ? authReq.pending.type : '(unknown type)';
            if (bytes[2] == 1) 
            {
                // success
                log('#messageLog', 'set '+type+' on mooltipass\n');
            }
            else 
            {
                // failed
                log('#messageLog', 'set failed for '+type+'\n');
            }
            setNextField();
            
            break;
        }

        case CMD_EXPORT_FLASH:
        case CMD_EXPORT_EEPROM:
            if (!exportData)
            {
                console.log('new export');
                var size = (cmd == CMD_EXPORT_FLASH) ? (FLASH_PAGE_COUNT*FLASH_PAGE_SIZE) : EEPROM_SIZE;
                exportData = new ArrayBuffer(size);
                exportDataUint8 = new Uint8Array(exportData);
                exportDataOffset = 0;
                console.log('new export ready');
                exportProgressBar.progressbar('value', 0);
            }
            // data packet
            packet = new Uint8Array(data.slice(2,2+len));
            if ((packet.length + exportDataOffset) > exportDataUint8.length)
            {
                var overflow = (packet.length + exportDataOffset) - exportDataUint8.length;
                console.log('error packet overflows buffer by '+overflow+' bytes');
                exportDataOffset += packet.length;
            } else {
                exportDataUint8.set(packet, exportDataOffset);
                exportDataOffset += packet.length;
                exportProgressBar.progressbar('value', (exportDataOffset * 100) / exportDataUint8.length);
            }
            break;

        case CMD_EXPORT_FLASH_END:
        case CMD_EXPORT_EEPROM_END:
            if (exportData && exportDataEntry)
            {
                log('#exportLog', 'export: saving to file\n');
                if (exportDataOffset < exportDataUint8.length)
                {
                    console.log('WARNING: only received '+exportDataOffset+' of '+exportDataUint8.length+' bytes');
                    log('#exportLog', 'WARNING: only received '+exportDataOffset+' of '+exportDataUint8.length+' bytes\n');
                }
                saveToEntry(exportDataEntry, exportDataUint8) 
            }
            else
            {
                log('#exportLog', 'Error received export end ('+cmd+') with no active export\n');
            }
            exportData = null;
            exportDataUint8 = null;
            exportDataOffset = 0;
            exportDataEntry = null;;
            break;

        case CMD_FLASH_READ:
        {
            var res = msg[0];
            if (res != 1)
            {
                log('#exportLog', 'Error reading flash\n');
                break;
            }
            if (!exportData)
            {
                console.log('new export');
                var size = 128*FLASH_PAGE_SIZE;
                exportData = new ArrayBuffer(size);
                exportDataUint8 = new Uint8Array(exportData);
                exportDataOffset = 0;
                console.log('new export ready');
                exportProgressBar.progressbar('value', 0);
            }

            addr = msg[4] << 24 | msg[3] << 16 | msg[2] << 8 | msg[1];
            var packet = new Uint8Array(data,7, len-5);

            log('#exportLog', 'flash read: addr 0x'+addr.toString(16)+' '+packet.length+' bytes\n');

            if ((packet.length + exportDataOffset) > exportDataUint8.length)
            {
                var overflow = (packet.length + exportDataOffset) - exportDataUint8.length;
                console.log('error packet overflows buffer by '+overflow+' bytes');
                exportDataOffset += packet.length;
            } else {
                exportDataUint8.set(packet, exportDataOffset);
                exportDataOffset += packet.length;
                exportProgressBar.progressbar('value', (exportDataOffset * 100) / exportDataUint8.length);

                var size = payloadSize - 5;
                if (size > (exportData.byteLength - exportDataOffset)) {
                    size = exportData.byteLength - exportDataOffset;
                }

                if (size > 0)
                {
                    args = new Uint8Array([
                                exportDataOffset & 0xFF,
                                (exportDataOffset >> 8) & 0xFF,
                                (exportDataOffset >> 16) & 0xFF,
                                (exportDataOffset >> 24) & 0xFF,
                                size]);
                    sendRequest(CMD_FLASH_READ, args);
                }
                else
                {
                    saveToEntry(exportDataEntry, exportDataUint8) 
                }
            }
            break;
        }

        case CMD_ERASE_EEPROM:
        case CMD_ERASE_FLASH:
        case CMD_ERASE_SMC:
            log('#developerLog', (bytes[2] == 1) ? 'succeeded\n' : 'failed\n');
            break;

        case CMD_ALLOCATE_SLOT: 
        {
            var slotId = bytes[2];
            if (slotId == 0) {
                log('#messageLog', 'allocate slot failed\n');
            } else {
                log('#messageLog', 'allocated slot '+slotId+'\n');
            }
            if (slotId > 0)
            {
                msg = new ArrayBuffer(Math.min(payloadSize, mediaData.byteLength));
                data = new Uint8Array(msg);
                data.set([slotId], 0);
                data.set(mediaData.slice(0, data.size-1), 1);
                sendRequest(CMD_WRITE_SLOT, data);
                mediaDataOffset = data.length;
                uploadProgressBar.progressbar('value', 0);
            }
            break;
        }

        case CMD_WRITE_SLOT :
        {
            var slotId = bytes[3];
            if (bytes[2] == 1)
            {
                if (mediaDataOffset < mediaData.byteLength)
                {
                    msg = new ArrayBuffer(Math.min(payloadSize, mediaData.byteLength - mediaDataOffset));
                    data = new Uint8Array(msg);
                    data.set([slotId], 0);
                    data.set(mediaData.slice(mediaDataOffset, data.size-1), 1);
                    sendRequest(CMD_WRITE_SLOT, data);
                    mediaDataOffset += data.length;
                    uploadProgressBar.progressbar('value', (mediaDataOffset * 100) / mediaData.byteLength);
                }
                else
                {
                    log('#messageLog', 'slot: '+slotId+' finished upload\n');
                }
            }
            else
            {
                log('#messageLog', 'slot: '+slotId+' failed to write\n');
            }
            break;
        }

        case CMD_ERASE_SLOTS:
            if (bytes[2] == 0)
            {
                log('#messageLog', 'erase slots failed\n');
            }
            else
            {
                log('#messageLog', 'erase slots succeeded\n');
            }
            break;

        default:
            log('#messageLog', 'unknown command '+cmd+'\n');
            break;
    }
    chrome.hid.receive(connection, packetSize, onDataReceived);
};


/**
 * Handler invoked when new USB mooltipass devices are found.
 * Connects to the device and sends a version request.
 * @param devices array of device objects
 * @note only the last device is used, assumes that one mooltipass is present.
 * Stale entries appear to be left in chrome if the mooltipass is removed
 * and plugged in again, or the firmware is updated.
 */
function onDeviceFound(devices) 
{
    var ind = devices.length - 1;
    console.log('Found ' + devices.length + ' devices.');
    console.log('Device ' + devices[ind].deviceId + ' vendor' + devices[ind].vendorId + ' product ' + devices[ind].productId);
    console.log('Device usage 0 usage_page' + devices[ind].usages[0].usage_page + ' usage ' + devices[ind].usages[0].usage);
    var devId = devices[ind].deviceId;

    console.log('Connecting to device '+devId);
    chrome.hid.connect(devId, function(connectInfo) 
    {
        if (!chrome.runtime.lastError) 
		{
            connection = connectInfo.connectionId;

            if (connectMsg)
            {
                // message pending to send
                msg = connectMsg;
                connectMsg = null;
                console.log('sending '+JSON.stringify(new Uint8Array(msg)));
            }
            else
            {
                msg = new ArrayBuffer(packetSize);
                data = new Uint8Array(msg);
                data.set([0, CMD_VERSION], 0);
                console.log('sending '+JSON.stringify(data));
            }
            chrome.hid.send(connection, 0, msg, function() 
            {
				if (!chrome.runtime.lastError) 
				{
					console.log('Send complete');
					chrome.hid.receive(connection, packetSize, onDataReceived);
				}
				else
				{
				  console.log('Failed to send to device: '+chrome.runtime.lastError.message);
                  log('#messageLog','Unable to connect.\n');
				  throw chrome.runtime.lastError.message;  
				}					
            });
        }
        else 
        {
          console.log('Failed to connect to device: '+chrome.runtime.lastError.message);
          throw chrome.runtime.lastError.message;    
        } 
    });
}

window.addEventListener('load', initWindow);
