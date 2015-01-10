var device_info = { "vendorId": 0x16d0, "productId": 0x09a0 };
var debug = false;

var packetSize = 64;    // number of bytes in an HID packet
var payloadSize = packetSize - 2;

var AUTH_REQ_TIMEOUT = 15000;   // timeout for requests sent to mooltipass

//var reContext = /^\https?\:\/\/(?:www)?([\w\d.\-\_]+)/;   // URL regex to extract base domain for context
var reContext = /^\https?\:\/\/([\w\-\+]+\.)*([\w\-\_]+\.[\w\-\_]+)/;   // URL regex to extract base domain for context

// Commands that the MP device can send.
var CMD_DEBUG               = 0x01;
var CMD_PING                = 0x02;
var CMD_VERSION             = 0x03;
var CMD_CONTEXT             = 0x04;
var CMD_GET_LOGIN           = 0x05;
var CMD_GET_PASSWORD        = 0x06;
var CMD_SET_LOGIN           = 0x07;
var CMD_SET_PASSWORD        = 0x08;
var CMD_CHECK_PASSWORD      = 0x09;
var CMD_ADD_CONTEXT         = 0x0A;
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
var CMD_DRAW_BITMAP         = 0x43;
var CMD_SET_FONT            = 0x44;
var CMD_EXPORT_FLASH_START  = 0x45;
var CMD_EXPORT_EEPROM_START = 0x46;
var CMD_SET_BOOTLOADER_PWD  = 0x47;
var CMD_JUMP_TO_BOOTLOADER  = 0x48;
var CMD_CLONE_SMARTCARD     = 0x49;
var CMD_STACK_FREE          = 0x4A;
var CMD_GET_USERPROFILE     = 0x50;
var CMD_END_MEMORYMGMT      = 0x51;
var CMD_IMPORT_MEDIA_START  = 0x52;
var CMD_IMPORT_MEDIA        = 0x53;
var CMD_IMPORT_MEDIA_END    = 0x54;
var CMD_STACK_FREE          = 0x50;
var CMD_RESET_CARD          = 0x60;

var PLUGIN_BYTE_NOCARD	    = 0x03; // Response to CMD_CONTEXT if no card.

// supported flash chips
// 264,   512,  128   1MB   0001 ID:00010=2  5  7 12, 6 2 16 S: 3 - 8,120,128
// 264,  1024,  128   2MB   0010 ID:00011=3  5  7 12, 5 3 16 S: 7 - 8,120,128
// 264,  2048,  256   4MB   0100 ID:00100=4  4  8 12, 4 5 17 S: 7 - 8,248,256
// 264,  4096,  256   8MB   1000 ID:00101=5  3  9 12, 3 4 17 S: 15 - 8,248,256
// 528,  4096,  256  16MB  10000 ID:00110=6  2  9 13, 2 4 18 S: 15 - 8,248,256
// 528,  8192,  128  32MB 100000 ID:00111=7  1 10 13, 1 6 17 S: 63 - 8,120,128

var FLASH_CHIP_1M           = 1;   // 1M Flash Chip (AT45DB011D)
var FLASH_CHIP_2M           = 2;   // 2M Flash Chip (AT45DB021E)
var FLASH_CHIP_4M           = 4;   // 4M Flash Chip (AT45DB041E)
var FLASH_CHIP_8M           = 8;   // 8M Flash Chip (AT45DB081E)
var FLASH_CHIP_16M          = 16;  // 16M Flash Chip (AT45DB161E)
var FLASH_CHIP_32M          = 32;  // 32M Flash Chip (AT45DB321E)
var FLASH_MEDIA_START_PAGE  = 8;

var flashInfo = {
     1: { pageSize: 264, pageCount:  512, pagesPerSector: 128 },
     2: { pageSize: 264, pageCount: 1024, pagesPerSector: 128 },
     4: { pageSize: 264, pageCount: 2048, pagesPerSector: 256 },
     8: { pageSize: 264, pageCount: 4096, pagesPerSector: 256 },
    16: { pageSize: 528, pageCount: 4096, pagesPerSector: 256 },
    32: { pageSize: 528, pageCount: 8192, pagesPerSector: 128 }
};

var flashChipId = null;

var clientId      = null;      // chrome extension address
var connection    = null;      // connection to the mooltipass
var connected     = false;     // current connection state
var version       = 'unknown'; // connected mooltipass version
var authReq       = null;      // current authentication request
var authReqQueue  = [];
var context       = null;
var contextGood   = false;
var createContext = false;
var loginValue    = null;

var connectMsg = null;  // saved message to send after connecting

var FLASH_PAGE_COUNT = 512;
var FLASH_PAGE_SIZE  = 264;
var EEPROM_SIZE      = 1024;
var FLASH_EXPORT_ALL = false;

var exportData = null;        // arraybuffer for receiving exported data
var exportDataUint8 = null;   // uint8 view of exportData
var exportDataEntry = null;   // File entry for flash export
var exportDataOffset = 0;     // current data offset in arraybuffer

var importData = {};          // arraybuffer holding data to import exported data
var importDataOffset = 0;     // current data offset in import arraybuffer
var importDataPageOffset = 0; // current write page offset

var mediaData = null;         // arraybuffer of media file data for slot storage
var mediaDataOffset = 0;      // current data offset in mediaData array

var media = {};               // media file info from mooltipass

var importProgressBar = null;
var exportProgressBar = null;
var uploadProgressBar = null;

var authReqTimeout = null;  // timer for auth requests sent to mooltipass

// map between input field types and mooltipass credential types
var getFieldMap = {
    password:   CMD_GET_PASSWORD,
    login:      CMD_GET_LOGIN,
};

var setFieldMap = {
    password:   CMD_SET_PASSWORD,
    login:      CMD_SET_LOGIN,
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
    connected = false;
    authReq = null;     // current authentication request
    context = null;
    contextGood = false;
    createContext = false;
    loginValue = null;
    connectMsg = null;

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
        if (false && type != CMD_EXPORT_FLASH && type != CMD_EXPORT_EEPROM && type != CMD_IMPORT_FLASH)
        {
            appendToLog('#messageLog','body '+JSON.stringify(body)+'\n');
        }
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
    sendMsg(msg);
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

    appendToLog('#messageLog', 'building request from array length '+data.length+'\n');

    for (var ind=0; ind<data.size; ind++)
    {
        appendToLog('#messageLog', 'adding array length '+data[ind].length+' '+JSON.stringify(data[ind])+'\n');
        body.set(data[ind], offset);
        offset += data[ind].length;
    }
    sendRequest(type, body);
}

/**
 * Get the next credential field value from the mooltipass
 * The pending credential is set to the next one, and
 * a request is sent to the mooltipass to get its value.
 */
function getNextField()
{
    if (authReq && authReq.type == 'inputs')
    {
        if (authReq.keys.length > 0) {
            authReq.pending = authReq.keys.pop();
            //console.log('getNextField(): request '+authReq.pending);
            sendRequest(getFieldMap[authReq.pending]);
        } else {
            // got all the credentials
            //console.log('getNextField(): got all fields '+JSON.stringify(authReq.inputs));
            chrome.runtime.sendMessage(authReq.senderId, {type: 'credentials', inputs: authReq.inputs});
            appendToLog('#messageLog','sent credentials\n');
            authReq = null;
        }
    }
    else
    {
        appendToLog('#messageLog',  'getNextField(): no authReq\n');
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
        if (authReq.keys.length > 0)
        {
            var key = authReq.keys.pop();
            authReq.pending = key;

            if (key in setFieldMap)
            {
                sendString(setFieldMap[key], authReq.inputs[key].value);
            }
            else
            {
                console.log('setNextField: type "'+authReq.key+'" not supported');
                authReq.pending = null;
                setNextField(); // try the next field
            }
        } else {
            // no more input fields to set on mooltipass
            chrome.runtime.sendMessage(authReq.senderId, {type: 'updateComplete'});
            appendToLog('#messageLog', 'update finished \n');
            endAuthRequest();
        }
    }
    else
    {
        appendToLog('#messageLog',  'setNextField: no authReq\n');
    }
}

function setContext(create)
{
    createContext = create;
    appendToLog('#messageLog', 'Set context: "'+authReq.context+'" \n');
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
                appendToLog('#exportLog', 'Error during write: ' + this.error.toString() + '\n');
            }
            else
            {
                if (fileWriter.length === 0) {
                    // truncate has finished
                    var blob = new Blob([data], {type: 'application/octet-binary'});
                    appendToLog('#exportLog',  'writing '+data.length+' bytes\n');
                    fileWriter.write(blob);
                    appendToLog('#exportLog', 'Save complete\n');
                } else {
                }
            }
        }
        fileWriter.truncate(0);
    });
}

function appendToLog(logId, text)
{
    console.log(logId, text);
    sendToElm({appendToLog:logId + ":" + text});
}

/**m
 * Return a sorted list of field keys
 */
function getKeys(fields)
{
    return Object.keys(fields).sort(function(a, b)
    {
        if (a == 'login')
        {
            return 1;
        } else {
            return 0;
        }
    });
}

/**
 * timeout waiting for current auth request to complete
 */
function timeoutAuthRequest()
{
    console.log('authreq timeout');
    authReqTimeout = null;
    endAuthRequest();
}

/**
 * end the current auth request
 */
function endAuthRequest()
{
    if (debug) {
        if (authReq) {
            console.log('endAuthRequest '+authReq.type);
        } else {
            console.log('endAuthRequest - no active request');
        }
    }
    if (authReqTimeout != null) {
        console.log('clear authreq timeout');
        clearTimeout(authReqTimeout);
        authReqTimeout = null;
    } else {
        console.log('no timeout to clear');
    }
    if (authReqQueue.length > 0) {
        authReq = authReqQueue.shift();
        startAuthRequest(authReq);
    } else {
        authReq = null;
    }
}

/**
 * start a new auth request
 */
function startAuthRequest(request)
{
    switch (request.type) {
    case 'ping':    // hellow from extension
        clientId = request.senderId;
        // Send current mooltipass status
        if (connected) {
            console.log('got extension ping, sending connected');
            chrome.runtime.sendMessage(request.senderId, {type: 'connected', version: version});
        } else {
            console.log('got extension ping, sending disconnected');
            chrome.runtime.sendMessage(request.senderId, {type: 'disconnected'});
        }
        break;
    case 'inputs':
        clientId = request.senderId;
        authReq = request;
        console.log('URL: '+request.url);
        authReq.keys = getKeys(request.inputs);

        console.log('keys: '+JSON.stringify(authReq.keys))

        match = reContext.exec(request.url);
        if (match.length > 1) {
            if (!context || context != match[2]) {
                context = match[2];
                console.log('context: '+context);
            } else {
                console.log('not updating context '+context+' to '+match[1]);
            }
        }
        authReq.context = context;

        authReqTimeout = setTimeout(timeoutAuthRequest, AUTH_REQ_TIMEOUT);
        setContext(false);
        break;

    case 'update':
        clientId = request.senderId;
        authReq = request;
        match = reContext.exec(request.url);
        if (match.length > 1) {
            authReq.context = match[2];
            console.log('auth context: '+authReq.context);
        }
        appendToLog('#messageLog', 'update:\n');
        for (var key in request.inputs)
        {
            id = (request.inputs[key].id) ? request.inputs[key].id : request.inputs[key].name;
            if (key == 'password') {
                appendToLog('#messageLog', '    set "'+id+'" = "'+request.inputs[key].value.replace(/./gi, '*')+'"\n');
            } else {
                appendToLog('#messageLog', '    set "'+id+'" = "'+request.inputs[key].value+'"\n');
            }
        }

        authReq.keys = getKeys(request.inputs);
        authReqTimeout = setTimeout(timeoutAuthRequest, AUTH_REQ_TIMEOUT);

        if (!contextGood || (context != authReq.context)) {
            setContext(true);
        } else {
            setNextField();
        }
        break;


    default:
        // not a supported request type
        endAuthRequest();
        break;
    }
}

/**
/* importer = { data: ArrayBuffer of data to send
 *               offset: current offset in data
 *               pageSpace: space left in page
 *               log: optional output log
 *               bar: optional progress bar
 *               }
 */
function sendNextPacket(cmd, importer)
{
    // need to match data to page sizes
    if (!importer.pageSpace)
    {
        importer.pageSpace = FLASH_PAGE_SIZE;
        importer.offset = 0;
    }
    var size = Math.min(importer.data.byteLength - importer.offset, payloadSize, importer.pageSpace);

    if (size <= 0)
    {
        // finished
        sendRequest(cmd+1);     // END
        return 0;
    }

    data = new Uint8Array(importer.data, importer.offset, size);

    // debug
    if (false && importer.log && ((importer.offset * 100)/importer.data.byteLength) > 95)
    {
        appendToLog(importer.log, 'import: offset '+importer.offset+' size '+size+' pageSpace '+importer.pageSpace+'\n');
    }

    importer.pageSpace -= size;
    if (importer.pageSpace <= 0)
    {
        importer.pageSpace = FLASH_PAGE_SIZE;
    }
    importer.offset += size;

    if (importer.bar)
    {
        importer.bar.progressbar('value', (importer.offset * 100)/ importer.data.byteLength);
    }
    sendRequest(cmd, data);

    return importer.data.byteLength - importer.offset;
}

function updateMedia(data)
{
    update = new Uint16Array(data,2,8);
    files = new Uint16Array(data,6,40);
    fileSizes = new Uint16Array(data,46,20);

    media.pageSize = update[0];
    media.size = update[1];
    media.file = new Uint16Array(files.length);
    media.file.set(files);
    media.fileSize = new Uint8Array(fileSizes.length);
    media.fileSize.set(fileSizes);
}

function allocateMediaPage(size)
{
    var ind=0;
}

/**
 * Handler for receiving new data from the mooltipass.
 * Decodes the HID message and updates the HTML message divider with
 * to report the received message.
 * @param data the received data
 */
function onDataReceived(reportId, data)
{
    if (typeof reportId === "undefined" || typeof data === "undefined")
    {
        if (chrome.runtime.lastError)
        {
            var err = chrome.runtime.lastError;
            if (err.message != "Transfer failed.")
            {
                console.log("Error in onDataReceived: " + err.message);
            }
        }
        return;
    }

    var bytes = new Uint8Array(data);
    var msg = new Uint8Array(data,2);
    var len = bytes[0]
    var cmd = bytes[1]

    if (debug && (cmd != CMD_VERSION) && (cmd != CMD_DEBUG) && ((cmd < CMD_EXPORT_FLASH) || (cmd >= CMD_EXPORT_EEPROM)))
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
            appendToLog('#debugLog', msg);
            break;
        }
        case CMD_PING:
            appendToLog('#messageLog', 'command: ping\n');
            break;
        case CMD_VERSION:
        {
            version = arrayToStr(new Uint8Array(data.slice(3)));
            if (!connected) {
                flashChipId = msg[0];
                appendToLog('#messageLog', 'Connected to Mooltipass ' + version + ' flashId '+flashChipId+'\n');
                connected = true;
                if (clientId) {
                    chrome.runtime.sendMessage(clientId, {type: 'connected', version: version});
                }
                sendToElm({setConnected:1});
            }
            break;
        }
        case CMD_ADD_CONTEXT:
            contextGood = (bytes[2] == 1);
            if (!contextGood)
            {
                appendToLog('#messageLog',  'failed to create context '+authReq.context+'\n');
                endAuthRequest();
            } else {
                appendToLog('#messageLog', 'created context "'+authReq.context+'" for '+authReq.type+'\n');
                appendToLog('#messageLog', 'setting context "'+authReq.context+'" for '+authReq.type+'\n');
                console.log('Added context, now set context "'+authReq.context+'" for '+authReq.type);
                sendString(CMD_CONTEXT, authReq.context);
            }
            break;

        case CMD_CONTEXT:
            contextGood = (bytes[2] == 1);
            noCard = (bytes[2] == PLUGIN_BYTE_NOCARD);

            if (contextGood) {
                appendToLog('#messageLog', 'Active: "'+authReq.context+'" for '+authReq.type+'\n');
                console.log('Successfully set context "'+authReq.context+'" for '+authReq.type);
                chrome.runtime.sendMessage(clientId, {type: 'cardPresent', state: true});
            } else if (noCard){
                appendToLog('#messageLog', 'No card: "'+authReq.context+'" for '+authReq.type+'\n');
                console.log('No card received when setting context: "'+authReq.context+'" for '+authReq.type);
                chrome.runtime.sendMessage(clientId, {type: 'cardPresent', state: false});
            } else {
                console.log('Failed to set context "'+authReq.context+'"');
                appendToLog('#messageLog','Unknown context "'+authReq.context+'" for '+authReq.type+'\n');
				chrome.runtime.sendMessage(clientId, {type: 'cardPresent', state: true});
            }

            if (authReq) {
                if (contextGood) {
                    if (authReq.type == 'inputs') {
                        getNextField();
                    } else {
                        setNextField();
                    }
                } else if (createContext) {
                    createContext = false;
                    appendToLog('#messageLog','add new context "'+authReq.context+'" for '+authReq.type+'\n');
                    sendString(CMD_ADD_CONTEXT, authReq.context);
                } else {
                    console.log('Failed to set up context "'+authReq.context+'"');
                    chrome.runtime.sendMessage(clientId, {type: 'noCredentials'});
                    // failed to set up context
                    endAuthRequest();
                }
            }
            break;

        // Input Fields
        case CMD_GET_LOGIN:
        case CMD_GET_PASSWORD:
            if (authReq && authReq.pending) {
                if (len > 1) {
                    var key = authReq.pending;
                    var value = arrayToStr(new Uint8Array(data.slice(2)));
                    if (key == 'password') {
                        appendToLog('#messageLog',  'got '+key+' = "'+value.replace(/./gi, '*')+'"\n');
                    } else {
                        appendToLog('#messageLog',  'got '+key+' = "'+value+'"\n');
                    }
                    authReq.inputs[key].value = value;

                    if (authReq.type == 'inputs') {
                        getNextField();
                    } else {
                        setNextField();
                    }
                } else {
                    // failed
                    endAuthRequest();
                }
            }
            break;

        // update and set results
        case CMD_SET_LOGIN:
            if (authReq && authReq.type == 'inputs' && authReq.pending) {
                if (bytes[2] == 1)
                {
                    console.log('set '+authReq.pending+' for '+authReq.context);
                    appendToLog('#messageLog', 'get '+authReq.pending+'\n');
                    // XXX is this still needed?
                    sendRequest(getFieldMap[authReq.pending]);
                    break;
                }
                // fallthrough
            }
        case CMD_SET_PASSWORD:
        {
            var type = (authReq && authReq.pending) ? authReq.pending : '(unknown type)';
            if (bytes[2] == 1)
            {
                // success
                appendToLog('#messageLog', 'set '+type+' on mooltipass\n');
            }
            else
            {
                // failed
                appendToLog('#messageLog', 'set failed for '+type+'\n');
            }
            setNextField();

            break;
        }

        case CMD_EXPORT_FLASH_START:
        {
            var ok = (bytes[2] == 1);

            if (ok)
            {
                // proceed
                args = new Uint8Array([0]);     // restart export from 0
                console.log('exporting flash');
                sendRequest(CMD_EXPORT_FLASH, args);
            }
            break;
        }

        case CMD_EXPORT_EEPROM_START:
        {
            var ok = (bytes[2] == 1);

            if (ok)
            {
                // proceed
                args = new Uint8Array([0]);     // restart export from 0
                sendRequest(CMD_EXPORT_EEPROM, args);
            }
            break;
        }

        case CMD_EXPORT_FLASH:
        case CMD_EXPORT_EEPROM:
            if (!exportData)
            {
                console.log('new export');
                var size;
                if (cmd == CMD_EXPORT_FLASH)
                {
                    console.log('flashChipId '+flashChipId +
                                ' pageSize ' + flashInfo[flashChipId].pageSize +
                                ' pages '+ flashInfo[flashChipId].pageCount +
                                ' media start page '+ FLASH_MEDIA_START_PAGE);

                    size = (flashInfo[flashChipId].pageSize * flashInfo[flashChipId].pageCount);

                    if (!FLASH_EXPORT_ALL) {
                        // export skips the media partition
                        size -= (flashInfo[flashChipId].pageSize * (flashInfo[flashChipId].pagesPerSector - FLASH_MEDIA_START_PAGE));
                    }
                }
                else
                {
                    size = EEPROM_SIZE;
                }
                console.log('exporting '+size+' bytes');
                exportData = new ArrayBuffer(size);
                exportDataUint8 = new Uint8Array(exportData);
                exportDataOffset = 0;
                console.log('new export ready');
                exportProgressBar.progressbar('value', 0);
            }
            // data packet
            packet = new Uint8Array(data.slice(2,2+len));
            if ((packet.length + exportDataOffset) < exportDataUint8.length)
            {
                exportDataUint8.set(packet, exportDataOffset);
                exportDataOffset += packet.length;
                exportProgressBar.progressbar('value', (exportDataOffset * 100) / exportDataUint8.length);
                args = new Uint8Array([1]);     // request next packet
                sendRequest(cmd, args);
            } else {
                if ((packet.length + exportDataOffset) > exportDataUint8.length)
                {
                    var overflow = (packet.length + exportDataOffset) - exportDataUint8.length;
                    console.log('error packet overflows buffer by '+overflow+' bytes');
                }

                // done, write the file to disk
                saveToEntry(exportDataEntry, exportDataUint8)
                exportData = null;
                exportDataUint8 = null;
                exportDataOffset = 0;
                exportDataEntry = null;;
            }
            break;

        case CMD_EXPORT_FLASH_END:
        case CMD_EXPORT_EEPROM_END:
            if (exportData && exportDataEntry)
            {
                appendToLog('#exportLog', 'export: saving to file\n');
                if (exportDataOffset < exportDataUint8.length)
                {
                    console.log('WARNING: only received '+exportDataOffset+' of '+exportDataUint8.length+' bytes');
                    appendToLog('#exportLog', 'WARNING: only received '+exportDataOffset+' of '+exportDataUint8.length+' bytes\n');
                }
                saveToEntry(exportDataEntry, exportDataUint8)
            }
            else
            {
                appendToLog('#exportLog', 'Error received export end ('+cmd+') with no active export\n');
            }
            exportData = null;
            exportDataUint8 = null;
            exportDataOffset = 0;
            exportDataEntry = null;;
            break;

        case CMD_ERASE_EEPROM:
        case CMD_ERASE_FLASH:
            appendToLog('#developerLog', (bytes[2] == 1) ? 'succeeded\n' : 'failed\n');
            break;

        case CMD_RESET_CARD:
            appendToLog('#developerLog', (bytes[2] == 1) ? 'succeeded\n' : 'failed\n');
            break;

        case CMD_IMPORT_FLASH_BEGIN:
        case CMD_IMPORT_FLASH:
        {
            var ok = bytes[2];
            if (ok == 0) {
                appendToLog('#importLog', 'import denied\n');
            } else {
                sendNextPacket(CMD_IMPORT_FLASH, importData);
            }
            break;
        }
        case CMD_IMPORT_MEDIA_START:
        case CMD_IMPORT_MEDIA:
        {
            var ok = bytes[2];
            if (ok == 0) {
                appendToLog('#importLog', 'import denied\n');
            } else {
                sendNextPacket(CMD_IMPORT_MEDIA, importData);
            }
            break;
        }
        case CMD_IMPORT_MEDIA_END:
            appendToLog('#importLog', 'import completed\n');
            importData = null;
            break;

        case CMD_IMPORT_FLASH_END:
        case CMD_IMPORT_EEPROM_END:
            importData = null;
            appendToLog('#importLog', 'import finished\n');
            break;

        case CMD_IMPORT_EEPROM_BEGIN:
        case CMD_IMPORT_EEPROM:
        {
            var ok = bytes[2];
            if (ok == 0) {
                remainder = importData.data.byteLength - importData.offset;
                if (remainder > 0) {
                    appendToLog('#importLog', 'import halted, '+remainder+' bytes left\n');
                } else {
                    appendToLog('#importLog', 'import finished\n');
                }
                importData = null;
            } else {
                sendNextPacket(CMD_IMPORT_EEPROM, importData);
            }
            break;
        }

        default:
            appendToLog('#messageLog', 'unknown command '+cmd+'\n');
            break;
    }
    if (connection) {
        chrome.hid.receive(connection, onDataReceived);
    }
};

function sendMsg(msg)
{
    if (debug) {
        msgUint8 = new Uint8Array(msg);
        // don't output the CMD_VERSION command since this is the keep alive
        if (msgUint8[1] != CMD_VERSION) {
            console.log('sending '+JSON.stringify(new Uint8Array(msg)));
        }
    }
    chrome.hid.send(connection, 0, msg, function()
    {
        if (!chrome.runtime.lastError)
        {
            chrome.hid.receive(connection, onDataReceived);
        }
        else
        {
            if (connected)
            {
                if (debug) {
                    console.log('Failed to send to device: '+chrome.runtime.lastError.message);
                }
                appendToLog('#messageLog', 'Disconnected from mooltipass\n');
                if (clientId) {
                    chrome.runtime.sendMessage(clientId, {type: 'disconnected'});
                }
                reset();
            }
        }
    });
}

function sendPing()
{
    msg = new ArrayBuffer(packetSize);
    data = new Uint8Array(msg);
    data.set([0, CMD_VERSION], 0);
    sendMsg(msg);
}


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
    if (devices.length <= 0)
    {
        return;
    }

    var ind = devices.length - 1;
    console.log('Found ' + devices.length + ' devices.');
    console.log('Device ' + devices[ind].deviceId + ' vendor' + devices[ind].vendorId + ' product ' + devices[ind].productId);
    //console.log('Device usage 0 usage_page' + devices[ind].usages[0].usage_page + ' usage ' + devices[ind].usages[0].usage);
    var devId = devices[ind].deviceId;

    console.log('Connecting to device '+devId);
    appendToLog('#messageLog', 'Connecting to device...\n');
    chrome.hid.connect(devId, function(connectInfo)
    {
        if (!chrome.runtime.lastError)
		{
            connection = connectInfo.connectionId;

            if (connectMsg)
            {
                sendMsg(connectMsg);
            }
            else
            {
                sendPing();
            }
        }
        else
        {
          console.log('Failed to connect to device: '+chrome.runtime.lastError.message);
          reset();
        }
    });
}

function checkConnection()
{
    if (!connected) {
        connect();
    } else {
        sendPing();
    }
}

setInterval(checkConnection,2000);
