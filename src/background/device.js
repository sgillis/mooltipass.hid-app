var device = {connection: null, connecting: 0, waitingForStatus : false};
var device_info = { "vendorId": 0x16d0, "productId": 0x09a0 };      // Mooltipass
var PACKET_SIZE = 64;

/**
 * Handler invoked when new USB mooltipass devices are found.
 * @param devices array of device objects
 * @note only the last device is used, assumes that one mooltipass is present.
 * Stale entries appear to be left in chrome if the mooltipass is removed
 * and plugged in again, or the firmware is updated.
 */
onDeviceFound = function (devices)
{
    if (devices.length <= 0)
    {
        return;
    }

    var ind = devices.length - 1;
    var devId = devices[ind].deviceId;

    chrome.hid.connect(devId, function(connectInfo)
    {
        if (!chrome.runtime.lastError)
		{
            device.connection = connectInfo.connectionId;
            deviceSendToElm({setHidConnected:true});
            elm.ports.deviceStatus.send(7);
            deviceSendToElm({appendToLog:"device found, connection made"});
        }
        clearTimeout(device.timeoutId);
        device.connecting = false;
    });
}

device.connect = function ()
{
    if (device.connecting === 1)
        return;
    else if (device.connecting === 0) {
        deviceSendToElm({appendToLog:"> looking for device"});
    }
    device.connecting = 1;
    device.timeoutId = setTimeout(function () {
        if (device.connecting === 1) {
            device.connecting = 2;
        }
    }, 5000)
    chrome.hid.getDevices(device_info, onDeviceFound);
}


function onDataReceived(reportId, data)
{
    var bytes = new Uint8Array(data);
    var ints = [];
    for (var i = 0, len = bytes.length; i < len; i++)
    {
        ints[i] = bytes[i];
    }
    if (ints[1] === 112) {//status update
        //console.log("<<");
        elm.ports.deviceStatus.send(ints[2]);
        device.waitingForStatus = false;
    } else  {
        deviceSendToElm({receiveCommand: ints});
    }

    //special case for read node reply message as we need to read 3 messages in
    //a row
    if (ints[1] == 85)
        chrome.hid.receive(device.connection, onDataReceived);
}

function hidErrorDisconnect(message) {
        console.log("hid error: ", message);
        device.connecting = 0;
        device.connection = null;
        device.waitingForStatus = false;
        deviceSendToElm({setHidConnected:false});
        //make sure then next status won't be dropped because of dropRepeats
        elm.ports.deviceStatus.send(7);
}

function sendMsg(message)
{
    if (device.connection == null) {
        hidErrorDisconnect("no connection when trying to send message")
        return;
    }
    if (message[1] === 112) { //status update
        if (device.waitingForStatus)
            return;
        else
            device.waitingForStatus = true;
    }
    //else
    //    console.log("app", message);
    //Buffer creation is a bit awkward because windows doesn't like us using
    //the Uint8Array.buffer directly (or maybe it's something to do with the
    //ArrayBuffer size argument?). This is what works on all platforms equally.
    var buffer = new ArrayBuffer(PACKET_SIZE);
    var view = new Uint8Array(buffer);
    view.set(message,0);
    chrome.hid.send(device.connection, 0, buffer, function()
    {
        if (!chrome.runtime.lastError)
        {
            chrome.hid.receive(device.connection, onDataReceived);
        }
        else
        {
            hidErrorDisconnect(chrome.runtime.lastError.message)
        }
    });
}
