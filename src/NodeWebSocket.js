// module NodeWebSocket

exports.mkServer = function() {
  return new require('websocket').server()
};

exports.registerEventHandlerUnsafe = function(receiver, method, msgType, callback, transform) {
  return function() {
    receiver[method](msgType, function(param) {
      callback(transform(param))()
    })
  }
}

exports.getMessageData = function(msg) {
  if (msg.type == 'utf8') {
    return msg.utf8Data
  } else if (msg.type == 'binary') {
    throw new Error('unhandled websocket message type: binary')
  }
}

exports.reject = function(request) {
  return function() {
    request.reject()
  }
}

exports.accept = function(request) {
  return function() {
    return request.accept(null, request.origin)
  }
}

exports.resourceUrl = function(request) {
  return request.resourceURL;
}

exports.sendImpl = function(conn, msg) {
  return function() {
    conn.sendUTF(msg)
  }
}

exports.mountImpl(wsServer, httpServer) {
  return function() {
    wsServer.mount({httpServer: httpServer})
  }
}

exports.close = function(conn) {
  return function() {
    conn.close()
  }
}
