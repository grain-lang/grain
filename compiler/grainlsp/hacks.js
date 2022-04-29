//Provides: caml_ml_open_descriptor_in
//Requires: caml_global_data,caml_sys_open,caml_raise_sys_error, caml_ml_channels
//Requires: fs_node_supported, caml_string_of_jsstring
function caml_ml_open_descriptor_in (fd)  {
    var data = caml_global_data.fds[fd];
    if(data.flags.wronly) caml_raise_sys_error("fd "+ fd + " is writeonly");
    var refill = null;
    if(fd == 0 && fs_node_supported()){
      var fs = require('fs');
      var Buffer = require('buffer').Buffer;
      refill = function () {
        var buf = Buffer.alloc(1024);
        var bytesRead = fs.readSync(0, buf);
        return caml_string_of_jsstring(buf.slice(0, bytesRead).toString('utf8'));
      };
    }
    var channel = {
      file:data.file,
      offset:data.offset,
      fd:fd,
      opened:true,
      out: false,
      refill:refill
    };
    caml_ml_channels[channel.fd]=channel;
    return channel.fd;
  }
