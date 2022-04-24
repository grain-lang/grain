// A wrapper around grainlsp_js.bc.js that prepares the `pkg` env
const preparePkg = require("./pkg");

preparePkg();

var lsp = require("./grainlsp_js.bc.js");

lsp.lspServer.enabledebug();

new Promise(resolve => {
    let message_count = 0
    let body = ''
    let reading_headers = true;
    let content_length = 0;
    let header_length = 0;
    process.stdin.on('data', (chunk) => {

        let content = chunk.toString('utf8');
        body += content

        if (reading_headers) {
            lsp.lspServer.log("Reading headers");
            let i = 0;
            const parts = body.split(/\r?\n/);

            while(reading_headers && i < parts.length) {
                const header = parts[i];
                if (header.startsWith("Content-Length: ")) {
                    const len = header.substring(16)
                    content_length = parseInt(len)
                    header_length += (header.length + 2)
                } 
                if (header.startsWith("Content-Type: ")) {
                    // We ignore this header for now
                    header_length += (header.length + 2)
                }
                if (header == "") {
                    reading_headers=false
                    header_length += 2
                }
                i++
            }

            body = body.substring(header_length)

            if (reading_headers) {
              lsp.lspServer.log("Malformed headers" );
                content_length = 0;
                header_length = 0;
            }
            
        } 

        if (reading_headers) {
           lsp.lspServer.log("Failed to parse this content" );
        } else {
            if (body.length >= content_length) {
                let message = body;
                if (body.length > content_length) {
                    message = body.substring(0,content_length)
                    body = body.substring(content_length)
                } else {
                    body = ''
                }

                lsp.lspServer.log("Message is " + message);
                lsp.lspServer.processmessage(message,message_count==0);
                reading_headers = true;
                content_length = 0;
                header_length = 0;
                message_count++;
            } else {
                // keep collecting bytes
                lsp.lspServer.log("Keep collecting");
            }               
           
        }

       
    });
}).then((line) => {
   console.log("LSP server finished")
})