const repl = require('repl');
const net = require('net');


async function main() {
    const replServer = repl.start({
        useGlobal: true,
    });

    const server = net.createServer((socket) => {
        socket.on("data", (data) => {
            const str = data.toString().trim();
            replServer.completer(str, (err, res) => {
                socket.write(JSON.stringify({err, res}));
            });
        });
    });

    server.listen(46623);
}

main();
