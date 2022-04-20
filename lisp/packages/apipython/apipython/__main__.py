from .transport import stdio_pipe_connect
from .server import Server
import asyncio
from .insp import Inspector

async def main():
    r, w = await stdio_pipe_connect()
    server = Server(r, w)

    server.use(Inspector())

    await server.run()

if __name__ == '__main__':
    asyncio.run(main())
