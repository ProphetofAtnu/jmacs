from .transport import stdio_pipe_connect
from .server import Server
from .eval import Evaluator
import asyncio
from .insp import Inspector
from .environment import Environment

async def run():
    r, w = await stdio_pipe_connect()
    server = Server(r, w)

    server.use(Inspector())
    server.use(Environment())
    server.use(Evaluator())

    await server.run()
