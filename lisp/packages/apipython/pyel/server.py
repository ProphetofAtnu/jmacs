from asyncio import tasks
from asyncio.futures import Future

from apipython.transport import stdio_pipe_connect
from .core import ApiCore
import importlib
from .transport import ErrorResponse, Response
from .world import World
from typing import Dict, List, Any
import asyncio


class Server:
    def __init__(
        self, read: asyncio.StreamReader, write: asyncio.StreamWriter, **kwargs
    ) -> None:
        self.reader = read
        self.writer = write
        self.pending: List[tasks.Task] = []
        self.world = World()
        self.core = ApiCore(self.world)

    async def write_response(self, res: Response | ErrorResponse):
        enc = self.world.encode(res)
        byt = enc.encode() + b"\n"
        self.writer.write(byt)
        await self.writer.drain()

    async def dispatch(self, msg: Dict[str, Any]):
        mid = msg.pop("id")
        do = msg.pop("do")
        if handler := getattr(self.core, do, None):
            result = handler(**msg)
            return await self.write_response(Response(id=mid, result=result))
        raise Exception("No handler")

    async def process(self, msg):
        pmsg = dict()
        try:
            pmsg = self.world.decode(msg)
        except Exception as err:
            await self.write_response(ErrorResponse(id=-1, error=str(err)))
            return
        try:
            await self.dispatch(pmsg)
        except Exception as err:
            return await self.write_response(
                ErrorResponse(id=pmsg.get("id", -1), error=str(err))
            )

    def cleanup(self, future):
        self.pending = [p for p in self.pending if p.done()]

    async def run(self):
        while msg := await self.reader.readline():
            task = asyncio.create_task(self.process(msg))
            self.pending.append(task)
            task.add_done_callback(self.cleanup)


async def run_server():
    r, w = await stdio_pipe_connect()
    server = Server(r, w)
    await server.run()
