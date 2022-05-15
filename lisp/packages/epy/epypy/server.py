from asyncio import tasks
from .endpoints import Dispatcher
from .transport import RpcCall, stdio_pipe_connect
from .transport import ErrorResponse, Response
from .world import World
from typing import List
import asyncio

from . import code


class Server:
    def __init__(
        self, read: asyncio.StreamReader, write: asyncio.StreamWriter, **kwargs
    ) -> None:
        self.reader = read
        self.writer = write
        self.pending: List[tasks.Task] = []
        self.world: World = Dispatcher.get_instance("world")

    async def write_response(self, res: Response | ErrorResponse):
        enc = self.world.encode(res)
        byt = enc.encode() + b"\n"
        self.writer.write(byt)
        await self.writer.drain()

    async def dispatch(self, msg: RpcCall):
        mid = msg.get("id")
        res = await Dispatcher.accept(msg)
        return await self.write_response(Response(id=mid, result=res))

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

    def cleanup(self, _):
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
