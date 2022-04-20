import asyncio
import importlib
import json
from typing import Awaitable, Callable, Any, Dict
from .transport import Message, Response
from .ref import Registry
from .helpers import HandlerFunc, Plugin


class Server:
    def __init__(self, read: asyncio.StreamReader, write: asyncio.StreamWriter, **kwargs) -> None:
        self.reader = read
        self.writer = write
        self._handler_base: Dict[str,HandlerFunc] = kwargs
        self._reg = Registry()
        self._setup_extension_handlers()
        self.use(self._reg)


    def _setup_extension_handlers(self):
        self._handler_base['help'] = self._help

    def use(self, plugin: Plugin):
        mems = plugin.provides()
        for (n, m) in mems:
            if callable(m):
                self._handler_base[n] = m

    async def _help(self):
        return [*self._handler_base.keys()]

    def _validate(self, msg: bytes):
        try: 
            return Message(**json.loads(msg, object_hook=self._reg.decode_hook))
        except Exception as e:
            return Response(e)

    async def write_response(self, res: Response):
        # enc = json.dumps(res.to_dict(), object)
        enc = self._reg.encode(res.to_dict())
        byt = enc.encode() + b'\n'
        # print(byt)
        self.writer.write(byt)
        await self.writer.drain()

    async def _ref_call(self, mth: str, target: Any, *argv, **kwargs):
        method = getattr(target, mth, None)
        if callable(method):
            result = method(*argv, **kwargs)
            if asyncio.iscoroutine(result):
                result = await result
            return result
        elif method:
            return method
        

    async def call(self, msg: Message):
        mth = msg.method
        argv = msg.argv or []
        argk = msg.argk or {}
        try: 
            if tgt := msg.target:
                result = await self._ref_call(mth, tgt, *argv, **argk)
                await self.write_response(Response(result, method=mth, id=msg.id))
            if hndlr := self._handler_base.get(mth, None):
                result = hndlr(*argv, **argk)
                if asyncio.iscoroutine(result):
                    result = await result
                await self.write_response(Response(result, method = mth, id=msg.id))
            else: 
                await self.write_response(
                        Response(Exception("no method handler"), 
                            method = mth, 
                            id=msg.id))
        except Exception as e:
            await self.write_response(
                    Response(e,
                        method = mth, 
                        id=msg.id))
            

    async def process(self, msg):
        msg_v = self._validate(msg)
        if isinstance(msg_v, Response):
            await self.write_response(msg_v)
            return
        await self.call(msg_v)

    async def run(self):
        while msg := await self.reader.readline():
            await self.process(msg)

