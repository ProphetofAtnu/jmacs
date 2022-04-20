import asyncio 
import sys
from typing import Optional, Any, List, Dict, Tuple

async def stdio_pipe_connect(): 
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    proto = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: proto, sys.stdin)
    w_transport, w_protocol = await loop.connect_write_pipe(asyncio.streams.FlowControlMixin, sys.stdout)
    writer = asyncio.StreamWriter(w_transport, w_protocol, reader, loop)
    return reader, writer

class Message:
    method: str
    id: Optional[int]
    target: Optional[Any]
    argv: Optional[List[Any]]
    argk: Optional[Dict[str, Any]]

    def __init__(self, **kwargs):
        self.method = kwargs.get('method', kwargs.get('m'))
        self.id = kwargs.get('id', kwargs.get('i', None))
        self.target = kwargs.get('target', kwargs.get('t', None))
        self.argv = kwargs.get('args', kwargs.get('v', None))
        self.argk = kwargs.get('kwargs', kwargs.get('k', None))

class Response:
    id: Optional[int]
    method: Optional[str]
    target: Optional[int]
    error: Optional[Tuple[Any, ...]]
    result: Optional[Any]

    def __init__(self, payload: Any, 
            method: Optional[str] = None,
            target: Optional[int] = None,
            id: Optional[int] = None) -> None:
        self.id = id
        self.error = None
        self.result = None
        self.method = method
        self.target = target
        if isinstance(payload, Exception):
            self.error = payload.args
        else:
            self.result = payload

    def to_dict(self):
        d = {}
        d['id'] = self.id
        d['to'] = self.method
        if self.target:
            d['target'] = self.target
        if self.error:
            d['error'] = self.error
        else:
            d['result'] = self.result
        return d

