import asyncio
from json.decoder import JSONDecoder 
import sys
from typing import Optional, Any, List, Dict, Tuple, TypedDict

async def stdio_pipe_connect(): 
    loop = asyncio.get_event_loop()
    reader = asyncio.StreamReader()
    proto = asyncio.StreamReaderProtocol(reader)
    await loop.connect_read_pipe(lambda: proto, sys.stdin)
    w_transport, w_protocol = await loop.connect_write_pipe(asyncio.streams.FlowControlMixin, sys.stdout)
    writer = asyncio.StreamWriter(w_transport, w_protocol, reader, loop)
    return reader, writer

class CallMessage(TypedDict):
    id: int
    target: Optional[Any]
    method: str
    args: Optional[List[Any]]
    kwargs: Optional[Dict[str, Any]]

class GetMessage(TypedDict):
    id: int
    target: Optional[Any]
    get: str

class SetMessage(TypedDict):
    id: int
    target: Optional[Any]
    get: str
    value: Any

class Response(TypedDict):
    id: int
    result: Any
    
class ErrorResponse(TypedDict):
    id: int
    error: Any
    
