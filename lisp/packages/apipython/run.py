import sys
import os
import asyncio

sys.path += os.path.dirname(os.path.realpath(__file__))

import pyel.server

if __name__ == '__main__':
    asyncio.run(pyel.server.run_server())
