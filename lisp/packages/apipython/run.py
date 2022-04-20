import sys
import os
import asyncio

sys.path += os.path.dirname(os.path.realpath(__file__))

import apipython

if __name__ == '__main__':
    asyncio.run(apipython.run())
