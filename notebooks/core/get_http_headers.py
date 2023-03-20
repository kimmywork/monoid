import asyncio
import urllib.parse
import sys


async def print_http_headers(url: str) -> None:

    url = urllib.parse.urlsplit(url)

    if url.scheme == "https":
        reader, writer = await asyncio.open_connection(url.hostname, 443, ssl=True)

    else:
        reader, writer = await asyncio.open_connection(url.hostname, 80)

    query = f'HEAD {url.path or "/"} HTTP/1.0\r\n' f"Host: {url.hostname}\r\n" f"\r\n"

    writer.write(query.encode("latin-1"))

    while True:
        line = await reader.readline()
        if not line:
            break
        line = line.decode("latin-1").rstrip()
        if line:
            print(f"HTTP header> {line}")
    writer.close()


asyncio.run(print_http_headers("https://www.google.com/webhp"))
