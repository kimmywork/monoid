import asyncio


async def waiter(event: asyncio.Event):
    print("Waiting for it ...")
    await event.wait()
    print("... got it")


async def main():

    event = asyncio.Event()

    waiter_task = asyncio.create_task(waiter(event))

    await asyncio.sleep(1)
    event.set()

    await waiter_task


asyncio.run(main())
