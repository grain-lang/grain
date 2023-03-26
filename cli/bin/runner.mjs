import { _setMemory } from "wasi_snapshot_preview1";

async function start() {
    const cwd = new URL(`${process.cwd()}/`, 'file:');
    const { memory, _start } = await import(new URL(process.argv[2], cwd))

    _setMemory(memory);
    _start()
}

start()
