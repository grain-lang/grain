export async function resolve(specifier, context, nextResolve) {
    if (specifier === "wasi_snapshot_preview1") {
        return {
            shortCircuit: true,
            url: new URL("./wasi_snapshot_preview1.mjs", import.meta.url).href,
        }
    }

    return nextResolve(specifier);
}
