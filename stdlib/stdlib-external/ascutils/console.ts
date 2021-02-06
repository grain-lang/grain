import { fd_write } from 'bindings/wasi'
import { calloc, free } from './grainRuntime'

export declare function debug(value: u64): void
export declare function tracepoint(n: u32): void

// Implementation of writeStringLn from as-wasi: https://github.com/jedisct1/as-wasi
// (MIT License)
export function log(ptr: u32): void {
  let iov = calloc(32)
  store<u32>(iov, ptr + 8)
  store<u32>(iov, load<u32>(ptr + 4), sizeof<usize>())
  let lf = calloc(8)
  store<u8>(lf, 10)
  store<u32>(iov, lf, sizeof<usize>() * 2)
  store<u32>(iov, 1, sizeof<usize>() * 3)
  let written_ptr = calloc(8)
  fd_write(1, iov, 2, written_ptr)
  free(written_ptr)
  free(lf)
  free(iov)
}
