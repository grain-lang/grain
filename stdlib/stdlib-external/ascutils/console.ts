import { fd_write } from 'bindings/wasi'
import { calloc, free } from './grainRuntime'

export declare function log(value: u32): void
export declare function debug(value: u64): void
export declare function tracepoint(n: u32): void

// Implementation of writeStringLn from as-wasi: https://github.com/jedisct1/as-wasi
// (MIT License)
export function consoleLog(s: string): void {
  let s_utf8_buf = String.UTF8.encode(s);
  let s_utf8_len: usize = s_utf8_buf.byteLength;
  // We use our own calloc/free implementations for consistency
  // [TODO] determine if it's safe to use memory.data?
  // [TODO] #2: Maybe our malloc/free should be ported to memory.data and friends?
  let iov = calloc(32);
  store<u32>(iov, changetype<usize>(s_utf8_buf));
  store<u32>(iov, s_utf8_len, sizeof<usize>());
  let lf = calloc(8);
  store<u8>(lf, 10);
  store<u32>(iov, lf, sizeof<usize>() * 2);
  store<u32>(iov, 1, sizeof<usize>() * 3);
  let written_ptr = calloc(8);
  // 1 == STDOUT
  fd_write(1, iov, 2, written_ptr);
  free(written_ptr)
  free(lf)
  free(iov)
}
