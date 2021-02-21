@external("GRAIN$MODULE$runtime/equal", "equal")
declare function equalExt(closure: u32, a: u32, b: u32): u32
@external("GRAIN$MODULE$runtime/equal", "GRAIN$EXPORT$equal")
declare let equalClosure: u32

// @ts-ignore: decorator
@inline
export function equal(a: u32, b: u32): u32 {
  return equalExt(equalClosure, a, b);
}
