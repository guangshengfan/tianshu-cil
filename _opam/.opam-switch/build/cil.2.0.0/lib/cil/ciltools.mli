open Cil
open Cilint

val isOne: Cil.exp -> bool
val is_volatile_tp: Cil.typ -> bool
val is_volatile_vi: Cil.varinfo -> bool

type sign = Signed | Unsigned 

exception Not_an_integer

val ocaml_int_to_cil: int64 -> int -> sign -> Cil.exp

val isCompositeType: Cil.typ -> bool

val one_instruction_per_statement: Cil.file -> unit

