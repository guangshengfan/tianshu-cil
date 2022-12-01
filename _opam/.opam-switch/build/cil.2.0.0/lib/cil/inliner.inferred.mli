module E = Errormsg
module H = Hashtbl
module IH = Inthash
module A = Alpha
val doInline : bool ref
val only_inline_main : bool ref
val main_entry : string ref
val debug : bool
val funMapVar : (string, string list) H.t
val funMapLoc : (string, int list) H.t
val nameTypMap : (string, Cil.typ) H.t
val funNoArg : bool ref
val funNoLval : bool ref
val singleLineMultiFunCall : bool ref
val funEnterPos : (string, int) H.t
val funExitPos : (string, int) H.t
val funCallTypeEnter : (string, int list) H.t
val funCallTypeExit : (string, int list) H.t
exception Recursion
class copyBodyVisitor :
  Cil.fundec ->
  Cil.varinfo ->
  (Cil.varinfo -> Cil.varinfo) ->
  Cil.varinfo option ->
  (string -> string) ->
  Cil.stmt ->
  object
    val argid : int ref
    val patches : Cil.stmt list ref
    val stmtmap : Cil.stmt IH.t
    method queueInstr : Cil.instr list -> unit
    method unqueueInstr : unit -> Cil.instr list
    method vattr : Cil.attribute -> Cil.attribute list Cil.visitAction
    method vattrparam : Cil.attrparam -> Cil.attrparam Cil.visitAction
    method vblock : Cil.block -> Cil.block Cil.visitAction
    method vexpr : Cil.exp -> Cil.exp Cil.visitAction
    method vfunc : Cil.fundec -> Cil.fundec Cil.visitAction
    method vglob : Cil.global -> Cil.global list Cil.visitAction
    method vinit :
      Cil.varinfo -> Cil.offset -> Cil.init -> Cil.init Cil.visitAction
    method vinitoffs : Cil.offset -> Cil.offset Cil.visitAction
    method vinst : Cil.instr -> Cil.instr list Cil.visitAction
    method vlval : Cil.lval -> Cil.lval Cil.visitAction
    method voffs : Cil.offset -> Cil.offset Cil.visitAction
    method vstmt : Cil.stmt -> Cil.stmt Cil.visitAction
    method vtype : Cil.typ -> Cil.typ Cil.visitAction
    method vvdec : Cil.varinfo -> Cil.varinfo Cil.visitAction
    method vvrbl : Cil.varinfo -> Cil.varinfo Cil.visitAction
  end
val varListTemp : string list ref
val replaceStatement :
  Cil.fundec ->
  (Cil.varinfo -> Cil.fundec option) ->
  (string -> string) ->
  bool ref -> Cil.fundec IH.t -> Cil.varinfo IH.t -> Cil.stmt -> Cil.stmt
val doFunction :
  Cil.fundec ->
  (Cil.varinfo -> Cil.fundec option) ->
  bool ref -> Cil.fundec IH.t -> Cil.varinfo IH.t -> unit
val doFile : (Cil.varinfo -> Cil.fundec option) -> Cil.file -> unit
val toinline : string list ref
val doit : Cil.file -> unit
val feature : Feature.t
