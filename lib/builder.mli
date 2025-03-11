type t
type func
type block
type data

val new_builder : unit -> t
val finish : t -> Il.program
val new_data : t -> string -> data
val set_data_linkage : data -> Il.linkage -> unit
val add_data_item : data -> Il.data_item -> unit
val finish_data : data -> unit
val new_function : t -> string -> func
val set_linkage : func -> Il.linkage -> unit
val set_return_type : func -> Il.ty -> unit
val finish_function : func -> unit
val new_block : func -> string -> block
val finish_block : block -> unit

val make_call :
  ?name:string ->
  ?ret:Il.ty ->
  block ->
  Il.value ->
  (Il.value * Il.ty) list ->
  Il.value

val make_add :
  ?name:string -> block -> Il.ty -> Il.value -> Il.value -> Il.value

val make_return : ?value:Il.value -> block -> unit
