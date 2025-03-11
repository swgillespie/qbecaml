type t = { mutable builder_items : Il.item list }

and func = {
  func_builder : t;
  func_name : string;
  mutable func_linkage : Il.linkage option;
  mutable func_return_type : Il.ty option;
  mutable func_finished_blocks : Il.block list;
  mutable func_local_index : int;
}

and block = {
  block_builder : func;
  block_name : string;
  mutable block_terminator : Il.jump option;
  mutable block_instructions : Il.instruction list;
}

and data = {
  data_builder : t;
  data_name : string;
  mutable data_linkage : Il.linkage option;
  mutable data_items : Il.data_item list;
}

let new_builder () = { builder_items = [] }
let finish builder = List.rev builder.builder_items

let new_data builder name =
  {
    data_builder = builder;
    data_name = name;
    data_linkage = None;
    data_items = [];
  }

let set_data_linkage data linkage = data.data_linkage <- Some linkage
let add_data_item data item = data.data_items <- item :: data.data_items

let finish_data data =
  let data_item =
    {
      Il.data_name = data.data_name;
      Il.data_linkage = data.data_linkage;
      Il.data_items = List.rev data.data_items;
    }
  in
  data.data_builder.builder_items <-
    Il.ItemData data_item :: data.data_builder.builder_items;
  ()

let new_function builder name =
  {
    func_builder = builder;
    func_name = name;
    func_linkage = None;
    func_return_type = None;
    func_finished_blocks = [];
    func_local_index = 0;
  }

let set_linkage fbuilder linkage = fbuilder.func_linkage <- Some linkage
let set_return_type fbuilder ty = fbuilder.func_return_type <- Some ty

let new_local_name fbuilder =
  let index = fbuilder.func_local_index in
  fbuilder.func_local_index <- index + 1;
  "r" ^ string_of_int index

let finish_function fbuilder =
  let func =
    {
      Il.function_name = fbuilder.func_name;
      Il.function_linkage = fbuilder.func_linkage;
      Il.function_abity = fbuilder.func_return_type;
      Il.function_body = List.rev fbuilder.func_finished_blocks;
      Il.function_parameters = [];
    }
  in
  fbuilder.func_builder.builder_items <-
    Il.ItemFunc func :: fbuilder.func_builder.builder_items;
  ()

let new_block fbuilder name =
  {
    block_builder = fbuilder;
    block_name = name;
    block_terminator = None;
    block_instructions = [];
  }

let finish_block block =
  if block.block_terminator == None then failwith "block is not terminated";
  let finished_block =
    {
      Il.block_label = block.block_name;
      Il.block_terminator = block.block_terminator;
      Il.block_instructions = List.rev block.block_instructions;
    }
  in
  block.block_builder.func_finished_blocks <-
    finished_block :: block.block_builder.func_finished_blocks

let make_call ?name:name_opt ?ret:ret_opt block callee args =
  let ident_name =
    match name_opt with
    | Some name -> name
    | None -> new_local_name block.block_builder
  in
  let call =
    {
      Il.instruction_call_retval =
        Option.map (fun ty -> (ident_name, ty)) ret_opt;
      Il.instruction_call_value = callee;
      Il.instruction_call_args = args;
    }
  in
  block.block_instructions <-
    Il.InstructionCall call :: block.block_instructions;
  Il.ValueTemp ident_name

let make_add ?name:name_opt block ty v1 v2 =
  let ident_name =
    match name_opt with
    | Some name -> name
    | None -> new_local_name block.block_builder
  in
  let add = Il.InstructionAdd (ident_name, ty, v1, v2) in
  block.block_instructions <- add :: block.block_instructions;
  Il.ValueTemp ident_name

let make_return ?value:value_opt block =
  if block.block_terminator <> None then
    failwith "block already has a terminator";
  block.block_terminator <- Some (Il.JumpReturn value_opt);
  ()

let%expect_test "return_zero" =
  let builder = new_builder () in
  let func = new_function builder "returns_zero" in
  set_linkage func Il.LinkageExport;
  set_return_type func Il.TypeWord;
  let block = new_block func "start" in
  make_return ~value:(Il.ValueConstInt 0) block;
  finish_block block;
  finish_function func;
  let program = finish builder in
  print_endline (Il.string_of_program program);
  [%expect
    {|
    export function w $returns_zero() {
    @start
      ret 0
    } |}]

let%expect_test "hello_world" =
  let builder = new_builder () in
  let str = new_data builder "str" in
  add_data_item str (Il.DataItemString "Hello, world!");
  add_data_item str (Il.DataConstant 0);
  finish_data str;

  let func = new_function builder "main" in
  set_linkage func Il.LinkageExport;
  set_return_type func Il.TypeWord;
  let block = new_block func "start" in
  let _ =
    make_call ~ret:Il.TypeWord block (Il.ValueGlobal "puts")
      [ (Il.ValueGlobal "str", Il.TypeLong) ]
  in
  make_return ~value:(Il.ValueConstInt 0) block;
  finish_block block;
  finish_function func;
  let program = finish builder in
  print_endline (Il.string_of_program program);
  [%expect
    {|
    data $str = { b "Hello, world!", b 0 }

    export function w $main() {
    @start
      %r0 =w call $puts(l $str)
      ret 0
    }
    |}]
