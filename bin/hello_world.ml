open Qbecaml

let () =
  let builder = Builder.new_builder () in
  let data = Builder.new_data builder "str" in
  Builder.add_data_item data (Il.DataItemString "hello, world!");
  Builder.add_data_item data (Il.DataConstant 0);
  Builder.finish_data data;
  let func = Builder.new_function builder "main" in
  Builder.set_return_type func Il.TypeWord;
  Builder.set_linkage func Il.LinkageExport;
  let block = Builder.new_block func "start" in
  let _ =
    Builder.make_call block (Il.ValueGlobal "puts")
      [ (Il.ValueGlobal "str", Il.TypeLong) ]
  in
  Builder.make_return ~value:(Il.ValueConstInt 0) block;
  Builder.finish_block block;
  Builder.finish_function func;
  let prog = Builder.finish builder in
  print_endline (Il.string_of_program prog)
