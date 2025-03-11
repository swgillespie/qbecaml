open Qbecaml

let () =
  let builder = Builder.new_builder () in
  let data = Builder.new_data builder "str" in
  Builder.add_data_item data (Il.DataItemString "%d\n");
  Builder.add_data_item data (Il.DataConstant 0);
  Builder.finish_data data;
  let func = Builder.new_function builder "main" in
  Builder.set_return_type func Il.TypeWord;
  Builder.set_linkage func Il.LinkageExport;
  let block = Builder.new_block func "start" in
  let a =
    Builder.make_add block Il.TypeLong (Il.ValueConstInt 1) (Il.ValueConstInt 2)
  in
  let b =
    Builder.make_add block Il.TypeLong (Il.ValueConstInt 3) (Il.ValueConstInt 4)
  in
  let c = Builder.make_add block Il.TypeLong a b in
  let _ =
    Builder.make_call block (Il.ValueGlobal "printf")
      [ (Il.ValueGlobal "str", Il.TypeLong); (c, Il.TypeLong) ]
  in
  Builder.make_return ~value:(Il.ValueConstInt 0) block;
  Builder.finish_block block;
  Builder.finish_function func;
  let prog = Builder.finish builder in
  print_endline (Il.string_of_program prog)
