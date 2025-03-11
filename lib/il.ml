type linkage = LinkageExport | LinkageThread
type data_item = DataItemString of string | DataConstant of int

type data = {
  data_linkage : linkage option;
  data_name : string;
  data_items : data_item list;
}

type value =
  | ValueTemp of string
  | ValueGlobal of string
  | ValueConstInt of int
  | ValueConstSingleFloat of float
  | ValueConstDoubleFloat of float

type jump =
  | Jump of string
  | JumpNonZero of value * string * string
  | JumpReturn of value option
  | JumpHalt

type ty =
  | TypeWord
  | TypeLong
  | TypeSingle
  | TypeDouble
  | TypeSignedByte
  | TypeUnsignedByte
  | TypeSignedHalfword
  | TypeUnsignedHalfword

type instruction_call = {
  instruction_call_retval : (string * ty) option;
  instruction_call_value : value;
  instruction_call_args : (value * ty) list;
}

type instruction =
  | InstructionCall of instruction_call
  | InstructionAdd of string * ty * value * value

type block = {
  block_label : string;
  block_instructions : instruction list;
  block_terminator : jump option;
}

type func = {
  function_name : string;
  function_linkage : linkage option;
  function_abity : ty option;
  function_body : block list;
  function_parameters : (string * ty) list;
}

type item = ItemData of data | ItemFunc of func
type program = item list

let string_of_value = function
  | ValueTemp s -> "%" ^ s
  | ValueGlobal s -> "$" ^ s
  | ValueConstInt i -> string_of_int i
  | ValueConstSingleFloat f -> Printf.sprintf "s_%f" f
  | ValueConstDoubleFloat f -> Printf.sprintf "d_%f" f

let string_of_ty = function
  | TypeWord -> "w"
  | TypeLong -> "l"
  | TypeSingle -> "s"
  | TypeDouble -> "d"
  | TypeSignedByte -> "sb"
  | TypeUnsignedByte -> "ub"
  | TypeSignedHalfword -> "sh"
  | TypeUnsignedHalfword -> "uh"

let string_of_instruction = function
  | InstructionCall
      { instruction_call_retval; instruction_call_value; instruction_call_args }
    -> (
      let args =
        String.concat ", "
          (List.map
             (fun (arg, ty) ->
               Printf.sprintf "%s %s" (string_of_ty ty) (string_of_value arg))
             instruction_call_args)
      in
      let v = string_of_value instruction_call_value in
      match instruction_call_retval with
      | Some (retval, ty) ->
          Printf.sprintf "%%%s =%s call %s(%s)" retval (string_of_ty ty) v args
      | None -> Printf.sprintf "call %s(%s)" v args)
  | InstructionAdd (name, ty, v1, v2) ->
      Printf.sprintf "%%%s =%s add %s, %s" name (string_of_ty ty)
        (string_of_value v1) (string_of_value v2)

let string_of_jump = function
  | Jump label -> Printf.sprintf "jmp @%s" label
  | JumpNonZero (v, label1, label2) ->
      Printf.sprintf "jnz %s @%s, @%s" (string_of_value v) label1 label2
  | JumpReturn None -> "ret"
  | JumpReturn (Some v) -> Printf.sprintf "ret %s" (string_of_value v)
  | JumpHalt -> "hlt"

let string_of_block blk =
  let { block_label; block_instructions; block_terminator } = blk in
  let instr_lines =
    List.map
      (fun instr -> Printf.sprintf "  %s" (string_of_instruction instr))
      block_instructions
  in
  let jump_line =
    match block_terminator with
    | Some j -> [ Printf.sprintf "  %s" (string_of_jump j) ]
    | None -> []
  in
  let name_line = [ Printf.sprintf "@%s" block_label ] in
  let lines = List.flatten [ name_line; instr_lines; jump_line ] in
  String.concat "\n" lines

let string_of_function func =
  let {
    function_name;
    function_linkage;
    function_abity;
    function_body;
    function_parameters;
  } =
    func
  in

  let linkage =
    match function_linkage with
    | Some LinkageExport -> "export "
    | Some LinkageThread -> "thread "
    | None -> ""
  in
  let params =
    String.concat ", "
      (List.map
         (fun (name, ty) -> Printf.sprintf "%s %s" (string_of_ty ty) name)
         function_parameters)
  in
  let abity =
    match function_abity with
    | Some ty -> Printf.sprintf " %s " (string_of_ty ty)
    | None -> " "
  in
  let def_line =
    Printf.sprintf "%sfunction%s$%s(%s) {\n" linkage abity function_name params
  in
  let body_lines =
    String.concat "\n" (List.map string_of_block function_body)
  in
  def_line ^ body_lines ^ "\n}"

let string_of_data data =
  let string_of_data_item = function
    | DataItemString s -> Printf.sprintf "b \"%s\"" (String.escaped s)
    | DataConstant int -> Printf.sprintf "b %d" int
  in
  let { data_name; data_items; _ } = data in
  let items = String.concat ", " (List.map string_of_data_item data_items) in
  Printf.sprintf "data $%s = { %s }" data_name items

let string_of_item = function
  | ItemData data -> string_of_data data
  | ItemFunc func -> string_of_function func

let string_of_program program =
  String.concat "\n\n" (List.map string_of_item program)

let%expect_test "data_hello_world" =
  let data =
    {
      data_linkage = None;
      data_name = "hello";
      data_items = [ DataItemString "Hello, world!"; DataConstant 0 ];
    }
  in
  print_endline (string_of_data data);
  [%expect {| data $hello = { b "Hello, world!", b 0 } |}]

let%expect_test "empty_block" =
  let block =
    { block_label = "start"; block_instructions = []; block_terminator = None }
  in
  print_endline (string_of_block block);
  [%expect {| @start |}]

let%expect_test "hello_world_func" =
  let func =
    {
      function_name = "main";
      function_linkage = Some LinkageExport;
      function_abity = Some TypeWord;
      function_parameters = [];
      function_body =
        [
          {
            block_label = "start";
            block_instructions =
              [
                InstructionCall
                  {
                    instruction_call_retval = Some ("r", TypeWord);
                    instruction_call_value = ValueGlobal "puts";
                    instruction_call_args = [ (ValueGlobal "str", TypeLong) ];
                  };
              ];
            block_terminator = Some (JumpReturn (Some (ValueConstInt 0)));
          };
        ];
    }
  in

  print_endline (string_of_function func);
  [%expect
    {|
    export function w $main() {
    @start
      %r =w call $puts(l $str)
      ret 0
    }
    |}]
