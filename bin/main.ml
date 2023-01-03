module Driver = Luno.Driver

let input_file = ref ""
let usage_msg = "luno <file>"
let handler_fun fname = input_file := fname

let () =
  Arg.parse [] handler_fun usage_msg;
  let gen = Driver.drive !input_file in
  gen