open Brr

let init () =
  let source_path =
    Jv.to_option Fun.id @@ Jv.get (Window.to_jv G.window) "sourcePath"
  in
  match source_path with
  | None -> ()
  | Some jv ->
    let path = Jv.to_string jv in
    let url = Format.asprintf "vscode://file/%s" path in
    Console.log [url];
    ignore
    @@ Ev.(
         listen keydown
           (fun e ->
             let ev = as_type e in
             if Keyboard.ctrl_key ev && Keyboard.key ev = Jstr.v "e" then begin
               prevent_default e;
               Jv.set (Window.to_jv G.window) "location.href" (Jv.of_string url)
             end)
           (Document.as_target G.document))
