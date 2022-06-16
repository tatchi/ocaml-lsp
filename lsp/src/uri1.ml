open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t =
  { scheme : string
  ; path : string
  }

let path t = t.path

let backslash_to_slash =
  String.map ~f:(function
    | '\\' -> '/'
    | _ as c -> c)

let slash_to_backslash =
  String.map ~f:(function
    | '/' -> '\\'
    | _ as c -> c)

let of_path path =
  (* TODO:
     https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L315 *)
  let path =
    if String.length path = 0 then "/"
    else
      let path = if !Private.win32 then backslash_to_slash path else path in
      if path.[0] <> '/' then "/" ^ path else path
  in
  { scheme = "file"; path }

let to_path t =
  let path = t.path in
  let path =
    if String.length path = 0 then "/"
    else if String.length path < 3 then path
    else
      let c0 = path.[0] in
      let c1 = path.[1] in
      let c2 = path.[2] in
      if
        c0 = '/'
        && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z'))
        && c2 = ':'
      then
        String.make 1 (Char.lowercase_ascii c1)
        ^ String.sub path ~pos:2 ~len:(String.length path - 2)
      else path
  in
  if !Private.win32 then slash_to_backslash path else path

let of_string s =
  let re =
    Re.Perl.re "^(([^:/?#]+?):)?(//([^/?#]*))?([^?#]*)"
    |> Re.compile
  in
  let res = Re.exec re s in
  let fmt = Format.str_formatter in
  Re.Group.pp fmt res;
  Format.pp_close_box fmt ();
  Format.flush_str_formatter ()
