open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t = Uri_lexer.t =
  { scheme : string option
  ; authority : string
  ; path : string
  }

let t_of_yojson json = Json.Conv.string_of_yojson json |> Uri_lexer.of_string

let to_string { scheme; authority; path } =
  let b = Buffer.create 64 in
  scheme
  |> Option.iter (fun s ->
         Buffer.add_string b s;
         Buffer.add_char b ':');
  Buffer.add_string b "//";
  Buffer.add_string b authority;
  if not (String.is_prefix path ~prefix:"/") then Buffer.add_char b '/';
  Buffer.add_string b
    (path |> Uri.pct_encode
    |> String.replace_all ~pattern:"%2F" ~with_:"/"
    (* https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L453 *)
    |> String.replace_all ~pattern:":" ~with_:"%3A"
    |> String.replace_all ~pattern:"?" ~with_:"%3F"
    |> String.replace_all ~pattern:"#" ~with_:"%23"
    |> String.replace_all ~pattern:"[" ~with_:"%5B"
    |> String.replace_all ~pattern:"]" ~with_:"%5D"
    |> String.replace_all ~pattern:"@" ~with_:"%40"
    |> String.replace_all ~pattern:"!" ~with_:"%21"
    |> String.replace_all ~pattern:"$" ~with_:"%24"
    |> String.replace_all ~pattern:"&" ~with_:"%26"
    |> String.replace_all ~pattern:"'" ~with_:"%27"
    |> String.replace_all ~pattern:"(" ~with_:"%28"
    |> String.replace_all ~pattern:")" ~with_:"%29"
    |> String.replace_all ~pattern:"*" ~with_:"%2A"
    |> String.replace_all ~pattern:"+" ~with_:"%2B"
    |> String.replace_all ~pattern:"," ~with_:"%2C"
    |> String.replace_all ~pattern:";" ~with_:"%3B"
    |> String.replace_all ~pattern:"=" ~with_:"%3D"
    |> String.replace_all ~pattern:" " ~with_:"%20");
  Buffer.contents b

let yojson_of_t t = `String (to_string t)

let equal = ( = )

let compare (x : t) (y : t) = Stdlib.compare x y

let hash = Hashtbl.hash

let to_dyn { scheme; authority; path } =
  let open Dyn in
  record
    [ ("scheme", (option string) scheme)
    ; ("authority", string authority)
    ; ("path", string path)
    ]

let to_path t =
  let path =
    t.path
    (* |> String.replace_all ~pattern:"\\" ~with_:"/" |> String.replace_all
       ~pattern:"%5C" ~with_:"/" |> String.replace_all ~pattern:"%3A" ~with_:":"
       |> String.replace_all ~pattern:"%20" ~with_:" " |> String.replace_all
       ~pattern:"%3D" ~with_:"=" |> String.replace_all ~pattern:"%3F"
       ~with_:"?" *)
  in
  path

let of_path (path : string) =
  let path = Uri_lexer.escape_path path in
  { path; scheme = Some "file"; authority = "" }
