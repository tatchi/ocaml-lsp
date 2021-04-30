open Import

let to_loc (loc : Warnings.loc) : Loc.t =
  { loc_start = loc.loc_start
  ; loc_end = loc.loc_end
  ; loc_ghost = loc.loc_ghost
  }

let cmt_sign (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Cmt_format.Implementation { Typedtree.str_type = sign; _ }
  | Cmt_format.Interface { Typedtree.sig_type = sign; _ }
  | Cmt_format.Packed (sign, _) ->
    sign
  | _ -> []

let loc_of_sig_item (signature_item : Types.signature_item) =
  match signature_item with
  | Types.Sig_value (_, descr, _) -> descr.val_loc
  | Types.Sig_type (_, descr, _, _) -> descr.type_loc
  | Types.Sig_typext (_, descr, _, _) -> descr.ext_loc
  | Types.Sig_module (_, _, descr, _, _) -> descr.md_loc
  | Types.Sig_modtype (_, descr, _) -> descr.mtd_loc
  | Types.Sig_class (_, descr, _, _) -> descr.cty_loc
  | Types.Sig_class_type (_, descr, _, _) -> descr.clty_loc

let id_of_sig_item (signature_item : Types.signature_item) =
  match signature_item with
  | Types.Sig_value (id, _, _)
  | Types.Sig_type (id, _, _, _)
  | Types.Sig_typext (id, _, _, _)
  | Types.Sig_module (id, _, _, _, _)
  | Types.Sig_modtype (id, _, _)
  | Types.Sig_class (id, _, _, _)
  | Types.Sig_class_type (id, _, _, _) ->
    id

let kind_of_sig_item (signature_item : Types.signature_item) : SymbolKind.t =
  match signature_item with
  | Types.Sig_value _ -> Function
  | Types.Sig_type _ -> String
  (* | Types.Sig_typext (_, _, Types.Text_exception, _) -> Constructor *)
  | Types.Sig_typext _ -> Constructor
  | Types.Sig_module _ -> Module
  | Types.Sig_modtype _ -> Module
  | Types.Sig_class _ -> Class
  | Types.Sig_class_type _ -> Class

let symbol_of_signature_item ~sourcefile (signature_item : Types.signature_item)
    =
  let id = id_of_sig_item signature_item in
  let loc = loc_of_sig_item signature_item in
  let name = Ident.name id in
  let kind = kind_of_sig_item signature_item in
  let range = Range.of_loc (loc |> to_loc) in
  let uri = sourcefile |> Uri.of_path in
  let location = Location.create ~uri ~range in
  SymbolInformation.create ~name ~kind ~location ()

let find_build_dir path =
  let ( / ) = Filename.concat in
  let files = Sys.readdir path in
  let build_dir = "_build" in
  let is_build folder = folder = build_dir in
  if Array.exists ~f:is_build files then
    Some (path / build_dir)
  else
    None

let project_root ?(path = Sys.getcwd ()) () =
  let ( / ) = Filename.concat in
  let home =
    try Sys.getenv "HOME" with
    | Not_found -> ""
  in
  let path =
    if Filename.is_relative path then
      Sys.getcwd () / path
    else
      path
  in
  let rec find path =
    if path = home then
      None
    else
      match find_build_dir path with
      | None ->
        let parent = Filename.dirname path in
        if path = parent then
          None
        else
          find parent
      | Some build -> Some (path, build)
  in
  match find path with
  | None -> (None, None)
  | Some (root, build) -> (Some root, Some build)

let unique_subdirs ?(skip = fun _ -> false) dir_list =
  let rec subdirs acc path =
    Array.fold_left
      ~f:(fun acc p ->
        if skip p then
          acc
        else
          let path = Filename.concat path p in
          if
            try Sys.is_directory path with
            | Sys_error _ -> false
          then
            subdirs acc path
          else
            acc)
      ~init:(path :: acc) (Sys.readdir path)
  in
  let remove_dups l =
    let rec aux = function
      | a :: (b :: _ as r) when a = b -> aux r
      | a :: r -> a :: aux r
      | [] -> []
    in
    aux (List.sort ~compare:(fun a b -> Ordering.of_int (compare a b)) l)
  in
  remove_dups (List.fold_left ~f:subdirs ~init:[] dir_list)

let read_cmt root_uri cmt_file =
  let cmt = Cmt_format.read_cmt cmt_file in
  let sourcefile =
    root_uri ^ "/" ^ Option.value ~default:"" cmt.cmt_sourcefile
  in
  let signatures = cmt_sign cmt in
  signatures |> List.map ~f:(symbol_of_signature_item ~sourcefile)

type cm_file =
  | Cmt of string
  | Cmti of string

let string_of_cm cm =
  match cm with
  | Cmt f
  | Cmti f ->
    f

module Cm_files = Map.Make (struct
  type t = string

  let compare a b = Stdlib.compare a b |> Ordering.of_int

  let to_dyn t = Dyn.String t
end)

let find_dirfiles dirs =
  let choose_file f1 f2 =
    match (f1, f2) with
    | (Cmt _ as f), _
    | _, (Cmt _ as f) ->
      f
    | (Cmti _ as f), Cmti _ -> f
  in
  let split_filename file =
    let maybe_index = String.rindex_opt file '.' in
    let len = String.length file in
    match maybe_index with
    | None -> (file, "")
    | Some index ->
      let modul = String.capitalize_ascii (String.sub file ~pos:0 ~len:index) in
      let ext =
        String.lowercase_ascii
          (String.sub file ~pos:(index + 1) ~len:(len - index - 1))
      in
      (modul, ext)
  in
  List.fold_left
    ~f:(fun files dir ->
      Sys.readdir dir
      |> Array.fold_left
           ~f:(fun acc file ->
             let modul, ext = split_filename file in
             let path = dir ^ "/" ^ file in
             match ext with
             | "cmt" -> Cm_files.set acc modul (Cmt path)
             | "cmti" -> (
               let current_file = Cm_files.find acc modul in
               let cmti_file = Cmti path in
               match current_file with
               | None -> Cm_files.set acc modul cmti_file
               | Some current_file ->
                 Cm_files.set acc modul (choose_file current_file cmti_file))
             | _ -> acc)
           ~init:files)
    ~init:Cm_files.empty dirs
  |> Cm_files.values |> List.map ~f:string_of_cm

let run ({ query; _ } : WorkspaceSymbolParams.t) (rootUri : Uri.t option) =
  let rootUri =
    Option.map rootUri ~f:Uri.to_path |> Option.value ~default:"No root uri"
  in
  let _, build_dir = project_root ~path:rootUri () in
  let build_dir = Option.value build_dir ~default:"No build dir" in
  let unique_subdirs = unique_subdirs [ build_dir ] in
  let all_files = find_dirfiles unique_subdirs in
  let load_file file =
    let maybe_index = String.rindex_opt file '.' in
    match maybe_index with
    | None -> []
    | Some i -> (
      let len = String.length file in
      let ext =
        String.lowercase_ascii (String.sub file ~pos:(i + 1) ~len:(len - i - 1))
      in
      match ext with
      | "cmt"
      | "cmti" ->
        read_cmt rootUri file
      | _ -> [])
  in
  let all_symbols = List.map ~f:load_file all_files |> List.flatten in

  match query with
  | "" -> all_symbols
  | query ->
    let reg_exp = Str.regexp_string query in
    all_symbols
    |> List.filter ~f:(fun symbol ->
           try
             let () =
               ignore
                 (Str.search_forward reg_exp SymbolInformation.(symbol.name) 0)
             in
             true
           with
           | Not_found -> false)
