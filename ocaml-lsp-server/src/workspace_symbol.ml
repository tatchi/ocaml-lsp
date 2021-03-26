open Import

let kind (kind : IndexTypes.kind) : SymbolKind.t =
  match kind with
  | Type -> String
  | Value -> Function
  | Exception -> Constructor
  | OpenType -> Module
  | Field _
  | Variant _ ->
    Constructor
  | Method _ -> Method
  | Module
  | ModuleType ->
    Module
  | Class
  | ClassType ->
    Class
  | Keyword -> Key

let to_loc (loc : Warnings.loc) : Loc.t =
  { loc_start = loc.loc_start
  ; loc_end = loc.loc_end
  ; loc_ghost = loc.loc_ghost
  }

let symbol (info : IndexTypes.info) =
  let loc_impl = Lazy.force_val info.loc_impl |> to_loc in
  let loc =
    if Loc.is_none loc_impl then
      Lazy.force_val info.loc_sig |> to_loc
    else
      loc_impl
  in
  let uri = loc.loc_start.pos_fname in
  let range = Range.of_loc loc in
  let location : Location.t = { uri; range } in
  let name = LibIndex.Print.path info in
  let kind = kind info.kind in
  SymbolInformation.create ~name ~kind ~location ()

let run ({ query } : WorkspaceSymbolParams.t) =
  let _, build_dir = IndexMisc.project_root () in
  let build_dir = Option.value build_dir ~default:"No build dir" in
  let unique_subdirs = LibIndex.Misc.unique_subdirs [ build_dir ] in
  let lib_index = LibIndex.load unique_subdirs in
  let items =
    LibIndex.complete
      ~filter:(fun info ->
        match info.file with
        | Cmi _ -> false
        | Cmti _
        | Cmt _ ->
          true)
      lib_index query
  in
  List.map items ~f:symbol
