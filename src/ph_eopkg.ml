open Unix
open Printf

open Utils
open Package_handler

let eopkg_detect () =
  Config.eopkg <> "no" && Config.fakeroot <> "no" &&
  (Os_release.get_id() = "solus" ||
   ((stat "/etc/solus-release").st_kind = S_REG))

let settings = ref no_settings

let eopkg_init s = settings := s

type eopkg_t = {
  name : string;
  version : string;
  release : string;
  arch : string;
}

let eopkg_of_pkg, pkg_of_eopkg = get_memo_functions ()

let eopkg_packages = Hashtbl.create 13

let get_value key r = 
  let len = String.length key in
  let i = String.index key ':' in
  r := String.sub key (i+2) (len-(i+2))

let get_value_of_key key =
  let r = ref "" in let _ = get_value key r in
  if !r = "" then error "eopkg: could not key value of %s" key
  else !r

let eopkg_package_of_string str =
  let cmd = sprintf "%s info %s | sed -n '/Installed package/,/Architecture/p'" Config.eopkg (quote str) in
  let lines = run_command_get_lines cmd in

  let nvr = ref "" and arch = ref "" in
  List.iter (
    fun line ->
      if string_prefix "Name " line then get_value line nvr
      else if string_prefix "Architecture " line then get_value line arch
  ) lines;

  let nvr = !nvr and arch = !arch in
  if nvr = "" || arch = "" then
    error "eopkg: Name/Architecture field missing in output of %s" cmd;

  let name, version, release =
    try
      let n, v, r =
        match string_split "," nvr with
        | [name; ver; rel] -> name, get_value_of_key ver, get_value_of_key rel
        | _ -> assert false in
      n,v,r
    with
      Failure "int_of_string" ->
      failwith ("eopkg: Failed to parse Name/version/release field with " ^ nvr) in

  let archi =
    try
      let a =
        match string_split "," arch with
        | [value; _] -> value
        | _ -> assert false in
      a
    with
      Failure "int_of_string" ->
      failwith ("eopkg: Failed to parse Architecture field with " ^ arch) in
  {
    name = name;
    version = version;
    release = release;
    arch = archi
  }

let eopkg_package_to_string pkg =
  let eopkg = eopkg_of_pkg pkg in
  sprintf "%s_%s_%s" eopkg.name eopkg.version eopkg.arch

let eopkg_get_package_database_mtime () =
  (lstat "/var/lib/eopkg/info/files.db" (* XXX? *) ).st_mtime


