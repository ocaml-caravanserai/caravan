(* A CA and a server certificate in memory, for development only. *)

type t =
  {ca: X509.Certificate.t; leaf: X509.Certificate.t; key: X509.Private_key.t}

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let dn cn =
  [ X509.Distinguished_name.(
      Relative_distinguished_name.singleton (CN (Common_name.v cn)) ) ]

let span_of_days n = Ptime.Span.of_int_s (n * 86400)

(* [sign ...] signs a certificate for [subject] with [ca_key]. *)
let sign ~time ~extensions ~ca_key ~ca_dn key subject =
  let valid_from = Option.get (Ptime.sub_span time (span_of_days 1))
  and valid_until = Option.get (Ptime.add_span time (span_of_days 30)) in
  match
    X509.Signing_request.sign
      (ok (X509.Signing_request.create subject key))
      ~valid_from ~valid_until ~extensions ca_key ca_dn
  with
  | Ok cert ->
      cert
  | Error e ->
      failwith (Format.asprintf "%a" X509.Validation.pp_signature_error e)

(* The subject alternative names: an IP address goes in an IP entry, each other
   name in a DNS entry. *)
let alt_names names =
  let ips, dns =
    List.partition_map
      (fun n ->
        match Ipaddr.of_string n with
        | Ok ip ->
            Left (Ipaddr.to_octets ip)
        | Error _ ->
            Right n )
      names
  in
  let add_some kind values set =
    if values = [] then set else X509.General_name.add kind values set
  in
  X509.General_name.(empty |> add_some DNS dns |> add_some IP ips)

let make ~time ~names =
  if names = [] then invalid_arg "Dev_cert.make: no names" ;
  let ca_key = X509.Private_key.generate `P256 in
  let ca_dn = dn "caravan development CA" in
  let ca_id = X509.Public_key.id (X509.Private_key.public ca_key) in
  (* The path length 0: the CA signs no other CA. *)
  let ca =
    sign ~time ~ca_key ~ca_dn
      ~extensions:
        X509.Extension.(
          empty
          |> add Basic_constraints (true, (true, Some 0))
          |> add Key_usage (true, [`Key_cert_sign])
          |> add Subject_key_id (false, ca_id) )
      ca_key ca_dn
  in
  let key = X509.Private_key.generate `P256 in
  let leaf =
    sign ~time ~ca_key ~ca_dn
      ~extensions:
        X509.Extension.(
          empty
          |> add Subject_alt_name (false, alt_names names)
          |> add Ext_key_usage (false, [`Server_auth])
          |> add Basic_constraints (true, (false, None))
          |> add Authority_key_id
               (false, (Some ca_id, X509.General_name.empty, None)) )
      key
      (dn (List.hd names))
  in
  {ca; leaf; key}

let ca t = t.ca

let write_ca t path =
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc (X509.Certificate.encode_pem t.ca) )

let server_config ?(alpn = ["http/1.1"]) t =
  ok
    (Tls.Config.server
       ~certificates:(`Single ([t.leaf], t.key))
       ~alpn_protocols:alpn () )
