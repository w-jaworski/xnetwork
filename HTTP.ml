open Lwt.Syntax
open Cohttp
open Cohttp_lwt_unix

(*
let* x = expr in body x
expr : 'a Lwt.t
x : 'a
body: 'a -> 'b Lwt.t
całość ma typ 'b Lwt.t

let+ x = expr in body x
expr : 'a Lwt.t
x : 'a
body: 'a -> 'b Lwt.t
całość ma typ 'b Lwt.t

let* x = expr in Lwt.return (body x)
jest równoważne (z pominięciem sposobu wykonywania obliczeń)
let+ x = expr in body x

Lwt_main.run : 'a Lwt.t -> 'a
Lwt.return : 'a -> 'a Lwt.t
*)

(* Wykonanie zapytania GET *)
let get uri =
  Lwt_main.run (
    let* resp, body = Client.get (Uri.of_string uri) in
    let code = Code.code_of_status (Response.status resp) in
    let+ body_content = Cohttp_lwt.Body.to_string body in
    if code = 200 then body_content
    else failwith ("Error: " ^ string_of_int code ^ " - " ^ body_content))

(*let http_get uri =
  Client.get uri >>= (fun (resp, body) ->
    let code = Code.code_of_status (Response.status resp) in
    (Cohttp_lwt.Body.to_string body) >|= (fun body_content ->
      if code = 200 then body_content
      else failwith ("Błąd API: " ^ string_of_int code ^ " - " ^ body_content)))*)

(* Wykonanie zapytania POST *)
let post uri body =
  Lwt_main.run (
    let headers = Header.init_with "Content-Type" "application/json" in
    let* resp, body = Client.post ~body:(Cohttp_lwt.Body.of_string body) ~headers (Uri.of_string uri) in
    let code = Code.code_of_status (Response.status resp) in
    let+ body_content = Cohttp_lwt.Body.to_string body in
    if code = 200 then body_content
    else failwith ("Error: " ^ string_of_int code ^ " - " ^ body_content))

(*let http_post uri body =
  let headers = Header.init_with "Content-Type" "application/json" in
  Client.post ~body:(Cohttp_lwt.Body.of_string body) ~headers uri >>= (fun (resp, body) ->
    let code = Code.code_of_status (Response.status resp) in
    (Cohttp_lwt.Body.to_string body) >|= (fun body_content ->
      if code = 200 then body_content
      else failwith ("Błąd API: " ^ string_of_int code ^ " - " ^ body_content)))      *)


(* Funkcja pomocnicza do wyświetlania paska postępu *)
let print_progress current total =
  match total with
  | Some t ->
      let percent = (float_of_int current /. float_of_int t) *. 100.0 in
      (* \r (carriage return) wraca na początek linii, nadpisując poprzedni tekst *)
      Printf.printf "\rRetrieved: %d / %d bajtów (%.2f%%)%!" current t percent
  | None ->
      Printf.printf "\rRetrieved: %d bajtów (rozmiar nieznany)%!" current

(* Zadanie monitorujące (co 0.5s) *)
let rec monitor_loop current_downloaded total_size =
  let* () = Lwt_unix.sleep 0.5 in
  print_progress !current_downloaded total_size;
  monitor_loop current_downloaded total_size

(* Zadanie pobierania *)
let download_task output_filename stream current_downloaded =
  Lwt_io.with_file ~mode:Lwt_io.Output output_filename (fun oc ->
    Lwt_stream.iter_s (fun chunk ->
      current_downloaded := !current_downloaded + (String.length chunk);
      Lwt_io.write oc chunk) stream)

(* Pobieranie małych plików *)
let download_file uri output_filename =
  Lwt_main.run (
    let* resp, body = Client.get (Uri.of_string uri) in
    let code = Code.code_of_status (Response.status resp) in
    let+ body_content = Cohttp_lwt.Body.to_string body in
    if code = 200 then Lwt_io.with_file ~mode:Lwt_io.Output output_filename (fun oc -> Lwt_io.write oc body_content)
    else failwith ("Error: " ^ string_of_int code ^ " - " ^ body_content))


(* Pobieranie dużych plików z wyświetlaniem paska postępu *)
let download_file_progress uri output_filename =
  Lwt_main.run (
    let* resp, body = Client.get (Uri.of_string uri) in
    let status = Response.status resp in
    if status <> `OK then failwith ("Error: " ^ Code.string_of_status status) else (
    let headers = Response.headers resp in
    let total_size =
      match Cohttp.Header.get headers "Content-Length" with
      | Some len -> int_of_string_opt len
      | None -> None in
    let stream = Cohttp_lwt.Body.to_stream body in
    let current_downloaded = ref 0 in
    let* () = Lwt.pick [download_task output_filename stream current_downloaded; monitor_loop current_downloaded total_size] in
    print_progress !current_downloaded total_size;
    print_newline ();
    Lwt.return ()))

(*(* Pobieranie listy plików *)
let download_files_list files_list =
  (* Konfiguracja puli. Używamy unit, bo nie potrzebujemy specjalnych zasobów *)
  let pool = Lwt_pool.create 32 (fun () -> Lwt.return ()) in
  Lwt_main.run (
    (* Lwt_list.iter_p próbuje uruchomić wszystko "na raz" *)
    Lwt_list.iter_p (fun (uri, filename) ->
      (* ...ale Lwt_pool.use wstrzymuje wykonanie, jeśli zajętych jest 32 sloty *)
      Lwt_pool.use pool (fun () ->
        print_endline ("Start: " ^ filename);
        let+ () = Lwt_main.run (download_file_lwt uri filename) in
        print_endline ("Finish: " ^ filename)
        (*download_file_lwt uri filename; Lwt.return ()*))
    ) files_list)

(* 4. Główna funkcja (Main) *)
let () =
  (* Generujemy sztuczną listę 1000 plików do pobrania.
     Dla testu używamy tego samego URL, ale zapisujemy do różnych plików. *)
  let test_url = "http://speedtest.tele2.net/1MB.zip" in

  let tasks =
    List.init 100 (fun i ->
      (test_url, Printf.sprintf "file_%04d.zip" i)
    )
  in
  download_files_list tasks*)

let download_file_catch uri output_filename =
  (* Lwt.catch zapobiega wywaleniu całego programu, gdy jeden plik się nie uda *)
  Lwt.catch (fun () ->
    let* (resp, body) = Client.get (Uri.of_string uri) in
    match Response.status resp with
    | `OK ->
        (* Otwieramy plik do zapisu *)
        let+ () = Lwt_io.with_file ~mode:Lwt_io.Output output_filename (fun oc ->
          (* Konwertujemy body na strumień i zapisujemy kawałki prosto do pliku *)
          let stream = Cohttp_lwt.Body.to_stream body in
          Lwt_stream.iter_s (fun chunk -> Lwt_io.write oc chunk) stream) in
        Printf.printf "Pobrano: %s\n%!" uri
    | status ->
        Printf.printf "Błąd pobierania %s: %s\n%!"
          uri (Code.string_of_status status);
        Lwt.return ()
  ) (fun exn ->
    Printf.printf "Wyjątek przy %s: %s\n%!" uri (Printexc.to_string exn);
    Lwt.return ()
  )

(* Pobieranie listy plików *)
let download_files_list files_list =
  (* Pula o rozmiarze 32. Używamy unit, bo nie potrzebujemy specjalnych zasobów *)
  let pool = Lwt_pool.create 32 (fun () -> Lwt.return ()) in
  (* Lwt_list.iter_p próbuje uruchomić wszystko "na raz" *)
  Lwt_main.run (
    Lwt_list.iter_p (fun (url_str, filename) ->
      (* ...ale Lwt_pool.use wstrzymuje wykonanie, jeśli zajętych jest 32 sloty *)
      Lwt_pool.use pool (fun () -> download_file_catch url_str filename)
    ) files_list)

