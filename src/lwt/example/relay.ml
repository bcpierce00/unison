
(* Usage: relay <listening_port> <dest_port> *)

(* This program waits for a connection on <listening_port>. It then
   connect to <dest_port> and relay everything it receives in either
   side to the other side.  It exists when either side closes the
   connection. *)

let listening_port = int_of_string Sys.argv.(1)
let dest_port = int_of_string Sys.argv.(2)

open Lwt

let rec really_write out_ch buffer pos len =
  Lwt_unix.write out_ch buffer pos len >>= (fun len' ->
  if len = len' then return () else
  really_write out_ch buffer (pos + len') (len - len'))

let relay in_ch out_ch =
  let rec relay_rec previous_write =
    let buffer = String.create 8192 in
    (* Read some data from the input socket *)
    Lwt_unix.read in_ch buffer 0 8192 >>= (fun len ->
    (* If we read nothing, this means that the connection has been
       closed.  In this case, we stop relaying. *)
    if len = 0 then return () else begin
      (* Otherwise, we write the data to the ouput socket *)
      let write =
        (* First wait for the previous write to terminate *)
        previous_write >>= (fun () ->
        (* Then write the contents of the buffer *)
        really_write out_ch buffer 0 len)
      in
      relay_rec write
    end)
  in
  relay_rec (return ())

let new_socket () = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
let local_addr num = Unix.ADDR_INET (Unix.inet_addr_any, num)

let _ =
  Lwt_unix.run
    ((* Initialize the listening address *)
     new_socket () >>= (fun listening_socket ->
     Unix.setsockopt listening_socket Unix.SO_REUSEADDR true;
     Unix.bind listening_socket (local_addr listening_port);
     Unix.listen listening_socket 1;
     (* Wait for a connection *)
     Lwt_unix.accept listening_socket >>= (fun (inp, _) ->
     (* Connect to the destination port *)
     new_socket () >>= (fun out ->
     Lwt_unix.connect out (local_addr dest_port) >>= (fun () ->
     (* Start relaying *)
     Lwt.choose [relay inp out; relay out inp])))))
