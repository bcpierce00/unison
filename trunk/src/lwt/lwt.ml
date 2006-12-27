
(* Either a thread ['a t] has terminated, eithera successfully [Return of 'a] or
 *  unsuccessfully [Fail of exn], or it is sleeping
 *)
type 'a state =
    Return of 'a
  | Fail of exn
  | Sleep

(* A suspended thread is described by ['a t]
 * It could have several [waiters], which are thunk functions
 *)
type 'a t =
  { mutable state : 'a state;
    mutable waiters : (unit -> unit) list }

(* [make st] returns a thread of state [st] and no waiters *)
let make st = { state = st; waiters = [] }

(* add a thunk [f] to the waiting list of thread [t] *)
let add_waiter t f = t.waiters <- f :: t.waiters

(* restart a sleeping thread [t], run all its waiters
 * and running all the waiters, and make the terminating state [st]
 * [caller] is a string that describes the caller
 *)
let restart t st caller =
  assert (st <> Sleep);
  if t.state <> Sleep then invalid_arg caller;
  t.state <- st;
  List.iter (fun f -> f ()) t.waiters;
  t.waiters <- []

(*
 * pre-condition: [t.state] is Sleep (i.e., not terminated)
 * [connect t t'] connects the two processes when t' finishes up
 * connecting means: running all the waiters for [t']
 * and assigning the state of [t'] to [t]
 *)
let rec connect t t' =
  if t.state <> Sleep then invalid_arg "connect";
  if t'.state = Sleep then
    add_waiter t' (fun () -> connect t t')
  else begin
    t.state <- t'.state;
    begin match t.waiters with
      [f] ->
        t.waiters <- [];
        f ()
    | _ ->
        List.iter (fun f -> f ()) t.waiters;
        t.waiters <- []
    end
  end

(* similar to [connect t t']; does nothing instead of raising exception when
 * [t] is not asleep
 *)
let rec try_connect t t' =
  if t.state <> Sleep then
    ()
  else if t'.state = Sleep then
    add_waiter t' (fun () -> try_connect t t')
  else begin
    t.state <- t'.state;
    List.iter (fun f -> f ()) t.waiters;
    t.waiters <- []
  end

(* apply function, reifying explicit exceptions into the thread type
 * apply: ('a -(exn)-> 'b t) -> ('a -(n)-> 'b t)
 * semantically a natural transformation TE -> T, where T is the thread
 * monad, which is layered over exception monad E.
 *)
let apply f x = try f x with e -> make (Fail e)

(****)

let return v = make (Return v)
let fail e = make (Fail e)

let wait () = make Sleep
let wakeup t v = restart t (Return v) "wakeup"
let wakeup_exn t e = restart t (Fail e) "wakeup_exn"

let rec bind x f =
  match x.state with
    Return v ->
      f v
  | Fail e ->
      fail e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun () -> connect res (bind x (apply f)));
      res
let (>>=) = bind

let rec catch_rec x f =
  match x.state with
    Return v ->
      x
  | Fail e ->
      f e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun () -> connect res (catch_rec x (apply f)));
      res

let catch x f = catch_rec (apply x ()) f

let rec try_bind_rec x f g =
  match x.state with
    Return v ->
      f v
  | Fail e ->
      apply g e
  | Sleep ->
      let res = wait () in
      add_waiter x (fun () -> connect res (try_bind_rec x (apply f) g));
      res

let try_bind x f = try_bind_rec (apply x ()) f

let poll x =
  match x.state with
    Fail e   -> raise e
  | Return v -> Some v
  | Sleep -> None

let rec ignore_result x =
  match x.state with
    Return v ->
      ()
  | Fail e ->
      raise e
  | Sleep ->
      add_waiter x (fun () -> ignore_result x)

let rec nth_ready l n =
  match l with
    [] ->
      assert false
  | x :: rem ->
      if x.state = Sleep then
        nth_ready rem n
      else if n > 0 then
        nth_ready rem (n - 1)
      else
        x

let choose l =
  let ready = ref 0 in
  List.iter (fun x -> if x.state <> Sleep then incr ready) l;
  if !ready > 0 then
    nth_ready l (Random.int !ready)
  else
    let res = wait () in
    (* XXX We may leak memory here, if we repeatedly select the same event *)
    List.iter (fun x -> try_connect res x) l;
    res
