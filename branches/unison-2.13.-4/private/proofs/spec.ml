type ('a, 'p) sigS =
    ExistS of 'a * 'p


let sigS_rec f = function
  ExistS (x0, x) -> f x0 x


type sumbool =
    Left_ren
  | Right_ren


type 'a sumor =
    Inleft of 'a
  | Inright


type ('kind, 'name) fS =
    Node of 'kind * ('name -> ('kind, 'name) fS)
  | Bot


let one_fs_rec eq_dec_kind h h' a =
  let rec f1 = function
    Node (k, f3) ->
      h (Node (k, f3)) (fun k' n ->
        match eq_dec_kind k' k with
          Left_ren -> f1 (f3 n)
        | Right_ren -> h')
  | Bot -> h'
  in f1 a


let three_fs_rec eq_dec_kind h h' o =
  one_fs_rec eq_dec_kind (fun a h0 h1 h'0 a0 b ->
    h1 a a0 b (fun k n ->
      h0 k n h1 h'0
        (match a0 with
           Node (k', c) ->
             (match eq_dec_kind k k' with
                Left_ren -> c n
              | Right_ren -> Bot)
         | Bot -> Bot)
        (match b with
           Node (k', c) ->
             (match eq_dec_kind k k' with
                Left_ren -> c n
              | Right_ren -> Bot)
         | Bot -> Bot))) (fun h0 h'0 a ->
    one_fs_rec eq_dec_kind (fun a0 h1 h2 h'1 b ->
      h2 Bot a0 b (fun k n ->
        h1 k n h2 h'1
          (match b with
             Node (k', c) ->
               (match eq_dec_kind k k' with
                  Left_ren -> c n
                | Right_ren -> Bot)
           | Bot -> Bot))) (fun h1 h'1 b ->
      one_fs_rec eq_dec_kind (fun a0 h2 -> h1 Bot Bot a0 h2) h'1 b) a
      h0 h'0) o h h'


let maximal_element eq_dec_kind o a b =
  three_fs_rec eq_dec_kind (fun o0 a0 b0 ->
    match a0 with
      Node (ka, ca) ->
        (match b0 with
           Node (kb, cb) ->
             (match eq_dec_kind ka kb with
                Left_ren -> (fun h2 ->
                  ExistS ((Node (kb, (fun z ->
                                 match h2 kb z with
                                   ExistS (y1, a1) -> y1))),
                          (ExistS ((Node (kb, (fun z ->
                                          match sigS_rec (fun x y1 ->
                                                  y1) (h2 kb z) with
                                            ExistS (y1, a1) -> y1))),
                                   (Node (kb, (fun z ->
                                          sigS_rec (fun x y1 -> y1)
                                            (sigS_rec (fun x y1 -> y1)
                                              (h2 kb z)))))))))
              | Right_ren ->
                  (match failwith "eq_fs_dec" with
                     Left_ren -> (fun h ->
                       ExistS (b0, (ExistS (b0, b0))))
                   | Right_ren ->
                       (match failwith "eq_fs_dec" with
                          Left_ren -> (fun h ->
                            ExistS (a0, (ExistS (a0, a0))))
                        | Right_ren ->
                            (match failwith "eq_fs_dec" with
                               Left_ren -> (fun h ->
                                 ExistS (a0, (ExistS (a0, a0))))
                             | Right_ren -> (fun h ->
                                 ExistS (o0, (ExistS (a0, b0))))))))
         | Bot ->
             (match failwith "eq_fs_dec" with
                Left_ren -> (fun h -> ExistS (b0, (ExistS (b0, b0))))
              | Right_ren ->
                  (match failwith "eq_fs_dec" with
                     Left_ren -> (fun h ->
                       ExistS (a0, (ExistS (a0, a0))))
                   | Right_ren ->
                       (match failwith "eq_fs_dec" with
                          Left_ren -> (fun h ->
                            ExistS (a0, (ExistS (a0, a0))))
                        | Right_ren -> (fun h ->
                            ExistS (o0, (ExistS (a0, b0))))))))
    | Bot ->
        (match failwith "eq_fs_dec" with
           Left_ren -> (fun h -> ExistS (b0, (ExistS (b0, b0))))
         | Right_ren ->
             (match failwith "eq_fs_dec" with
                Left_ren -> (fun h -> ExistS (a0, (ExistS (a0, a0))))
              | Right_ren ->
                  (match failwith "eq_fs_dec" with
                     Left_ren -> (fun h ->
                       ExistS (a0, (ExistS (a0, a0))))
                   | Right_ren -> (fun h ->
                       ExistS (o0, (ExistS (a0, b0))))))))
    (ExistS (Bot, (ExistS (Bot, Bot)))) o a b


