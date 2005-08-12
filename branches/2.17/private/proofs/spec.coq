
Axiom File : Set.
Axiom Name : Set.
Axiom FProps : Set.
Axiom DProps : Set.

Inductive FS : Set :=
  F: File -> FProps -> FS
| D: (Name -> FS) -> DProps -> FS
| Bot: FS.

Inductive aok[X : Set] : X -> X -> X -> X -> X -> Set :=
  id : (o,a,b:X)(aok X o a b a b)
| rtl: (s, s' : X) (aok X s s s' s' s')
| ltr: (s, s' : X) (aok X s s' s s' s').

Axiom fprops : FS -> FProps.
Axiom dprops : FS -> DProps.
Axiom contents : FS -> File.
Axiom children : FS -> Name -> FS.

Axiom file_contents : (c : File)(p : FProps)((contents (F c p)) = c).

Inductive ok : FS -> FS -> FS -> FS -> FS -> Set :=
  atomic: (o,a,b,a',b':FS)(aok FS o a b a' b')->(ok o a b a' b')
| dir: (o : FS)(x, y, x', y' : Name -> FS)(p, q, p', q' : DProps)
       (aok DProps (dprops o) p q p' q') ->
       ((n : Name) (ok (children o n) (x n) (y n) (x' n) (y' n))) ->
       (ok o (D x p) (D y q) (D x' p') (D y' q'))
| file: (o : FS)(c,d,c',d' : File)(p, q, p', q' : FProps)
        (aok FProps (fprops o) p q p' q') ->
        (aok File (contents o) c d c' d') ->
        (ok o (F c p) (F d q) (F c' p') (F d' q')).

Inductive True_everywhere [P : FS -> FS -> FS -> FS -> FS -> Prop] :
    FS -> FS -> FS -> FS -> FS -> Prop :=
  true_everywhere :
    (o,a,b,a',b' : FS)
    (P o a b a' b') ->
    ((n : Name)
     (True_everywhere P (children o n) (children a n)
         (children b n) (children a' n) (children b' n))) ->
    (True_everywhere P o a b a' b').

Definition not_dir [a : FS] : Prop :=
  Cases a of
  | (D _ _) => False
  | _       => True
  end.

Definition user_change_preserved : FS -> FS -> FS -> FS -> FS -> Prop :=
 [o, a, b, a', b' : FS]
 (ok o a b a' b') ->
 (~(o = a) -> ((not_dir o) \/ (not_dir a)) -> (a' = a))
   /\
 (Cases o a of
    (D x q) (D x' p) =>
      ~(p = q) ->
      Cases a' of
        (D y p') => (p' = p)
      | _        => False
      end
  | _ _ =>
      True
  end).

Axiom te :
  (P : FS -> FS -> FS -> FS -> FS -> Prop)
  ((o,a,b,a',b' : FS)(P o a b a' b')) ->
  ((o,a,b,a',b' : FS)(True_everywhere P o a b a' b')).

Lemma ucp :
  (o,a,b,a',b': FS)(True_everywhere user_change_preserved o a b a' b'). 
  

       1) ok(o,a,b,a',b') and o(r)<>get(a(r)) and o(r),a(r)not both directories
          ==> 
          a'(r) = a(r)
       2) ok(o,a,b,a',b') and o(r)=D(x,q) and a(r)=D(x',p) and p<>q 
          ==> 
          a'(r) = D(y,p') and
          p' = p
