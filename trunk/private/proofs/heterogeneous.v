
Implicit Arguments On.

Section synchronization.

(**** Abstract kinds ****)

Variable KindO : Set.
Variable Name : Set.

Variable compatible : KindO -> KindO -> Prop.
Hypothesis compatible_trans :
  (k, k', k'' : KindO)
  (compatible k k') -> (compatible k' k'') -> (compatible k k'').
Hypothesis compatible_sym :
  (k, k' : KindO)(compatible k k') -> (compatible k' k).
Hypothesis compatible_refl :
  (k : KindO)(compatible k k).
Hypothesis compatible_dec :
  (k, k' : KindO){(compatible k k')}+{~(compatible k k')}.

(**** File system ****)

Section Fs.

Variable Kind : Set.
Variable get : Kind -> KindO.
Variable set : (k : Kind)(ko : KindO)(compatible ko (get k)) -> Kind.

Inductive FS : Set :=
    Node : Kind -> (Name -> FS) -> FS
  | Bot :  FS.

Definition child : KindO -> FS -> Name -> FS :=
  [k;a;n]
  Cases a of
    (Node k' c) =>
       Cases (compatible_dec k (get k')) of
         left  => (c n)
       | right => Bot
       end
  | _ =>
       Bot
  end.

End Fs.

(**** File system updates ****)

Section Fsget.

Variable Kind : Set.
Variable get : Kind -> KindO.
Variable set : (k : Kind)(ko : KindO)(compatible ko (get k)) -> Kind.
Variable create : KindO -> Kind.

Fixpoint fsget [a : (FS Kind)] : (FS KindO) :=
  Cases a of
     Bot       => (Bot KindO)
  | (Node k c) => (Node (get k) [n:Name](fsget (c n)))
  end.

Fixpoint fscreate [o : (FS KindO)] : (FS Kind) :=
  Cases o of
     Bot       => (Bot Kind)
  | (Node k c) => (Node (create k) [n:Name](fscreate (c n)))
  end.

Fixpoint fsset [a : (FS Kind); o : (FS KindO)] : (FS Kind) :=
  Cases a o of
     _            Bot         =>
       (Bot Kind)
  | Bot          _           =>
       (fscreate o)
  | (Node ka ca) (Node ko co) =>
       Cases (compatible_dec ko (get ka)) of
         (left compatible) =>
            (Node (!set ka ko compatible) [n:Name](fsset (ca n) (co n)))
       | (right _)         =>
            (fscreate o)
       end
  end.

End Fsget.

(**** Useful lemmas ****)

Section child_set_get.

Variable KindA : Set.
Variable KindB : Set.
Variable getA : KindA -> KindO.
Variable getB : KindB -> KindO.
Variable setA : (ka : KindA)(ko : KindO)(compatible ko (getA ka)) -> KindA.
Variable setB : (kb : KindB)(ko : KindO)(compatible ko (getB kb)) -> KindB.
Variable createA : KindO -> KindA.
Variable createB : KindO -> KindB.
Local childA := (child getA).
Local childB := (child getB).
Local fsgetA := (fsget getA).
Local fsgetB := (fsget getB).
Local fssetA := (fsset setA createA).
Local fssetB := (fsset setB createB).
Hypothesis get_set_A :
   (k : KindO) (ka : KindA) (H : (compatible k (getA ka)))
   (getA (!setA ka k H)) = k.
Hypothesis get_create_A : (k : KindO) (getA (createA k)) = k.
Local getO [x : KindO] := x.
Local childO := (child getO).

Lemma child_fsset :
  (a : (FS KindA)) (b : (FS KindB)) (k : KindO) (n : Name)
  (childA k (fssetA a (fsgetB b)) n) =
  (fssetA (childA k a n) (fsgetB (childB k b n))).
Destruct b;
  [ Intros kb cb; Case a;
      [ Intros ka ca k n; Simpl;
        Case (compatible_dec (getB kb) (getA ka));
          [ Simpl; Intro H; Rewrite (get_set_A H);
            Case (compatible_dec k (getA ka));
            Case (compatible_dec k (getB kb));
               [ Trivial
               | Intros H' H'';
                 Case (H' (compatible_trans H'' (compatible_sym H)))
               | Intros H' H'';
                 Case (H'' (compatible_trans H' H))
               | Auto ]
          | Simpl; Rewrite (get_create_A (getB kb));
            Case (compatible_dec k (getA ka));
            Case (compatible_dec k (getB kb));
              [ Intros H H' H'';
                Case (H'' (compatible_trans (compatible_sym H) H'))
              | Case (ca n); Auto
              | Case (fsgetB (cb n)); Auto
              | Auto ] ]
      | Simpl; Rewrite (get_create_A (getB kb));
        Intros k n; Case (compatible_dec k (getB kb));
          [ Case (fsgetB (cb n)); Auto | Trivial ] ]
  | Intros k n; Case (childA k a n); Case a; Auto ].
Qed.

Lemma child_fsget :
  (a : (FS KindA))(k : KindO)(n : Name)
  (childO k (fsgetA a) n) = (fsgetA (childA k a n)).
Destruct a; Simpl;
  [ Unfold getO; Intros ka ca k;
    Case (compatible_dec k (getA ka)); Trivial
  | Trivial ].
Qed.

End child_set_get.

(**** Concrete kinds ****)

Variable KindA : Set.
Variable KindB : Set.

Definition getO [x : KindO] := x.
Variable getA : KindA -> KindO.
Variable getB : KindB -> KindO.
Variable setA : (ka : KindA)(ko : KindO)(compatible ko (getA ka)) -> KindA.
Variable setB : (kb : KindB)(ko : KindO)(compatible ko (getB kb)) -> KindB.
Variable createA : KindO -> KindA.
Variable createB : KindO -> KindB.
Hypothesis get_set_A :
   (k : KindO) (ka : KindA) (H : (compatible k (getA ka)))
   (getA (!setA ka k H)) = k.
Hypothesis get_create_A : (k : KindO) (getA (createA k)) = k.
Hypothesis get_set_B :
   (k : KindO) (kb : KindB) (H : (compatible k (getB kb)))
   (getB (!setB kb k H)) = k.
Hypothesis get_create_B : (k : KindO) (getB (createB k)) = k.

Definition fsgetA := (fsget getA).
Definition fsgetB := (fsget getB).
Definition fssetA := (fsset setA createA).
Definition fssetB := (fsset setB createB).

Definition childO := (child getO).
Definition childA := (child getA).
Definition childB := (child getB).

(**** Synchronization ****)

Definition sync_pred :=
  (FS KindO)-> (FS KindA) -> (FS KindB) -> (FS KindA) -> (FS KindB) -> Prop.

Inductive aok :  sync_pred :=
    id :  (o : (FS KindO))(a : (FS KindA))(b : (FS KindB))
          (aok o a b a b)
  | rtl : (a : (FS KindA))(b : (FS KindB))
          (aok (fsgetA a) a b (fssetA a (fsgetB b)) b)
  | ltr : (a : (FS KindA))(b : (FS KindB))
          (aok (fsgetB b) a b a (fssetB b (fsgetA a))).

Inductive ok : sync_pred :=
    atomic : (o : (FS KindO))(a, a' : (FS KindA))(b, b' : (FS KindB))
             (aok o a b a' b') -> (ok o a b a' b')
  | rec :
      (o : (FS KindO))
      (k : KindO)
      (ka : KindA)
      (kb : KindB)
      (getA ka) = k ->
      (getB kb) = k ->
      (ca, ca' : Name -> (FS KindA))
      (cb, cb' : Name -> (FS KindB))
      [a = (Node ka ca)]
      [b = (Node kb cb)]
      [a' = (Node ka ca')]
      [b' = (Node kb cb')]
      ((n : Name)
       (ok (childO k o n) (childA k a n) (childB k b n)
           (childA k a' n) (childB k b' n))) ->
      (ok o a b a' b').

(**** Paths ****)

Section Paths.

Variable Kind : Set.
Variable get : Kind -> KindO.

Inductive Path : Set :=
    Here : Path
  | Child : KindO -> Name -> Path -> Path.

Fixpoint follows [p : Path] : (FS Kind) -> (FS Kind) :=
  [a]
  Cases p of
    Here           => a
  | (Child k n p') => (child get k (follows p' a) n)
  end.

End Paths.

Definition followsO := (follows getO).
Definition followsA := (follows getA).
Definition followsB := (follows getB).

(**** Lemma about propertied holding everywhere ****)

Definition everywhere [P : sync_pred] : sync_pred :=
  [o;a;b;a';b']
  (p : Path)
  (P (followsO p o) (followsA p a) (followsB p b)
     (followsA p a') (followsB p b')).

Lemma comp_child :
  (k, k' : KindO) (compatible k k') ->
  (o : (FS KindO)) (n : Name)
  (childO k o n) = (childO k' o n).
Intros k k' H o n; Case o;
  [ Simpl; Intros ko co;
    Case (compatible_dec k (getO ko));
    Case (compatible_dec k' (getO ko));
      [ Trivial
      | Intros H' H''; Case (H' (compatible_trans (compatible_sym H) H''))
      | Intros H' H''; Case (H'' (compatible_trans H H'))
      | Trivial ]
  | Trivial ].
Qed.

Lemma ok_children :
  (o : (FS KindO))(a, a' : (FS KindA)) (b, b' : (FS KindB))
  (k : KindO)(n : Name)
  (ok o a b a' b') ->
  (ok (childO k o n) (childA k a n) (childB k b n)
      (childA k a' n) (childB k b' n)).
Intros o a a' b b' k n H; Induction H;
  [ Apply atomic; Induction H;
      [ Apply id
      | Rewrite (child_fsget getA);
        Rewrite (child_fsset getB get_set_A get_create_A);
        Apply rtl
      | Rewrite (child_fsget getB);
        Rewrite (child_fsset getA get_set_B get_create_B);
        Apply ltr ]
  | Generalize H1; Clear H1; Simpl; Rewrite H; Rewrite H0;
    Case (compatible_dec k0 k0);
      [ Case (compatible_dec k k0);
          [ Intros H3 H4 H1; 
            Rewrite (comp_child H3);
            Apply H1
          | Intros; Apply atomic; Apply id ]
      | Intros E; Case (E (compatible_refl k0)) ] ].
Qed.

Lemma ok_everywhere :
  (o : (FS KindO)) (a, a' : (FS KindA)) (b,b' : (FS KindB))
  (ok o a b a' b') -> (everywhere ok o a b a' b').
Unfold everywhere; Intros o a a' b b' H; Induction p;
  [ Exact H
  | Intros k n p' H'; Simpl; Apply ok_children; Exact H' ].
Qed.

Lemma lift_everywhere :
  (P : sync_pred)
  ((o : (FS KindO)) (a, a' : (FS KindA)) (b,b' : (FS KindB))
   (ok o a b a' b') -> (P o a b a' b')) ->
  (o : (FS KindO)) (a, a' : (FS KindA)) (b,b' : (FS KindB))
  (ok o a b a' b') -> (everywhere P o a b a' b').
Intros P H o a a' b b' H'; Intro p; Apply H; Exact (ok_everywhere H' p).
Qed.

(**** The properties we want to check ****)

Definition same_kind [Kind : Set; a, b : (FS Kind)] :=
  Cases a b of
    (Node ka ca) (Node kb cb) => (ka = kb)
  | Bot          Bot          => True
  | _            _            => False
  end.

Definition no_loss_prop : sync_pred :=
  [o,a,b,a',b'] ~(same_kind o (fsgetA a)) -> (same_kind a' a).

Definition propagate_changes_prop : sync_pred :=
  [o,a,b,a',b'] ~(same_kind a a') -> (same_kind (fssetA a (fsgetB b)) a').

Definition no_change_below_conflict_prop : sync_pred :=
  [o,a,b,a',b']
  ~(o = (fsgetA a)) -> ~(same_kind (fsgetA a) (fsgetB b)) -> a = a'.

(**** Local properties ****)

Section local_properties.

Variable o : (FS KindO).
Variable a, a' : (FS KindA).
Variable b, b' : (FS KindB).
Hypothesis H : (ok o a b a' b').

Lemma no_loss_local : (no_loss_prop o a b a' b').
Unfold no_loss_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Clear H o a b; Intros o a b; Case a; Simpl; Trivial
      | Clear H a b; Intros a b E; Case E; Case (fsgetA a); Simpl; Trivial
      | Intro a''; Case a''; Simpl; Trivial ]
  | Simpl; Trivial ].
Qed.

Lemma propagate_changes_local : (propagate_changes_prop o a b a' b').
Unfold propagate_changes_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Intros o1 a1 b1 E; Case E; Case a1; Simpl; Trivial
      | Intros a'' b''; Case (fssetA a'' (fsgetB b'')); Simpl; Trivial
      | Intros a'' b'' E; Case E; Case a''; Simpl; Trivial ]
  | Intros o1 k1 ka kb Ea Eb ca ca' cb cb' H' E;
    Case E; Simpl; Trivial ].
Qed.
  
Lemma no_change_below_conflict_local :
  (no_change_below_conflict_prop o a b a' b').
Unfold no_change_below_conflict_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Trivial
      | Intros s s' E; Case E; Trivial
      | Trivial ]
  | Clear H o a b a' b'; Intros o k ka kb Ha Hb ca ca' cb cb' H H' H'';
    Case H''; Simpl; Rewrite Ha; Auto ].
Qed.

End local_properties.

(**** Local properties ****)

Section global_properties.

Variable o : (FS KindO).
Variable a, a' : (FS KindA).
Variable b, b' : (FS KindB).
Hypothesis H : (ok o a b a' b').

Definition no_loss : (everywhere no_loss_prop o a b a' b')
  := (lift_everywhere no_loss_local H).

Definition propagate_changes : (everywhere propagate_changes_prop o a b a' b')
  := (lift_everywhere propagate_changes_local H).

Definition no_change_below_conflict :
  (everywhere no_change_below_conflict_prop o a b a' b')
  := (lift_everywhere no_change_below_conflict_local H).

End global_properties.

End synchronization.

(****)

(* XXX
Lemma ok_symm :
  (o,a,b,a',b' : FS)(ok o a b a' b') -> (ok o b a b' a').
Intros o a b a' b' H; Induction H;
  [ Apply atomic; Induction H;
      [ Apply id | Apply ltr | Apply rtl ]
  | Apply rec; Exact H0 ].
Qed.
*)
