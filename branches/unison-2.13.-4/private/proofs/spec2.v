(* TODO:
   - get rid of the axiom extensionality

   - existence of a maximal triple for ok
     (for ok and for a "more synchronized" relation)
   - this triple is the one that synchronize the most the filesystems
*)

Implicit Arguments On.

Section synchronization.

Variable Kind : Set.
Hypothesis eq_dec_kind : (k,k':Kind){k = k'} + {~k = k'}.
Variable Name : Set.

Inductive FS : Set :=
    Node : Kind -> (Name -> FS) -> FS
  | Bot :  FS.

Axiom extensionality :
  (A, B : Set)(f, g : A -> B)((x : A)((f x) = (g x))) -> f = g.

Definition child : Kind -> FS -> Name -> FS :=
  [k;a;n]
  Cases a of
    (Node k' c) =>
       Cases (eq_dec_kind k k') of
         (left equality) => (c n)
       | (right _)       => Bot
       end
  | _ =>
       Bot
  end.

Definition sync_pred := FS -> FS -> FS -> FS -> FS -> FS -> Prop.

Inductive aok [X : Set] :  X -> X -> X -> X -> X -> X -> Prop :=
    id : (o,a,b : X) (aok o a b o a b)
  | rtl : (s, s' : X) (aok s s s' s' s' s')
  | ltr : (s, s' : X) (aok s s' s s' s' s')
  | sync : (o,a : X) (aok o a a a a a).

Inductive ok : sync_pred :=
    atomic : (o,a,b,o',a',b' : FS) (aok o a b o' a' b') -> (ok o a b o' a' b')
  | rec :
      (o : FS)
      (k : Kind)
      (ca, cb, co', ca', cb' : Name -> FS)
      [a = (Node k ca)]
      [b = (Node k cb)]
      [o' = (Node k co')]
      [a' = (Node k ca')]
      [b' = (Node k cb')]
      ((n : Name)
       (ok (child k o n) (child k a n) (child k b n)
           (child k o' n) (child k a' n) (child k b' n))) ->
      (ok o a b o' a' b').

(****)

Inductive Path : Set :=
    Here : Path
  | Child : Kind -> Name -> Path -> Path.

Fixpoint follows [p : Path] : FS -> FS :=
  [a : FS]
  Cases p of
    Here           => a
  | (Child k n p') => (child k (follows p' a) n)
  end.

Definition everywhere [P : sync_pred] : sync_pred :=
  [o,a,b,o',a',b']
  (p : Path)
  (P (follows p o) (follows p a) (follows p b)
     (follows p o') (follows p a') (follows p b')).

(****)

Lemma child_diff_kind :
  (k, k' : Kind) ~(k=k') -> (c : Name -> FS) (n : Name)
  (child k (Node k' c) n) = Bot.
Intros k k' E c n; Unfold child; Case (eq_dec_kind k k');
  [ Intro E'; Case (E E') | Trivial ].
Qed.

(****)

Lemma ok_children :
  (o,a,b,o',a',b':FS)(k : Kind)(n : Name)
  (ok o a b o' a' b') ->
  (ok (child k o n) (child k a n) (child k b n)
      (child k o' n) (child k a' n) (child k b' n)).
Intros o a b o' a' b' k n H; Induction H;
  [ Apply atomic; Induction H;
      [ Apply id | Apply rtl | Apply ltr | Apply sync ]
  | Case (eq_dec_kind k k0); Intro E;
      [ Rewrite E; Apply H
      | Repeat Rewrite (child_diff_kind E);
        Apply atomic; Apply sync ] ].
Qed.

Lemma ok_everywhere :
  (o,a,b,o',a',b' : FS) (ok o a b o' a' b') -> (everywhere ok o a b o' a' b').
Unfold everywhere; Intros o a b o' a' b' H; Induction p;
  [ Exact H
  | Intros k n p' H'; Simpl; Apply ok_children; Exact H' ].
Qed.

Lemma lift_everywhere :
  (P : sync_pred)
  ((o,o',a,a',b,b' : FS)(ok o o' a a' b b') -> (P o o' a a' b b')) ->
  (o,o',a,a',b,b' : FS)(ok o o' a a' b b') -> (everywhere P o o' a a' b b').
Intros P H o o' a a' b b' H'; Intro p; Apply H; Exact (ok_everywhere H' p).
Qed.

(****)

Inductive same_kind : FS -> FS -> Prop :=
    same_kind_node :
      (k : Kind) (ca, cb : Name -> FS) (same_kind (Node k ca) (Node k cb))
  | same_kind_bot :
      (same_kind Bot Bot).

Definition no_loss_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a a') -> (same_kind o a).

Definition propagate_changes_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a a') -> (same_kind b a').

Definition no_change_below_conflict_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a b) -> a' = a \/ a = o.

Definition atomicity_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a b) -> a' = a \/ a' = b.

(* It there is a change, the archive reflects the new status. *)
Definition archive_reflects_updates_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind o' a') -> (a = a').

(* If there is a conflict, the archive remains unchanged. *)
Definition archive_unchanged_on_conflict_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a' b') -> o = o'.

(* If this archive is modified, it agrees with the new state of the
   filesystem. *)
Definition archive_modified_from_update_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind o' a') -> o = o'.

(* The archive is updated atomically when an update is propagated. *)
Definition archive_atomic_update_prop : sync_pred :=
  [o,a,b,o',a',b'] ~(same_kind a b) -> (same_kind a' b') -> o' = a'.

(****)

Lemma same_kind_refl : (a : FS) (same_kind a a).
Intro a; Case a; Intros; [ Apply same_kind_node | Apply same_kind_bot ].
Qed.

Section local_properties.

Variable o, a, b, o', a', b' : FS.
Hypothesis H : (ok o a b o' a' b').

Lemma no_loss_local : (no_loss_prop o a b o' a' b').
Unfold no_loss_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
      [ Intros o'' a'' b'' E; Case E
      | Idtac
      | Intros s s' E; Case E
      | Intros s s' E; Case E ];
    Intros; Apply same_kind_refl
  | Clear H o a b o' a' b';
    Intros o k ca cb co' ca' cb' H' E; Case E;
    Apply same_kind_node ].
Qed.

Lemma propagate_changes_local : (propagate_changes_prop o a b o' a' b').
Unfold propagate_changes_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
      [ Intros o'' a'' b'' E; Case E
      | Idtac
      | Intros s s' E; Case E
      | Intros s s' E; Case E ];
    Intros; Apply same_kind_refl
  | Clear H o a b o' a' b';
    Intros o k ca cb co' ca' cb' H' E; Case E;
    Apply same_kind_node ].
Qed.
  
Lemma no_change_below_conflict_local :
  (no_change_below_conflict_prop o a b o' a' b').
Unfold no_change_below_conflict_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H; Auto
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

Lemma atomicity_local :
  (atomicity_prop o a b o' a' b').
Unfold atomicity_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H; Auto
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

Lemma archive_reflects_updates :
  (archive_reflects_updates_prop o a b o' a' b').
Unfold archive_reflects_updates_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
    Trivial Orelse (Intros s s' E; Case E; Apply same_kind_refl)
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

Lemma archive_unchanged_on_conflict :
  (archive_unchanged_on_conflict_prop o a b o' a' b').
Unfold archive_unchanged_on_conflict_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
    Trivial Orelse (Intros s s' E; Case E; Apply same_kind_refl)
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

Lemma archive_modified_from_update :
  (archive_modified_from_update_prop o a b o' a' b').
Unfold archive_modified_from_update_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
    Trivial Orelse (Intros s s' E; Case E; Apply same_kind_refl)
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

Lemma archive_atomic_update :
  (archive_atomic_update_prop o a b o' a' b').
Unfold archive_atomic_update_prop; Case H;
  [ Clear H o a b o' a' b'; Intros o a b o' a' b' H; Case H;
    Trivial Orelse (Intros o'' a'' b'' E E'; Case (E E'))
  | Clear H o a b o' a' b'; Intros o k ca cb co' ca' cb' H H' H'';
    Case H'; Apply same_kind_node ].
Qed.

End local_properties.

(****)

Section fs_inductions.

Lemma one_fs_ind :
  (P : FS -> Prop)
  ((a : FS)
   ((k : Kind) (n : Name)
    (P (child k a n))) ->
    (P a)) ->
  (P Bot) ->
  (a : FS)
  (P a).
Intros P H H' a; Apply FS_ind;
  [ Intros k c H''; Apply H;
    Simpl; Intros k' n; Case (eq_dec_kind k' k); Trivial
  | Trivial ].
Qed.

Lemma two_fs_ind :
  (P : FS -> FS -> Prop)
  ((a,b : FS)
   ((k : Kind) (n : Name)
    (P (child k a n) (child k b n))) ->
    (P a b)) ->
  (P Bot Bot) ->
  (a,b : FS)
  (P a b).
Intros P H H' a; Generalize H H'; Clear H' H; Pattern a; Apply one_fs_ind;
  [ Auto
  | Intros H H' b; Pattern b; Apply one_fs_ind; Auto ].
Qed.

Lemma three_fs_ind :
  (P : FS -> FS -> FS -> Prop)
  ((o,a,b : FS)
   ((k : Kind) (n : Name)
    (P (child k o n) (child k a n) (child k b n))) ->
    (P o a b)) ->
  (P Bot Bot Bot) ->
  (o,a,b : FS)
  (P o a b).
Intros P H H' o; Generalize H H'; Clear H' H; Pattern o; Apply one_fs_ind;
  [ Auto
  | Intros H H' a b; Pattern a b; Apply two_fs_ind; Auto ].
Qed.

(* XXX *)
Axiom three_fs_rec :
  (f : FS -> FS -> FS -> Set)
  ((o,a,b : FS)
   ((k : Kind) (n : Name)
    (f (child k o n) (child k a n) (child k b n))) ->
    (f o a b)) ->
  (f Bot Bot Bot) ->
  (o,a,b : FS)
  (f o a b).

Lemma six_fs_ind :
  (P : sync_pred)
  ((o,a,b,o',a',b' : FS)
   ((k : Kind) (n : Name)
    (P (child k o n) (child k a n) (child k b n)
       (child k o' n) (child k a' n) (child k b' n))) ->
    (P o a b o' a' b')) ->
  (P Bot Bot Bot Bot Bot Bot) ->
  (o,a,b,o',a',b' : FS)
  (P o a b o' a' b').
Intros P H H' o a b; Generalize H H'; Clear H' H;
Pattern o a b; Apply three_fs_ind;
  [ Auto
  | Intros H H' o' a' b'; Pattern o' a' b'; Apply three_fs_ind; Auto ].
Qed.

Lemma nine_fs_ind :
  (P : FS -> FS -> FS -> FS -> FS -> FS -> FS -> FS -> FS -> Prop)
  ((o,a,b,o',a',b',o'',a'',b'' : FS)
   ((k : Kind) (n : Name)
    (P (child k o n) (child k a n) (child k b n)
       (child k o' n) (child k a' n) (child k b' n)
       (child k o'' n) (child k a'' n) (child k b'' n))) ->
    (P o a b o' a' b' o'' a'' b'')) ->
  (P Bot Bot Bot Bot Bot Bot Bot Bot Bot) ->
  (o,a,b,o',a',b',o'',a'',b'' : FS)
  (P o a b o' a' b' o'' a'' b'').
Intros P H H' o a b o' a' b'; Generalize H H'; Clear H' H;
Pattern o a b o' a' b'; Apply six_fs_ind;
  [ Auto
  | Intros H H' o'' a'' b''; Pattern o'' a'' b''; Apply three_fs_ind; Auto ].
Qed.

End fs_inductions.

Section ok_properties.

Derive Inversion_clear ok_inversion
  with (o,a,b,o',a',b':FS)(ok o a b o' a' b') Sort Prop.

Derive Inversion_clear ok_inversion_2
  with (o,o':FS)(k : Kind)(ca, cb, ca', cb' : Name -> FS)
       (ok o (Node k ca) (Node k cb) o' (Node k ca') (Node k cb'))
  Sort Prop.

Derive Inversion_clear ok_inversion_3
  with (o',a',b':FS)(k : Kind)(co, ca, cb : Name -> FS)
       (ok (Node k co) (Node k ca) (Node k cb) o' a' b')
  Sort Prop.

Derive Inversion_clear aok_inversion
  with (o,a,b,o',a',b':FS)(aok o a b o' a' b') Sort Prop.

Derive Inversion_clear aok_inversion_3
  with (o',a',b':FS)(k : Kind)(co, ca, cb : Name -> FS)
       (aok (Node k co) (Node k ca) (Node k cb) o' a' b')
  Sort Prop.

Lemma ok_symm :
  (o,a,b,o',a',b' : FS)(ok o a b o' a' b') -> (ok o b a o' b' a').
Intros o a b o' a' b' H; Induction H;
  [ Apply atomic; Induction H;
      [ Apply id | Apply ltr | Apply rtl | Apply sync ]
  | Apply rec; Exact H0 ].
Qed.

Lemma ok_eq :
  (o,o',a',b' : FS)(ok o o o o' a' b') -> o' = o /\ a' = o /\ b' = o.
Intro o; Pattern o; Apply one_fs_ind;
  [ Clear o; Intros o H o' a' b' H';
    Generalize H H'; Clear H;
    Inversion_clear H';
      [ Inversion_clear H; Repeat Split; Trivial
      | Intro H'; Generalize [n : Name](H' ? ? ? ? ? (H n));
        Simpl; Case (eq_dec_kind k k);
          [ Intros E H''; 
            CutRewrite -> co' = ca;
              [ CutRewrite -> ca' = ca;
                  [ CutRewrite -> cb' = ca;
                      [ Auto
                      | Apply extensionality;
                        Intro n; Generalize (H'' n);
                        Intros (H1, (H2, H3));
                        Auto ]
                  | Apply extensionality;
                    Intro n; Generalize (H'' n);
                    Intros (H1, (H2, H3));
                    Auto ]
              | Apply extensionality;
                Intro n; Generalize (H'' n);
                Intros (H1, (H2, H3));
                Auto ]
          | Intro E; Case E; Trivial ] ]
  | Intros o' a' b' H'; Inversion_clear H';
    Inversion_clear H; Repeat Split; Trivial ].
Qed.

Theorem ok_refl :
  (o, a, b : FS) (ok o a b o a b).
Intros; Apply atomic; Apply id.
Qed.

Theorem ok_antisym :
  (o, a, b, o', a', b' : FS)
  (ok o a b o' a' b') ->
  (ok o' a' b' o a b) ->
  o = o' /\ a = a' /\ b = b'.
Intros o a b o' a' b'; Pattern o a b o' a' b';
Apply six_fs_ind;
  [ Clear o a b o' a' b'; Intros o a b o' a' b' H' H;
    Generalize H H'; Clear H'; Inversion H using ok_inversion; Clear H;
      [ Intro H; Inversion H using aok_inversion;
          [ Auto
          | Intros H1 H2 H3; Exact (ok_eq H3)
          | Intros H1 H2 H3; Exact (ok_eq H3)
          | Intros H1 H2 H3; Exact (ok_eq H3) ]
      | Intros k ca cb co' ca' cb' H1 H2 H3 H4;
        Generalize H1 H2 H3 H4; Clear H1 H2 H3;
        Inversion H4 using ok_inversion_2; Clear H4;
          [ Intro H; Inversion H using aok_inversion; Clear H;
              [ Auto
              | Intros H1 H2 H3 H4; Generalize (ok_eq H2);
                Intros (H5, (H6, H7)); Auto
              | Intros H1 H2 H3 H4; Generalize (ok_eq H2);
                Intros (H5, (H6, H7)); Auto
              | Intros H1 H2 H3 H4; Generalize (ok_eq H2);
                Intros (H5, (H6, H7)); Auto ]
          | Intros co H1 H2 H3 H4 H5;
            Generalize [n:Name](H4 k n (H2 n) (H1 n));
            Simpl; Case (eq_dec_kind k k);
              [ Intros E G;
                CutRewrite -> co = co';
                  [ CutRewrite -> ca = ca';
                      [ CutRewrite -> cb = cb';
                          [ Auto
                          | Apply extensionality;
                            Intro n; Generalize (G n);
                            Intros (G1, (G2, G3)); Auto ]
                      | Apply extensionality;
                        Intro n; Generalize (G n);
                        Intros (G1, (G2, G3)); Auto ]
                  | Apply extensionality;
                    Intro n; Generalize (G n);
                    Intros (G1, (G2, G3)); Auto ]
              | Intro E; Case E; Trivial ] ] ]
  | Auto ].
Qed.

Theorem ok_trans :
  (o, a, b, o', a', b', o'', a'', b'' : FS)
  (ok o a b o' a' b') ->
  (ok o' a' b' o'' a'' b'') ->
  (ok o a b o'' a'' b'').
Intros o a b o' a' b' o'' a'' b''; Pattern o a b o' a' b' o'' a'' b'';
Apply nine_fs_ind;
  [ Clear o a b o' a' b' o'' a'' b''; Intros o a b o' a' b' o'' a'' b'';
    Intros H H'; Generalize H H'; Clear H;
    Inversion H' using ok_inversion; Clear H';
      [ Intro H; Inversion H using aok_inversion; Clear H;
          [ Trivial
          | Intros H H' H''; Generalize (ok_eq H'');
            Intros (E1, (E2, E3)); Rewrite E1; Rewrite E2; Rewrite E3;
            Apply atomic; Apply rtl
          | Intros H H' H''; Generalize (ok_eq H'');
            Intros (E1, (E2, E3)); Rewrite E1; Rewrite E2; Rewrite E3;
            Apply atomic; Apply ltr
          | Intros H H' H''; Generalize (ok_eq H'');
            Intros (E1, (E2, E3)); Rewrite E1; Rewrite E2; Rewrite E3;
            Apply atomic; Apply sync ]
      | Intros k ca cb co' ca' cb' H1 H2 H3 H4;
        Generalize H1 H2 H3 H4; Clear H1 H2 H3;
        Inversion H4 using ok_inversion_3; Clear H4;
          [ Intro H; Inversion H using aok_inversion_3; Clear H;
              [ Trivial
              | Intros H1 H2 H3 H4; Apply rec; Intro n;
                Apply H2; [ Trivial | Apply atomic; Apply rtl ]
              | Intros H1 H2 H3 H4; Apply rec; Intro n;
                Apply H2; [ Trivial | Apply atomic; Apply ltr ]
              | Intros H1 H2 H3 H4; Apply rec; Intro n;
                Apply H2; [ Trivial | Apply atomic; Apply sync ] ]
          | Intros co'' ca'' cb'' H1 H2 H3 H4 H5;
            Apply rec; Intro n; Apply H3; Trivial ] ]
 | Trivial ].
Qed.

End ok_properties.

Section global_properties.

Variable o, a, b, o', a', b' : FS.
Hypothesis H : (ok o a b o' a' b').

(* User changes are never overwritten *)
Definition no_loss : (everywhere no_loss_prop o a b o' a' b') :=
  (lift_everywhere no_loss_local H).

(* Every change the synchronizer makes is the propagation of a user change *)
Definition propagate_changes :
   (everywhere propagate_changes_prop o a b o' a' b')
  := (lift_everywhere propagate_changes_local H).

(* Below conflict paths, the synchronizer changes nothing *)
Definition no_change_below_conflict :
  (everywhere no_change_below_conflict_prop o a b o' a' b')
  := (lift_everywhere no_change_below_conflict_local H).

(* Either there is a change or there is no change *)
Definition atomicity :
  (everywhere atomicity_prop o a b o' a' b')
  := (lift_everywhere atomicity_local H).

End global_properties.

Section completeness.

Definition swap [P : sync_pred] : sync_pred :=
  [o,a,b,o',a',b' : FS](P o b a o' b' a').

Lemma everywhere_children :
  (P : sync_pred) (o,a,b,o',a',b' : FS) (k : Kind) (n : Name)
  (everywhere P o a b o' a' b') ->
  (everywhere P (child k o n) (child k a n) (child k b n)
                (child k o' n) (child k a' n) (child k b' n)).
Intros P o a b o' a' b' k n H; Intro p;
Generalize P o a b o' a' b' k n H; Clear H P o a b o' a' b' k n;
Induction p;
  [ Intros P o a b o' a' b' k n H; Generalize (H (Child k n Here)); Trivial
  | Intros P o a b o' a' b' k' n' H; Simpl;
    Pattern (follows p (child k' o n')) (follows p (child k' a n'))
            (follows p (child k' b n')) (follows p (child k' o' n'))
            (follows p (child k' a' n')) (follows p (child k' b' n'));
    Apply Hrecp;
    Intro p'; Generalize (H (Child k n p')); Trivial ].
Qed.

Lemma same_kind_dec : (a, b : FS) {(same_kind a b)} + {~(same_kind a b)}.
Destruct a;
  [ Intros ka ca; Destruct b;
      [ Intros kb cb;
        Case (eq_dec_kind ka kb);
          [ Intro E; Rewrite E; Left; Apply same_kind_node
          | Intro E; Right; Intro E'; Inversion E'; Case (E H) ]
      | Right; Intro E; Inversion E ]
  | Destruct b;
      [ Intros kb cb; Right; Intro E; Inversion E
      | Left; Apply same_kind_bot ] ].
Qed.
      
Lemma same_kind_trans :
   (a, b, c : FS) (same_kind a b) -> (same_kind b c) -> (same_kind a c).
Intros a b c H; Inversion_clear H; Intro H; Inversion_clear H;
  [ Apply same_kind_node | Apply same_kind_bot ].
Qed.

Lemma same_kind_sym :
   (a, b : FS) (same_kind a b) -> (same_kind b a).
Intros a b H; Inversion_clear H;
  [ Apply same_kind_node | Apply same_kind_bot ].
Qed.

Theorem completeness_theorem :
  (o, a, b, o', a', b' : FS)
  (everywhere no_loss_prop o a b o' a' b') ->
  (everywhere propagate_changes_prop o a b o' a' b') ->
  (everywhere no_change_below_conflict_prop o a b o' a' b') ->
  (everywhere atomicity_prop o a b o' a' b') ->
  (everywhere (swap no_loss_prop) o a b o' a' b') ->
  (everywhere (swap propagate_changes_prop) o a b o' a' b') ->
  (everywhere (swap no_change_below_conflict_prop) o a b o' a' b') ->
  (everywhere (swap atomicity_prop) o a b o' a' b') ->
  (everywhere archive_reflects_updates_prop o a b o' a' b') ->
  (everywhere (swap archive_reflects_updates_prop) o a b o' a' b') ->
  (everywhere archive_unchanged_on_conflict_prop o a b o' a' b') ->
  (everywhere archive_modified_from_update_prop o a b o' a' b') ->
  (everywhere archive_atomic_update_prop o a b o' a' b') ->
  (ok o a b o' a' b').
Intros o a b o' a' b'; Pattern o a b o' a' b'; Apply six_fs_ind;
  [ Clear o a b o' a' b';
    Intros o a b o' a' b' H0 H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12 H13;
    Generalize
      (H1 Here) (H2 Here) (H3 Here) (H4 Here) (H5 Here) (H6 Here) (H7 Here)
      (H8 Here) (H9 Here) (H10 Here) (H11 Here) (H12 Here) (H13 Here);
    Simpl;
    Unfold swap no_loss_prop propagate_changes_prop;
    Unfold no_change_below_conflict_prop atomicity_prop;
    Unfold archive_reflects_updates_prop archive_unchanged_on_conflict_prop;
    Unfold archive_modified_from_update_prop archive_atomic_update_prop;
    Intros G1 G2 G3 G4 G5 G6 G7 G8 G9 G10 G11 G12 G13;
    Case (same_kind_dec a b);
      [ Intro E;
        Case (same_kind_dec a a');
          [ Case (same_kind_dec b b');
              [ Case (same_kind_dec o' a');
                  [ Case (same_kind_dec o' b');
                      [ Intros E1 E2 E3 E4; Generalize E1 E3 E E4 E2;
                        Clear E3 E E4 E2; Inversion E1;
                        Intro E2; Inversion E2;
                        Intro E3; Inversion E3;
                        Intro E4; Inversion E4;
                        Intro E5; Inversion E5;
                        Intro E6;
                          [ Apply rec;
                            Rewrite H; Rewrite H14;
                            Rewrite H23 in H19; Rewrite H19;
                            Rewrite H22;
                            Rewrite H20 in H15; Rewrite H15;
                            Intro n; Apply H0;
                            Apply everywhere_children; Assumption
                          | Apply atomic; Apply sync ]
                      | Intros E1 E2 E3 E4;
                        Case
                          (E1 (same_kind_trans
                                 (same_kind_trans E2 (same_kind_sym E4))
                                 (same_kind_trans E E3))) ]
                  | Case (same_kind_dec o' b');
                      [ Intros E1 E2 E3 E4;
                        Case
                          (E2 (same_kind_trans
                                 (same_kind_trans E1 (same_kind_sym E3))
                                 (same_kind_trans (same_kind_sym E) E4)))
                      | Intros E1 E2; Rewrite (G9 E2); Rewrite (G10 E1);
                        Rewrite (G12 E2); Intros; Apply atomic; Apply id ] ]
              | Intros E1 E2;
                Case (E1 (same_kind_trans (same_kind_sym E) (G6 E1))) ]
          | Intro E1; Case (E1 (same_kind_trans E (G2 E1))) ]
      | Intro E;
        Case (same_kind_dec b a); Intro E';
          [ Case (E (same_kind_sym E'))
          | Apply atomic;
            Generalize G3 G7 G11 G13;
            Case (G4 E); Intro E1; Rewrite E1;
            Case (G8 E'); Intro E2; Rewrite E2;
              [ Intros F1 F2 F3 F4; Rewrite (F3 E);
                Apply id
              | Intros F1 F2 F3 F4; Rewrite (F4 E (same_kind_refl a));
                Case (F2 E'); Intro E3; Rewrite E3; [ Apply sync | Apply ltr ]
              | Intros F1 F2 F3 F4; Rewrite (F4 E (same_kind_refl b));
                Case (F1 E); Intro E3; Rewrite E3; [ Apply sync | Apply rtl ]
              | Intros F1 F2 F3 F4; Rewrite (F3 E');
                Case (F1 E);
                  [ Intro E3; Rewrite E3; Apply id
                  | Intro E3; Case (F2 E');
                      [ Intro E4; Rewrite E4; Apply id
                      | Intro E4; Rewrite E3; Rewrite E4; Apply id ] ] ] ] ]
  | Intros; Apply atomic; Apply id ].
Qed.

End completeness.

Definition is_same_kind :=
  [a, b : FS]
  Cases a b of
    (Node ka _) (Node kb _) =>
      Cases (eq_dec_kind ka kb) of
        left  => true
      | right => false
      end
  | Bot Bot =>
      true
  | _ _ =>
      false
  end.

Lemma bool_dec : (x : bool){x = true} + {x = false}.

Lemma same_kind_true :
  (a,b : FS) ((is_same_kind a b) = true) -> (same_kind a b).
Destruct a;
  [ Intros ka ca; Destruct b;
      [ Intros kb cb;
        Simpl; Case (eq_dec_kind ka kb);
          [ Intros E E'; Rewrite E; Apply same_kind_node
          | Intros; Discriminate ]
      | Intros; Discriminate ]
  | Destruct b;
      [ Intros; Discriminate
      | Intros; Apply same_kind_bot ] ].
Qed.

Theorem maximal_element:
  (o,a,b : FS)
  { o' : FS &
  { a' : FS &
  { b' : FS |
   (o'', a'', b'' : FS)
   (ok o a b o'' a'' b'') -> (ok o'' a'' b'' o' a' b')}}}.
abcd.
Intros o a b; Pattern o a b; Apply three_fs_rec;
  [ Clear o a b; Intros o a b;
    Case (bool_dec (is_same_kind a b));
      [ Intro E; Inversion E;
          [ Intro G;

End synchronization.

(****)

Section filesystem.

Variable dprops : Set.
Variable fprops : Set.
Variable fcontents : Set.
Hypothesis eq_dec_dprops : (x,x':dprops){x = x'} + {~x = x'}.
Hypothesis eq_dec_fprops : (x,x':fprops){x = x'} + {~x = x'}.
Hypothesis eq_dec_fcontents : (x,x':fcontents){x = x'} + {~x = x'}.

Inductive Kind : Set :=
    Dir : Kind
  | DProps : dprops -> Kind
  | File : Kind
  | FProps : fprops -> Kind
  | FContents : fcontents -> Kind.

Require EqDecide.

Lemma eq_dec_kind : (k,k':Kind){k = k'} + {~k = k'}.
Decide Equality.
Qed.

Variable name : Set.

Inductive Name : Set :=
    property : Name
  | contents : Name
  | desc : name -> Name.

End filesystem.
