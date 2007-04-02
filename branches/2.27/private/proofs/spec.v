(* TODO:
   - get rid of the axiom eq_fs_dec

   - new archive o'
   - abstract specification of the properties of the new archive
   - ok is an order relation on triples

   - existence of a maximal triple for ok
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

(* XXX *)
Axiom eq_fs_dec :
   (a, b : FS) { a = b } + { ~ a = b }.

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

Definition sync_pred := FS -> FS -> FS -> FS -> FS -> Prop.

Inductive aok [X : Set] :  X -> X -> X -> X -> X -> Prop :=
    id : (o,a,b : X) (aok o a b a b)
  | rtl : (s, s' : X) (aok s s s' s' s')
  | ltr : (s, s' : X) (aok s s' s s' s').

Inductive ok : sync_pred :=
    atomic : (o,a,b,a',b' : FS) (aok o a b a' b') -> (ok o a b a' b')
  | rec :
      (o : FS)
      (k : Kind)
      (ca, cb, ca', cb' : Name -> FS)
      [a = (Node k ca)]
      [b = (Node k cb)]
      [a' = (Node k ca')]
      [b' = (Node k cb')]
      ((n : Name)
       (ok (child k o n) (child k a n) (child k b n)
           (child k a' n) (child k b' n))) ->
      (ok o a b a' b').

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
  [o,a,b,a',b']
  (p : Path)
  (P (follows p o) (follows p a) (follows p b) (follows p a') (follows p b')).

(****)

Lemma child_diff_kind :
  (k, k' : Kind) ~(k=k') -> (c : Name -> FS) (n : Name)
  (child k (Node k' c) n) = Bot.
Intros k k' E c n; Unfold child; Case (eq_dec_kind k k');
  [ Intro E'; Case (E E') | Trivial ].
Qed.

(****)

Lemma ok_children :
  (o,a,b,a',b':FS)(k : Kind)(n : Name)
  (ok o a b a' b') ->
  (ok (child k o n) (child k a n) (child k b n)
      (child k a' n) (child k b' n)).
Intros o a b a' b' k n H; Induction H;
  [ Apply atomic; Induction H; [ Apply id | Apply rtl | Apply ltr ]
  | Case (eq_dec_kind k k0); Intro E;
      [ Rewrite E; Apply H
      | Repeat Rewrite (child_diff_kind E);
        Apply atomic; Apply id ] ].
Qed.

Lemma ok_everywhere :
  (o,a,b,a',b' : FS) (ok o a b a' b') -> (everywhere ok o a b a' b').
Unfold everywhere; Intros o a b a' b' H; Induction p;
  [ Exact H
  | Intros k n p' H'; Simpl; Apply ok_children; Exact H' ].
Qed.

Lemma lift_everywhere :
  (P : sync_pred)
  ((o,a,a',b,b' : FS)(ok o a a' b b') -> (P o a a' b b')) ->
  (o,a,a',b,b' : FS)(ok o a a' b b') -> (everywhere P o a a' b b').
Intros P H o a a' b b' H'; Intro p; Apply H; Exact (ok_everywhere H' p).
Qed.

(****)

Inductive same_kind : FS -> FS -> Prop :=
    same_kind_node :
      (k : Kind) (ca, cb : Name -> FS) (same_kind (Node k ca) (Node k cb))
  | same_kind_bot :
      (same_kind Bot Bot).

Definition no_loss_prop : sync_pred :=
  [o,a,b,a',b'] ~(same_kind o a) -> (same_kind a' a).

Definition propagate_changes_prop : sync_pred :=
  [o,a,b,a',b'] ~(same_kind a a') -> (same_kind b a').

Definition no_change_below_conflict_prop : sync_pred :=
  [o,a,b,a',b'] ~(o = a) -> ~(same_kind a b) -> a = a'.

Definition atomicity_prop : sync_pred :=
  [o,a,b,a',b'] ~(same_kind a b) -> a' = a \/ a' = b.

(****)

Lemma same_kind_refl : (a : FS) (same_kind a a).
Intro a; Case a; Intros; [ Apply same_kind_node | Apply same_kind_bot ].
Qed.

Section local_properties.

Variable o, a, b, a', b' : FS.
Hypothesis H : (ok o a b a' b').

Lemma no_loss_local : (no_loss_prop o a b a' b').
Unfold no_loss_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Idtac | Intros s s' E; Case E | Idtac ];
    Intros; Apply same_kind_refl
  | Intros; Apply same_kind_node ].
Qed.

Lemma propagate_changes_local : (propagate_changes_prop o a b a' b').
Unfold propagate_changes_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Intros o1 a1 b1 E; Case E | Idtac | Intros s s' E; Case E ];
    Intros; Apply same_kind_refl
  | Intros o1 k1 ca cb ca' cb' H' E; Case E; Apply same_kind_node ].
Qed.
  
Lemma no_change_below_conflict_local :
  (no_change_below_conflict_prop o a b a' b').
Unfold no_change_below_conflict_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Trivial
      | Intros s s' H''; Case H''; Trivial
      | Trivial ]
  | Clear H o a b a' b'; Intros o k ca cb ca' cb' H H' H'';
    Case H''; Apply same_kind_node ].
Qed.

Lemma atomicity_local :
  (atomicity_prop o a b a' b').
Unfold atomicity_prop; Case H;
  [ Clear H o a b a' b'; Intros o a b a' b' H; Case H;
      [ Left | Right | Left ];
    Trivial
  | Clear H o a b a' b'; Intros o k ca cb ca' cb' H H';
    Case H'; Apply same_kind_node ].
Qed.

End local_properties.

(****)

Lemma ok_symm :
  (o,a,b,a',b' : FS)(ok o a b a' b') -> (ok o b a b' a').
Intros o a b a' b' H; Induction H;
  [ Apply atomic; Induction H;
      [ Apply id | Apply ltr | Apply rtl ]
  | Apply rec; Exact H0 ].
Qed.

Section global_properties.

Variable o, a, b, a', b' : FS.
Hypothesis H : (ok o a b a' b').

(* User changes are never overwritten *)
Definition no_loss : (everywhere no_loss_prop o a b a' b') :=
  (lift_everywhere no_loss_local H).

(* Every change the synchronizer makes is the propagation of a user change *)
Definition propagate_changes : (everywhere propagate_changes_prop o a b a' b')
  := (lift_everywhere propagate_changes_local H).

(* Below conflict paths, the synchronizer changes nothing *)
Definition no_change_below_conflict :
  (everywhere no_change_below_conflict_prop o a b a' b')
  := (lift_everywhere no_change_below_conflict_local H).

(* Either there is a change or there is no change *)
Definition atomicity :
  (everywhere atomicity_prop o a b a' b')
  := (lift_everywhere atomicity_local H).

End global_properties.

Section completeness.

Definition swap [P : sync_pred] : sync_pred :=
  [o,a,b,a',b' : FS](P o b a b' a').

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
Intros P H H' a; Generalize H H'; Clear H' H; Induction a;
  [ Intros H1 H2 b; Apply H1;
    Intros k' n; Simpl; Case (eq_dec_kind k' k);
      [ Intro E; Apply H; Trivial
      | Intro E; Pattern (child k' b n); Apply one_fs_ind; Auto ]
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
Intros P H H' o; Generalize H H'; Clear H' H; Induction o;
  [ Intros H1 H2 a b; Apply H1;
    Intros k' n; Simpl; Case (eq_dec_kind k' k);
      [ Intro E; Apply H; Trivial
      | Intro E; Pattern (child k' a n) (child k' b n);
        Apply two_fs_ind; Auto ]
  | Intros H H' a b; Pattern a b; Apply two_fs_ind; Auto ].
Qed.

Lemma four_fs_ind :
  (P : FS -> FS -> FS -> FS -> Prop)
  ((a,b,a',b' : FS)
   ((k : Kind) (n : Name)
    (P (child k a n) (child k b n) (child k a' n) (child k b' n))) ->
    (P a b a' b')) ->
  (P Bot Bot Bot Bot) ->
  (a,b,a',b' : FS)
  (P a b a' b').
Intros P H H' a; Generalize H H'; Clear H' H; Induction a;
  [ Intros H1 H2 b a' b'; Apply H1;
    Intros k' n; Simpl; Case (eq_dec_kind k' k);
      [ Intro E; Apply H; Trivial
      | Intro E; Pattern (child k' b n) (child k' a' n) (child k' b' n);
        Apply three_fs_ind; Auto ]
  | Intros H H' b a' b'; Pattern b a' b'; Apply three_fs_ind; Auto ].
Qed.

Lemma five_fs_ind :
  (P : sync_pred)
  ((o,a,b,a',b' : FS)
   ((k : Kind) (n : Name)
    (P (child k o n) (child k a n) (child k b n)
       (child k a' n) (child k b' n))) ->
    (P o a b a' b')) ->
  (P Bot Bot Bot Bot Bot) ->
  (o,a,b,a',b' : FS)
  (P o a b a' b').
Intros P H H' o; Generalize H H'; Clear H' H; Induction o;
  [ Intros H1 H2 a b a' b'; Apply H1;
    Intros k' n; Simpl; Case (eq_dec_kind k' k);
      [ Intro E; Apply H; Trivial
      | Intro E;
        Pattern (child k' a n) (child k' b n) (child k' a' n) (child k' b' n);
        Apply four_fs_ind; Auto ]
  | Intros H H' a b a' b'; Pattern a b a' b'; Apply four_fs_ind; Auto ].
Qed.

Lemma everywhere_children :
  (P : sync_pred) (o,a,b,a',b' : FS) (k : Kind) (n : Name)
  (everywhere P o a b a' b') ->
  (everywhere P (child k o n) (child k a n) (child k b n)
                (child k a' n) (child k b' n)).
Intros P o a b a' b' k n H; Intro p;
Generalize P o a b a' b' k n H; Clear H P o a b a' b' k n;
Induction p;
  [ Intros P o a b a' b' k n H; Generalize (H (Child k n Here)); Trivial
  | Intros P o a b a' b' k' n' H; Simpl;
    Pattern (follows p (child k' o n')) (follows p (child k' a n'))
            (follows p (child k' b n')) (follows p (child k' a' n'))
            (follows p (child k' b' n'));
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
  (o, a, b, a', b' : FS)
  (everywhere no_loss_prop o a b a' b') ->
  (everywhere propagate_changes_prop o a b a' b') ->
  (everywhere no_change_below_conflict_prop o a b a' b') ->
  (everywhere atomicity_prop o a b a' b') ->
  (everywhere (swap no_loss_prop) o a b a' b') ->
  (everywhere (swap propagate_changes_prop) o a b a' b') ->
  (everywhere (swap no_change_below_conflict_prop) o a b a' b') ->
  (everywhere (swap atomicity_prop) o a b a' b') ->
  (ok o a b a' b').
Intros o a b a' b'; Pattern o a b a' b'; Apply five_fs_ind;
  [ Clear o a b a' b';
    Intros o a b a' b' H0 H1 H2 H3 H4 H5 H6 H7 H8;
    Generalize (H8 Here); Generalize (H7 Here);
    Generalize (H6 Here); Generalize (H5 Here);
    Generalize (H4 Here); Generalize (H3 Here);
    Generalize (H2 Here); Generalize (H1 Here);
    Simpl;
    Unfold swap no_loss_prop propagate_changes_prop;
    Unfold no_change_below_conflict_prop atomicity_prop;
    Intros G1 G2 G3 G4 G5 G6 G7 G8;
    Case (same_kind_dec a b);
      [ Intro E;
        Case (same_kind_dec a a');
          [ Case (same_kind_dec b b');
              [ Intro E1; Generalize E; Clear E; Inversion E1;
                Intro E2; Inversion E2; Intro E3; Inversion E3;
                  [ Apply rec; Rewrite H12 in H10;
                    Rewrite H10; Rewrite H; Rewrite H11; Rewrite H9;
                    Intro n; Apply H0; Apply everywhere_children; Assumption
                  | Apply atomic; Apply id ]
              | Intros E1 E2;
                Case (E1 (same_kind_trans (same_kind_sym E) (G6 E1))) ]
          | Intro E1; Case (E1 (same_kind_trans E (G2 E1))) ]
      | Intro E;
        Case (same_kind_dec b a); Intro E';
          [ Case (E (same_kind_sym E'))
          | Case (G4 E); Intro E1; Rewrite E1;
            Case (G8 E'); Intro E2; Rewrite E2;
              [ Apply atomic ; Apply id
              | Case (eq_fs_dec o b); Intro E3;
                  [ Rewrite E3; Apply atomic; Apply ltr
                  | Rewrite <- (G7 E3 E') in E2; Rewrite E2;
                    Apply atomic; Apply id ]
              | Case (eq_fs_dec o a); Intro E3;
                  [ Rewrite E3; Apply atomic; Apply rtl
                  | Rewrite <- (G3 E3 E) in E1; Rewrite E1;
                    Apply atomic; Apply id ]
              | Case (eq_fs_dec o a); Intro E3;
                  [ Case (eq_fs_dec o b); Intro E4;
                      [ Rewrite E4 in E3; Rewrite E3;
                        Apply atomic; Apply id
                      | Rewrite <- (G7 E4 E') in E2; Rewrite E2;
                        Apply atomic; Apply id ]
                  | Rewrite <- (G3 E3 E) in E1; Rewrite E1;
                    Apply atomic; Apply id ] ] ] ]
  | Intros; Apply atomic; Apply id ].
Qed.

End completeness.

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
