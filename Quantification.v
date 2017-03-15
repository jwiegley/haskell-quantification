Require Import Coq.Unicode.Utf8.

Set Universe Polymorphism.

(**************************************************************************)

Section LogicalQuantification.

Lemma forall_not_exists : (∀ x : Prop, x) ↔ (¬ ∃ x, ¬ x).
Proof.
  intuition; eauto.
  apply H.
  firstorder.
  contradiction (H x).
  intros.
  eapply H; eauto.
Qed.

Lemma not_forall_not_exists : (¬ ∀ x : Prop, x) ↔ (∃ x, ¬ x).
Proof.
  intuition; eauto.
  apply H0.
Qed.

Lemma exists_not_forall : (  ∃ x, x) ↔ (¬ ∀ x, ¬ x).
Proof. intuition; eauto. Qed.

Lemma not_exists_not_forall : (¬ ∃ x, x) ↔ (  ∀ x, ¬ x).
Proof. intuition; eauto. Qed.

End LogicalQuantification.

(**************************************************************************)

Section Quantification.

Variable r : Prop.

Notation "¬ x" := (x → r) (at level 75, right associativity).

Lemma fun_forall_not_exists : (∀ x : Prop, x) ↔ (¬ ∃ x, ¬ x).
Proof.
  intuition; eauto.
  apply H.
  firstorder.
  contradiction (H r).
  intros; assumption.
Qed.

Lemma fun_not_forall_not_exists : (¬ ∀ x : Prop, x) ↔ (∃ x, ¬ x).
Proof.
  intuition; eauto.
  apply H0.
Qed.

Lemma fun_exists_not_forall : (∃ x, x) ↔ (¬ ∀ x, ¬ x).
Proof. intuition; eauto. Qed.

Lemma fun_not_exists_not_forall : (¬ ∃ x, x) ↔ (  ∀ x, ¬ x).
Proof. intuition; eauto. Qed.

End Quantification.

(**************************************************************************)

Definition Ex := ∃ x, x.

Inductive Exists : Prop := Here : ∀ x : Prop, x → Exists.

Definition Exists' : Prop := ∀ r : Prop, (∀ x : Prop, x → r) → r.

(**************************************************************************)

Require Import Coq.Program.Basics.

Open Scope program_scope.

(* This encodes the fact that the result of [t] cannot depend on its argument,
   due to the rank-2 universal in [Exists']. *)
Axiom existence_free_theorem :
  forall (e : Exists') (r : Prop) (t : ∀ x : Prop, x → r),
    forall (T : Prop) (x : T), t T x = e r t.

Lemma existence_implied : Exists <-> Exists'.
Proof.
  unfold Exists'; split; intros.
    eapply H0; eauto.
  apply H; intros.
  econstructor; eauto.
Qed.

Definition phi (x : Exists) : Exists' :=
  match x with Here T t => fun _ k => k T t end.

Definition psi (x : Exists') : Exists := x Exists Here.

Lemma psi_phi : forall x : Exists, psi (phi x) = x.
Proof.
  unfold psi, phi; intros.
  destruct x; auto.
Qed.

Require Import FunctionalExtensionality.

Lemma phi_psi : forall x : Exists', phi (psi x) = x.
Proof.
  unfold Exists', phi, psi; intros.
  destruct (x Exists Here).
  extensionality r.
  extensionality t.
  apply existence_free_theorem.
Qed.

(**************************************************************************)

Definition isomorphic (X Y : Type) : Prop :=
  exists (f : X → Y) (g : Y → X), f ∘ g = id /\ g ∘ f = id.

Notation "X ≅ Y" := (isomorphic X Y) (at level 100).

Require Import Hask.Control.Monad.Cont.

(* a ≅ Identity a
     ≅ Yoneda Identity a
     ≅ Ran Identity Identity a
     ≅ forall r, (a → Identity r) → Identity r
     ≅ forall r, (a → r) → r
*)

Theorem cont_iso : forall a, a ≅ forall r, (a → r) → r.
Proof.
  intros.
  exists (fun x _ k => k x).
  exists (fun k => k _ id).
  split; trivial.
  extensionality x.
  extensionality t.
  extensionality f.
  apply Cont_parametricity.
Qed.
