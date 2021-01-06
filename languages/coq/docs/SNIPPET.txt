Require Import Coq.Strings.String.

Open Scope string_scope.

Definition hello:string := "Hello, World!".

(* Unit test *)
Lemma HelloTest:
  hello = "Hello, World!" .
Proof.
  reflexivity.
Qed.