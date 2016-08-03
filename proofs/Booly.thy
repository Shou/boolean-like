
theory Booly
imports Main
begin


(* Structure equality function; comparing first layer of constructors. *)
fun
  strucEq :: "'a option \<Rightarrow> 'a option \<Rightarrow> bool"
where
  "strucEq (Some _) (Some _) = True"
| "strucEq None None = True"
| "strucEq _ _ = False"

fun
  andlike :: "'a option \<Rightarrow> 'a option \<Rightarrow> 'a option"
where
  "andlike None _ = None"
| "andlike _ None = None"
| "andlike (Some a) (Some _) = Some a"

lemma and_assoc:
  "andlike a (andlike b c) = andlike (andlike a b) c"
by (metis andlike.elims andlike.simps(2) andlike.simps(3) strucEq.elims(2) strucEq.elims(3))

lemma and_struc_commut:
  "strEq (andlike a b) (andlike b a)"
by (metis andlike.elims strucEq.simps(1) strucEq.simps(2))

fun
  orlike :: "'a option \<Rightarrow> 'a option \<Rightarrow> 'a option"
where
  "orlike None None = None"
| "orlike None a = a"
| "orlike a b = a"

lemma or_assoc:
  "orlike a (orlike b c) = orlike (orlike a b) c"
by (metis option.collapse orlike.simps(1) orlike.simps(2) orlike.simps(3))

lemma or_struc_commut:
  "strEq (orlike a b) (orlike b a)"
by (metis not_None_eq orlike.simps(2) orlike.simps(3) strucEq.elims(3))

lemma or_commut:
  "orlike a b = orlike b a"
sledgehammer
sorry

fun
  xorlike :: "'a option \<Rightarrow> 'a option \<Rightarrow> 'a option"
where
  "xorlike None None = None"
| "xorlike (Some _) (Some _) = None"
| "xorlike (Some a) _ = Some a"
| "xorlike _ (Some a) = Some a"

lemma xor_not_assoc:
  "\<exists>a b c. strEq (xorlike (xorlike a b) c) (xorlike a (xorlike b c))"
apply (rule exI[where x="Some _"])
apply (rule exI[where x="Some _"])
apply (rule exI[where x="Some _"])
apply auto
done

lemma xor_struc_assoc:
  "strEq (xorlike a (xorlike b c)) (xorlike (xorlike a b) c)"
by (metis (no_types, lifting) not_None_eq strEq.elims(3) xorlike.simps(1) xorlike.simps(2) xorlike.simps(3) xorlike.simps(4))

lemma xor_struc_commut:
  "strEq (xorlike a b) (xorlike b a)"
by (metis (full_types) option.collapse option.simps(3) strEq.elims(3) xorlike.simps(2) xorlike.simps(3) xorlike.simps(4))

end
